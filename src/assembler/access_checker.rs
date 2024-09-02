
use enum_as_inner::EnumAsInner;

use super::{code_location::CodeLocation, compilation::Compilation, diagnostic::{Diagnostic, DiagnosticPipelineLocation, DiagnosticType}, symbol_table::{Function, Operation, SymbolTable}, syntax::{Arg, Node}, token::ParamModifier};

pub fn check_access(compilation: &mut Compilation, symbol_table: &SymbolTable, function: &Function) {
    let mut register_manager = RegisterManager::new(function.node.as_func().unwrap().2);

    for body_code in &function.body_code { //TODO! Branching and loops and jumps and all
        let ops = body_code.get_op(symbol_table);
        register_manager.do_ops(&ops, &body_code.get_node().get_blame_location(), compilation);
    }

    

}

pub fn step_until(compilation: &mut Compilation, syntax: &Vec<Node>) {

}


fn setup_funcs(symbol_table: &SymbolTable) {

} 


struct FuncAccessability<'a> {
    func: &'a Function,
    start_reg_status: [RegisterStatus; 16]
}

#[derive(Clone, Copy)]
struct RegisterTracker {
    pub status: RegisterStatus,
    pub constraint: Constraint,
}
struct RegisterManager {
    register_status: [RegisterTracker; 16]
}

impl RegisterManager {
    pub fn new(args: &Vec<Arg>) -> Self {
        let mut register_status = [RegisterTracker {status: RegisterStatus::Uncertain, constraint: Constraint::Unavailable}; 16];

        for arg in args {
            let arg_idx = arg.register;
            match &arg.modifier {
                None => {
                    register_status[arg_idx as usize].constraint = Constraint::Readonly;
                    register_status[arg_idx as usize].status = RegisterStatus::Certain;
                }
                Some(modifier) => {
                    match modifier.token_type().as_param_modifier().unwrap() {
                        ParamModifier::In => {
                            (register_status[arg_idx as usize].status, register_status[arg_idx as usize].constraint) = (RegisterStatus::Certain, Constraint::Writable);
                        }
                        ParamModifier::Mut => {
                            (register_status[arg_idx as usize].status, register_status[arg_idx as usize].constraint) = (RegisterStatus::Certain, Constraint::Writable);
                        }
                        ParamModifier::Out => {
                            (register_status[arg_idx as usize].status, register_status[arg_idx as usize].constraint) = (RegisterStatus::Uncertain, Constraint::Writable);
                        }
                        ParamModifier::Use => {
                            (register_status[arg_idx as usize].status, register_status[arg_idx as usize].constraint) = (RegisterStatus::Uncertain, Constraint::Writable);
                        }
                    }
                }
            }
        }

        Self { register_status }
    }

    pub fn do_ops(&mut self, ops: &Vec<Operation>, blame_location: &CodeLocation, compilation: &mut Compilation) {
        for op in ops {
            let register = op.get_used_reg();
            if register == 0 { //We do not care about rules for r0, as it is constant
                continue;
            }
            let register_tracker = &mut self.register_status[register as usize];
            if register_tracker.constraint.is_unavailable() {
                compilation.add_diagnostic(Diagnostic::new(
                    DiagnosticType::Error, 
                    format!("Register r{} cannot be read, as it is not available in the current scope. Consider adding it as a parameter.", register), 
                    Some(blame_location.to_owned()), 
                    DiagnosticPipelineLocation::Access));
                    return;
            }
            match op {
                Operation::ReadReg(_) => {
                    if !register_tracker.status.is_certain() {
                        compilation.add_diagnostic(Diagnostic::new(
                            DiagnosticType::Error, 
                            format!("Register r{} cannot be read, as it is not certain. Consider writing to it.", register), 
                            Some(blame_location.to_owned()), 
                            DiagnosticPipelineLocation::Access))
                    }

                }
                Operation::WriteReg(_) => {
                    if register_tracker.constraint.is_readonly() {
                        compilation.add_diagnostic(Diagnostic::new(
                            DiagnosticType::Error, 
                            format!("Register r{} cannot be written to, as it is readonly. Consider making it writable using a mut, in or out modifier.", register), 
                            Some(blame_location.to_owned()), 
                            DiagnosticPipelineLocation::Access))
                    }
                    register_tracker.status = RegisterStatus::Certain;
                }
                Operation::Uncertify(_) => {
                    register_tracker.status = RegisterStatus::Uncertain;
                }
            }
        }
    }
}

#[derive(Clone, Copy, EnumAsInner)]
enum RegisterStatus {
    Uncertain, //Cannot be read, only written, to change its state to known
    Certain, //Can be read and written to
}
#[derive(Clone, Copy, EnumAsInner)]
enum Constraint {
    Readonly,
    Writable,
    Unavailable,
}

enum StatusReason<'a> {
    Declaration(&'a Arg),
    FuncCall(&'a Node, &'a Arg),
    //An instruction can't make the status uncertain
}

impl StatusReason<'_> {
    pub fn add_visualisation(&self, diagnostic: Diagnostic) -> Diagnostic {
        match self {
            StatusReason::Declaration(arg) => {
                if let Some(modifier) = &arg.modifier {
                    let description = match modifier.token_type().as_param_modifier().unwrap() {
                        ParamModifier::Out => "Out parameters are uncertain before written to.",
                        ParamModifier::Use => "Use parameters are uncertain before written to.",
                        _ => "<Internal fallback/error> Modifier declared here" //This should not cause an error, failsafe
                    };
                    diagnostic.with_visualisation(modifier.code_location().to_owned(), description.to_owned())
                }
                else {
                    diagnostic.with_visualisation(arg.register_token.code_location().to_owned(), "<Internal fallback/error> Parameter implicitly declared as readonly".to_owned())
                }
            },
            StatusReason::FuncCall(node, _) => {
                diagnostic.with_visualisation(node.get_blame_location(), "This function uncertifies the register. Consider backing it up or declaring it as an out, mut or readonly parameter.".to_owned())
            },
        }
    }
}

