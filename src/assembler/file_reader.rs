use std::path::PathBuf;

use super::diagnostic::Diagnostic;



pub trait FileReader {
    fn get_position(&self) -> u64;
    fn set_position(&mut self, pos: u64);
    fn reset_to_file(&mut self, file: &PathBuf) -> Result<(), Diagnostic>;
    fn read_char(&mut self) -> Result<char, FileReaderError>;
    fn peek_char(&mut self) -> Result<char, FileReaderError>;
    fn get_path(&self) -> Option<&PathBuf>;
}
#[derive(Debug)]
pub enum FileReaderError {
    ReachedEOF,
    DiagnosticError(Diagnostic)
}