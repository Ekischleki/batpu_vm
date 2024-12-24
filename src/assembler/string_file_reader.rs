use std::{fs::File, io::Read, path::PathBuf};

use super::{code_location::CodeLocation, diagnostic::{Diagnostic, DiagnosticPipelineLocation, DiagnosticType}, file_reader::{FileReader, FileReaderError}};


pub struct StringFileReader {
    position: u64,
    file: Vec<char>,
    file_path: PathBuf,
}

impl StringFileReader {
    pub fn new() -> Self {
        Self { position: 0, file: vec![], file_path: PathBuf::new()}
    }
}

impl FileReader for StringFileReader {
    fn get_position(&self) -> u64 {
        self.position
    }

    fn set_position(&mut self, pos: u64) {
        self.position = pos;
    }

    fn reset_to_file(&mut self, path: &PathBuf) -> Result<(), Diagnostic> {
        self.position = 0;
        

        let mut file;
        match File::open(path) {
            Ok(f) => file = f,
            Err(e) => {
                return Err(Diagnostic::new(
                    DiagnosticType::Error, 
                    format!("Unable to open file '{:?}'.\nInternal error: {}", *path, e.to_string()), 
                    None, 
                    DiagnosticPipelineLocation::IO));
            }
        }
        
        let file_size_result = get_file_size(&file, &path);
        let mut file_string;
        match file_size_result {
            Err(d) => {
                return Err(d);
            }
            Ok(s) => {
                file_string = String::with_capacity(s);
            }
        }

        
        match file.read_to_string(&mut file_string) {
            Err(e) => {
                return Err(Diagnostic::new(
                    DiagnosticType::Error, 
                    format!("Couldn't read file\nInternal reason: {}\nPlease make sure it is encoded in valid UTF-8", e.to_string()), 
                    Some(CodeLocation::new(path.to_owned())),
                    DiagnosticPipelineLocation::IO
                ));
            }
            _ => {}
        }
        self.file = file_string.chars().collect();
        self.file_path = path.to_owned();
        return Ok(());
    }

    fn read_char(&mut self) -> Result<char, FileReaderError> {
        let res = self.peek_char();
        self.position += 1;
        return res;
    }
    fn peek_char(&mut self) -> Result<char, FileReaderError> {
        if self.position >= self.file.len().try_into().expect("usize should fit into a u64, right?!") {
            return Err(FileReaderError::ReachedEOF);
        } else {
            let index: usize;
            match self.position.try_into() {
                Ok(i) => {index = i},
                Err(_) => {
                    return Err(FileReaderError::DiagnosticError(
                        Diagnostic::new(
                            DiagnosticType::Error,
                            "File too large to be read".to_owned(),
                            Some(CodeLocation::new(self.file_path.to_owned())),
                            DiagnosticPipelineLocation::IO)
                    ));
                }
            }

            return Ok(self.file[index]);
        }    
    }
    
    fn get_path(&self) -> Option<&PathBuf> {
        Some(&self.file_path)
    }
}

impl StringFileReader {
    
}


pub fn get_file_size(file: &File, path: &PathBuf) -> Result<usize, Diagnostic> {

    let file_length_long: u64;

    let metadata = file.metadata();

    match metadata {
        Ok(m) => {
            file_length_long = m.len();
        }
        Err(_) => {
            return Err(Diagnostic::new(
                DiagnosticType::Error,
                "Couldn't get file metadata.".to_owned(), 
                Some(CodeLocation::new(path.to_owned())),
                DiagnosticPipelineLocation::IO
            ));
        }
    }
    match file_length_long.try_into() {
        Ok(l) => {
            return Ok(l);
        }
        Err(_) => {
            return Err(Diagnostic::new(
                DiagnosticType::Error,
                "File is too large for this system to handle. Try reducing the file size, or switching to a 64 bit system.".to_owned(),
                Some(CodeLocation::new(path.to_owned())),
                DiagnosticPipelineLocation::IO
            ));
        }
    }

    
}