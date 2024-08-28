use std::{fs::File, io::{self, Read, Write}, path::PathBuf};
use serde::{Serialize, Deserialize};

use super::code_location::{CodeLocation, Section};


#[derive(Serialize, Deserialize, Debug)]
pub struct SourceMappings {
    files: Vec<PathBuf>,
    mappings: Vec<Map>,
}

impl SourceMappings {

    pub fn new() -> Self {
        Self {
            files: vec![],
            mappings: vec![]
        }
    }

    pub fn push_mapping(&mut self, code_location: &CodeLocation, asm_start: usize, asm_end: usize) {
        let section = 
            if let Some(s) = &code_location.section {
                s
            } else {
                &Section {
                    location_begin: 0,
                    location_end: 0,
                }
            };
        
        let file_idx = self.files.iter().position(|p| p == &code_location.path);
        let file_idx =
            if let Some(idx) = file_idx {
                idx
            } else {
                self.files.push(code_location.path.to_owned());
                self.files.len() - 1
            };
        
        let map = Map {
            asm_start: asm_start as u64,
            asm_length: (asm_end - asm_start) as u16,
            origin_file_idx: file_idx as u16,
            source_start: section.location_begin,
            source_length:(section.location_end - section.location_begin) as u16
        };
        self.mappings.push(map);
    } 

    pub fn serialize(&self, path: &str) -> io::Result<()> {
        let encoded: Vec<u8> = bincode::serialize(self).unwrap();
        let mut file = File::create(path)?;
        file.write_all(&encoded)?;
        Ok(())
    }

    pub fn deserialize(path: &str) -> io::Result<Self> {
        let mut file = File::open(path)?;
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)?;
        let decoded: SourceMappings = bincode::deserialize(&buffer).unwrap();
        Ok(decoded)
    }


    pub fn find_map(&self, instruction_pointer: u64) -> Option<&Map> {
        //mappings is sorted

        let mut binary_search_begin: usize = 0;
        let mut binary_search_end = self.mappings.len();
        loop {
            let binary_search_pos = binary_search_begin + (binary_search_end - binary_search_begin) / 2;
            let searching_map = &self.mappings[binary_search_pos];
            if instruction_pointer < searching_map.asm_start { //Search lower half of map
                if binary_search_pos == 0 { // Prevent underflow
                    break;
                }
                binary_search_end = binary_search_pos - 1;
            } else if searching_map.asm_start + (searching_map.asm_length as u64) < instruction_pointer  {
                binary_search_begin = binary_search_pos + 1;
            } else {
                return Some(searching_map);
            }

            //Safety check
            let binary_search_size = binary_search_end.checked_sub(binary_search_begin)?;
            if binary_search_size < 1 {
                break;
            }
        }
        None
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Map {
    origin_file_idx: u16,
    asm_start: u64,
    asm_length: u16,

    source_start: u64,
    source_length: u16
}