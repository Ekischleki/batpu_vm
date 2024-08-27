use std::vec::IntoIter;

#[derive(Debug)]
pub struct TypeStream<T> {
    tokens: IntoIter<T>,
    next: Option<T>
}


impl<T> Iterator for TypeStream<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next.is_some() {
            Some(self.next())
        } else {
            None
        }
    }
}

impl<T> TypeStream<T> {

    pub fn new(tokens: Vec<T>) -> Self{
        let mut res = Self {
            tokens: tokens.into_iter(),
            next: None,
        };
        res.next = res.tokens.next();
        return res;
    }

    pub fn next(&mut self) -> T {
        
        match self.next.take() {
            Some(t) => {
                self.next = self.tokens.next();
                return t;
            }
            None => {
                panic!("Oh no, the typed stream is empty qwq\nSeems like SOMEBODY hasn't regarded the EOF token...")
            }
        }
    }
    ///Performs an operation with the upcoming item as input. This can be used as a replacement for `.peek()`
   pub fn extract<U>(&self, extractor: fn (&T) -> U) -> U {
        extractor(&self.next.as_ref().expect("Typed stream was empty."))
   }

    pub fn skip(&mut self) {
        _ = self.next();
    }
}