# Batpu VM
This is a vm for the batpu, with planned support for a debugger and custom programming language called Sassy.
## Sassy
Sassy is an acronym for **S**afe **ass**embl**y** and aims to make programming in assembly more predictable, reducing side effects, and result in a more managable codebase.
It does that by enforcing access rules for registers and removing labels, replacing them with clearer branching statements like `if` `else`, `loop`, `break` and `continue`. Labels can also not be called, which is what **func**tions are for.
### Access rules
These are rules checked at compile-time for ensuring that data in registries doesn't get 'corrupted', to reduce side effects.
It does that, by having each function explicitly declare which registers it will use, and how it will use them. These declarations are then enforced by the compiler.
Here an example of what might go wrong in assembly:
```as
.main
  ldi r1 "important data"
  cal .do_work
  str r1 r2 //r1 was (unknowingly to us) replaced in `do_work` with unintentional data (Not good)
  hlt

.do_work
  ldi r1 "temporary data"
  //do work...
  ldi r2 "some adr"
  ret
```
Damn, now we've written some data somewhere, which was not intended to be there. Even though this might not be a very 'real world' example, stuff like this might happen in a more complex codebase. Let's see the same code in Sassy, to see how it would prevent that mistake.
```sy
func main() {
  ldi r1 "important data"
  do_work(use r1, out r2) //You don't *have* to add input arguments `do_work()` would be semantically ok
  str r1 r2 //Error: r1 is uncertain and cannot be read.
}

 func do_work(use r1, out r2) { //Explicitly states, that do work is gonna *use* r1 as temporary data storage in this function, and return a result value in r2
  ldi r1 "temporary data"
  //do work...
  ldi r2 "some adr"
}
```
The compiler now threw an error when trying to read from `r1`, but how exactly is this enforced?

#### Certainty
Every register is either certain or uncertain. Certain register's values are meant to be intended by the programmer, while uncertain registers are considered to cause side-effects, which we want to mitigate. 
**The compiler basically only makes sure, that uncertain registers aren't read. If you write to a register, it is considered to be certain again in your current scope. *(This is very important)***
Functions take registers as parameters which they can add modifiers on. These modifiers dictate how certainty is for function and caller.
There are following modifiers:

  - *none*: Register is readonly inside the function and stays certain after the function call. *Use for constant function arguments.*
    ```sy
    func main() {
      ldi r1 1
      example_none(r1)
      //Here, r1 will still be 1, and is therefore certain
    }
    
    func example_none(r1) {
      adi r1 5 //Error: Can't write to r1 as it is read-only
      cmp r1 r0 //We can however read from r1
      if eq {
        hlt
      }
    }
    ```
  - mut: Register is certain for the function and caller after the function call. *Use for function arguments which change during the function, but which's changes are expected.*
    ```sy
    func main() {
      ldi r1 1
      example_mut(mut r1)
      //Here, the side effects of example_mut are intended. r1 is still certain and can be read
    }
    
    func example_mut(mut r1) {
      inc r1
    }
    ```
  - in: Register is certain for the function, but will turn uncertain for the caller. *Use for function arguments, which might change during the function.*
    ```sy
    func main() {
      ldi r1 1
      example_in(in r1)
      //Here, the side effects of example_in are not intended to be read by the caller. r1 is still no longer certain
      adi r1 1 //Error: r1 is not certain and cannot be read.
    }
    
    func example_in(in r1) {
      cmp r1 r0
      if eq {
        ldi r1 42
        str r1 r0
      }
    }
    ```
  - out: Register is uncertain for the function, but will be certain for the caller. *Use for function return values.*
    ```sy
    func main() {
      example_out(out r1)
      //Here, r1 is an out value and is meant to be read.
    }
    
    func example_out(out r1) {
      inc r1 //Error: r1 is not certain and cannot be read.
      ldi r1 42
      lod r1 r1
    }
    ```
  - use: Register is uncertain for the function and for its caller. *Use for temporary values inside the function.*
    ```sy
    func main() {
      example_use(use r1)
      //Here, r1 is an out value and is meant to be read.
    }
    
    func example_use(use r1) {
      ldi r1 42
      lod r1 r1
      cmp r1 r0
      
    }
    ```


