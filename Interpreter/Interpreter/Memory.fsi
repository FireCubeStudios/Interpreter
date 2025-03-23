module Interpreter.Memory
    (*
        This signature file contains signatures for Memory.fs which manages program memory
    *)
    
    (*
        A "memory" type record which contains a Map from integers memory adresses) to integers (memory values)
        Also contains an integer "next" which is a pointer to the next available free memory address
        The "memory" map has memory addresses -> values 
        Whenever we allocate memory the "next" pointer is increased (for simplicity it will never decrease)
    *)
    type memory = { memory: Map<int, int>; next: int }

    (*
        Creates a blank memory representation with the pointer "next" set to 0
        NOTE: memSize argument unused right now
    *)
    val empty : int -> memory

    (*
        A function that given an amount of memory to allocate "size" and a memory "mem" returns a (memory * int) option
        It basically returns a newly allocated "memory" of amount "size" all set to 0 if "size" > 0
        - Returns Some(mem', next) 
            Where mem' is identical to "mem" but with all addresses from "next" to "next + size - 1" set to 0
            Where the "next" pointer is updated to next + size
        - Returns None otherwise if "size" is smaller than or equal to 0
    *)
    val alloc : int -> memory -> (memory * int) option
    
    (*
        A function which takes a "ptr" pointer int, a "size" int and a "mem" memory type and returns a memory option
        This function basically frees up memory from "mem" starting at the pointer position "ptr"
        - Returns Some (mem') where mem' is "mem", but all addresses from "ptr" to "ptr + size - 1" have been removed
            As long as all of these addresses are allocated in "mem"
        - Returns None otherwise, Note: the function does not decrease the "next" pointer
    *)
    val free : int -> int -> memory -> memory option

    (*
        A function to set a value 'v' at the address "ptr" in the "mem" memory
        - returns a Some mem' option where mem' is the memory type with the value 'v' at the address "ptr"
        - retrns a None option if the address "ptr" does not exist
    *)
    val setMem : int -> int -> memory -> memory option
       
    (*
        A function to get the value from the address "ptr" in the "mem" memory
        - returns a Some 'v' option  where 'v' is the value at the address "ptr" in the "mem" memory
        - retrns a None option if the address "ptr" is not allocated
    *)
    val getMem : int -> memory -> int option


