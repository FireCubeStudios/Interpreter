module Interpreter.Memory
    (*
        This signature file contains signatures for Memory.fs which manages program memory
    *)
    open Language

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
        A function that given an amount of memory to allocate "size" and a memory "mem" returns a (memory * int) result
        It basically returns a newly allocated "memory" of amount "size" all set to 0 if "size" > 0
        - Returns Ok(mem', next) 
            Where mem' is identical to "mem" but with all addresses from "next" to "next + size - 1" set to 0
            Where the "mem.next" pointer is updated to next + size
        - Returns "Result.Error" of type:
            NegativeMemoryAllocated (size) if "size" is smaller than or equal to 0
            OutOfMemory if there is not "size" memory available in "mem" - TODO
    *)
    val alloc : int -> memory -> Result<(memory * int), error>
    
    (*
        A function which takes a "ptr" pointer int, a "size" int and a "mem" memory type and returns a memory result
        This function basically frees up memory from "mem" starting at the pointer position "ptr"
        - Returns Ok (mem') where mem' is "mem", but all addresses from "ptr" to "ptr + size - 1" have been removed
            As long as all of these addresses are allocated in "mem"
        - Returns "Result.Error" of type MemoryNotAllocated (ptr') otherwise, 
            if any memory in "mem" between "ptr" and "ptr + size - 1" is not allocated
            where ptr' is the smallest address greater than or equal to "ptr" that is not allocated.
            Note: the function does not decrease the "next" pointer
    *)
    val free : int -> int -> memory -> Result<memory, error>

    (*
        A function to set a value 'v' at the address "ptr" in the "mem" memory
        - returns a Ok mem' result where mem' is the memory type with the value 'v' at the address "ptr"
        - returns a "Result.Error" of type MemoryNotAllocated(ptr) if the address "ptr" does not exist
    *)
    val setMem : int -> int -> memory -> Result<memory, error>
       
    (*
        A function to get the value from the address "ptr" in the "mem" memory
        - returns a Ok 'v' result where 'v' is the value at the address "ptr" in the "mem" memory
        - returns a "Result.Error" of type MemoryNotAllocated(ptr) if the address "ptr" is not allocated
    *)
    val getMem : int -> memory -> Result<int, error>


