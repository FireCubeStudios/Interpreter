module Interpreter.Memory
    (*
        This module contains a model for memory

        The following piece of memory stores the values 2, 3, and 42 at addresses 0, 5, and 9, respectively
         0 1 2 3 4 5 6 7 8  9 10 ...
        -------------------------
        |2| | | | |3| | | |42|  | ...
        -------------------------
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
    let empty memSize = { memory = Map.empty; next = 0 }

    (*
        A function which that given an amount of memory to allocate "size" and a memory "mem" returns a (memory * int) option
        It basically returns a newly allocated "memory" of amount "size" all set to 0 if "size" > 0
        - Returns Some(mem', next) 
            Where mem' is identical to "mem" but with all addresses from "next" to "next + size - 1" set to 0
            Where the "next" pointer is updated to next + size
        - Returns None otherwise if "size" is smaller than or equal to 0
    *)
    let alloc size mem = 
        if size <= 0 then None 
        else 
            let allocatedMemory = 
                List.fold (fun map address -> Map.add address 0 map) mem.memory [mem.next .. mem.next + size - 1] 
            Some({ memory = allocatedMemory; next = mem.next }, mem.next + size)
    

    let free _ = failwith "not implemented"
        

    let setMem _ = failwith "not implemented"
       
       
    let getMem _ = failwith "not implemented"

