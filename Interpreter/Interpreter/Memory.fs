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
    let empty (memSize: int) = { memory = Map.empty; next = 0 }

    (*
        A function that given an amount of memory to allocate "size" and a memory "mem" returns a (memory * int) option
        It basically returns a newly allocated "memory" of amount "size" all set to 0 if "size" > 0
        - Returns Some(mem', next) 
            Where mem' is identical to "mem" but with all addresses from "next" to "next + size - 1" set to 0
            Where the "next" pointer is updated to next + size
        - Returns None otherwise if "size" is smaller than or equal to 0
    *)
    let alloc size mem = 
        if size <= 0 then None 
        else 
            let limit = mem.next + size - 1
            let rec initialise address limit map =
                match address with
                | address when address <= limit -> Map.add address 0 map |> initialise (address + 1) limit
                | _ -> map
            let allocatedMemory = initialise mem.next limit mem.memory
            Some({ memory = allocatedMemory; next = mem.next }, mem.next + size)
    
    (*
        A function which takes a "ptr" pointer int, a "size" int and a "mem" memory type and returns a memory option
        This function basically frees up memory from "mem" starting at the pointer position "ptr"
        - Returns Some (mem') where mem' is "mem", but all addresses from "ptr" to "ptr + size - 1" have been removed
            As long as all of these addresses are allocated in "mem"
        - Returns None otherwise, Note: the function does not decrease the "next" pointer
    *)
    let free ptr size mem = 
        let limit = ptr + size - 1
        let rec allExist addresses =
            match addresses with
            | [] -> true
            | address :: rest -> if Map.containsKey address mem.memory then allExist rest else false

        let addresses = [ptr .. limit]
        if allExist addresses then
            let updatedMemory = List.fold (fun m addr -> Map.remove addr m) mem.memory addresses
            Ok { mem with memory = updatedMemory }
        else None
    (*
        A function to set a value 'v' at the address "ptr" in the "mem" memory
        - returns a Some mem' option where mem' is the memory type with the value 'v' at the address "ptr"
        - retrns a None option if the address "ptr" does not exist
    *)
    let setMem ptr v mem = 
        if mem.memory.ContainsKey(ptr) then 
            Ok { memory = Map.add ptr v mem.memory; next = mem.next }
        else Error
       
    (*
        A function to get the value from the address "ptr" in the "mem" memory
        - returns a Some 'v' option  where 'v' is the value at the address "ptr" in the "mem" memory
        - retrns a None option if the address "ptr" is not allocated
    *)
    let getMem ptr mem = Map.tryFind ptr mem.memory

