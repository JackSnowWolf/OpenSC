-- test case: find variable fail

signature SimpleStorage {
    storage storedData : UInt;
    constructor c : UInt -> void;
}

-- implementation

constructor c (s : UInt){
  storage
    supply                |-> s;
  returns void;
}
