/- test case -/

signature SimpleStorage {
    storage storedData : int;
    constructor c : UInt -> void;
    method set : (int, Bool) -> void;
}

/- implementation -/

constructor c (s : UInt){
  storage
    storedData              |-> s;
  returns void;
}

method set(x: int, y: Bool) {
	guard{}
	storage{
        storedData     |-> x;
        storedData     |-> y;
    }
	effects{}
	returns voidlit;
}