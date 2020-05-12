-- test case: find variable type succ

signature SimpleStorage {
    storage tag : Bool;
    constructor c : void -> void;
    method set : (UInt, Bool) -> void;
}

-- implementation

constructor c (){
  storage
  returns void;
}

method set(x: UInt, y: Bool) {
	guard{
        x > 0;
    }
	storage{
        tag     |-> x;
    }
	effects{}
	returns void;
}

