/- A simple storage program -/

signature SimpleStorage {
    storage storedData : int;

	constructor c : (void) -> void;
	method set : (int) -> void;
}

constructor c (){
	storage
	returns void;
}

method set(x: int) {
	guard{
		/-x > 0 ;-/
	}
	storage{
    	storedData |-> x;
	}
	effects{}
	returns voidlit;
}
