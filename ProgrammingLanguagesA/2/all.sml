(* Records *)
val record = {name = "Bob", id = 4444};
val id = #d record;
val name = #name record;

(* Datatype Bindings *)
datatype mytype = TwoInts of int * int
		| Str of string
                | Pizza
val a = Str "hi";
val b = Str;
val c = Pizza;
val d = TwoInts(1,3);
val e = a;

(* Case Expressions *)
fun f x =
    case x of
	Pizza => 3
      | Str s => 8
      | TowInts(i1,i2) => i1 + i2;

(* Type Synonyms *)
type my_type = mytype;
type my_record = { name: string, id: int };

fun sum_triple triple =
    case triple of
	(x, y, z) => x + y + z;

fun sum_triple2 =
    let val (x,y,z) = triple
    in x+y+z
    end

fun sum_triple3 (x,y,z) =
    x+y+z

exception Exception1;
exception Exception2 of int * int;
raise Exception1;
raise Exception2(1,2);

				  

		   
