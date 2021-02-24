open OUnit2

module Hex = struct
  let hexa = "0123456789abcdef"
let of_char c =
  let x = Char.code c in
  hexa.[x lsr 4], hexa.[x land 0xf]

let to_char x y =
  let code c = match c with
    | '0'..'9' -> Char.code c - 48 (* Char.code '0' *)
    | 'A'..'F' -> Char.code c - 55 (* Char.code 'A' + 10 *)
    | 'a'..'f' -> Char.code c - 87 (* Char.code 'a' + 10 *)
    | _ -> let msg = Printf.sprintf "Hex.to_char: %d is an invalid char" (Char.code c) in failwith msg 
  in
  Char.chr (code x lsl 4 + code y)
end


let encode_nix_safe_path str =
	let encode ch =
	  let a,b = (Hex.of_char ch) in
          Printf.sprintf "+x%c%c" a b 
	in
	let open Str in
	full_split (regexp "[^.+_a-zA-Z0-9-]\\|\\+x") str
        |> List.map (function
	       | Delim x ->
                  let b = Buffer.create (String.length x) in
                  String.iter (fun c -> Buffer.add_string b (encode c)) x;
                  Buffer.contents b
		| Text x -> x
	) |> String.concat ""

let decode_nix_safe_path str =
	let open Str in
	let hex = "[0-9a-fA-F]" in
	full_split (regexp ("\\+x" ^ hex ^ hex)) str |> List.map (function
		| Delim x -> Hex.to_char x.[2] x.[3] |> String.make 1
		| Text x -> x
	) |> String.concat ""

let print_string x = x

let test_encode str expected =
	"encode " ^ str >:: (fun _ ->
		assert_equal ~printer:print_string expected (encode_nix_safe_path str)
	)

let test_decode str expected =
	"decode" >:: (fun _ ->
		assert_equal ~printer:print_string expected (decode_nix_safe_path str)
	)

let group_by f l =
  let h = Hashtbl.create 7 in
  List.iter
    (fun e -> let k = f e in match Hashtbl.find_opt h k with
                             | Some l -> Hashtbl.add h k (e :: l)
                             | None -> Hashtbl.add h k [e]) l;
  Hashtbl.fold (fun k v l -> (k, v) :: l) h []

let suite =
  "Util" >:::
    [
      "filename encoding" >::: [
	test_encode "hello there" "hello+x20there";
		test_encode "hello	there" "hello+x09there";
		test_encode "++x+x" "++x2b+x78+x2b+x78";
		test_encode "xxx" "xxx";
		test_encode "+++" "+++";

		test_decode "hello+x20there" "hello there";
		test_decode "hello+x09there" "hello	there";
		test_decode "++x2b+x78+x2b+x78" "++x+x";
	];

	"group_by" >:::
			let printer = fun groups -> (
				let inner = groups |> List.map (fun (b, nums) ->
					Printf.sprintf "(%b, [%s])" b (nums |> List.map string_of_int |> String.concat "; ")
				) in
				Printf.sprintf "[%s]" (String.concat "; " inner)
			) in
			let assert_equal = assert_equal ~printer in
			let group_by_zero = group_by (fun x -> x = 0) in
		[

		"single run" >:: (fun _ ->
			assert_equal [
				(false, [1;2;3])
			]
			(group_by_zero [1;2;3])
		);

		"multiple runs" >:: (fun _ ->
			assert_equal [
				(false, [1;2;3]);
				(true, [0]);
				(false, [4;5]);
				(true, [0;0]);
			]
			(group_by_zero [1;2;3;0;4;5;0;0])
		);
	];
]
