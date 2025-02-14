(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Heterogenous_list

type outcome =
  | Accept
  | Skip

module Make (Iterator : Leapfrog.Iterator) = struct
  type 's stack =
    | Stack_nil : nil stack
    | Stack_cons :
        'a Iterator.t
        * 'a option Named_ref.t
        * ('a -> 's) continuation
        * 's stack
        -> ('a -> 's) stack

  and 's continuation = 's stack -> unit

  type ('a, 's) instruction =
    | Advance : ('a, 's) instruction
    | Up : ('x, 's) instruction -> ('x, 'a -> 's) instruction
    | Dispatch : ('a, 'b -> 's) instruction
    | Seek :
        'b option Named_ref.t * 'b Iterator.t * ('a, 's) instruction
        -> ('a, 's) instruction
    | Open :
        'b Iterator.t
        * 'b option Named_ref.t
        * ('a, 'b -> 's) instruction
        * ('a, 'b -> 's) instruction
        -> ('a, 's) instruction
    | Action : 'a * ('a, 's) instruction -> ('a, 's) instruction
    | Call :
        ('b Constant.hlist -> unit)
        * 'b Option_ref.hlist
        * ('a, 's) instruction
        * string
        -> ('a, 's) instruction

  let pp_instruction pp_act ff instr =
    let pp_initiator ppf depth =
      if depth > 0 then Format.pp_print_space ppf ()
    in
    let pp_terminator ppf = Format.fprintf ppf "@]" in
    let rec pp_terminators ppf depth =
      if depth > 0
      then (
        pp_terminator ppf;
        pp_terminators ppf (depth - 1))
    in
    let rec pp_instruction : type s. _ -> (_, s) instruction * int -> unit =
     fun ff (instr, depth) ->
      match instr with
      | Advance ->
        (* Default terminator *)
        pp_terminators ff depth
      | Up instr ->
        let rec print_breaks : type s. _ -> (_, s) instruction -> unit =
         fun n instr ->
          match instr with
          | Up instr -> print_breaks (n + 1) instr
          | Advance | Open _ | Seek _ | Dispatch | Action _ | Call _ ->
            Format.fprintf ff "%a" pp_initiator depth;
            if n > 1
            then Format.fprintf ff "break %d" n
            else Format.fprintf ff "break";
            Format.fprintf ff "%t%a" pp_terminator pp_instruction
              (instr, depth - n)
        in
        print_breaks 1 instr
      | Open (iterator, var, instr1, Dispatch) ->
        Format.fprintf ff "%a@[<v 2>@[<hov 2>for %a in %a:@]%a" pp_initiator
          depth Named_ref.pp_name var Iterator.print_name iterator
          pp_instruction
          (instr1, depth + 1)
      | Seek (var, iterator, instr) ->
        Format.fprintf ff "%a@[<v 2>@[<hov 2>for _ in {%a} ⨝ %a:@]%a"
          pp_initiator depth Named_ref.pp_name var Iterator.print_name iterator
          pp_instruction
          (instr, depth + 1)
      | Dispatch ->
        Format.fprintf ff "%adispatch%a" pp_initiator depth pp_terminators depth
      | Open (iterator, var, instr1, instr2) ->
        Format.fprintf ff
          "%a@[<v 2>@[<hov 2>open (%a : %a) [@;<1 0>%a@;<1 -2>]@] {%a"
          pp_initiator depth Named_ref.pp_name var Iterator.print_name iterator
          pp_instruction (instr2, 0) pp_instruction
          (instr1, depth + 1)
      | Action (a, instr) ->
        Format.fprintf ff "%a@[<v 2>%a@]%a" pp_initiator depth pp_act a
          pp_instruction (instr, depth)
      | Call (_f, l, instr, name) ->
        Format.fprintf ff "%a%s (%a)%a" pp_initiator depth name
          Option_ref.pp_name_hlist l pp_instruction (instr, depth)
    in
    pp_instruction ff (instr, 0)

  let[@inline always] dispatch ~advance (stack : (_ -> _) stack) =
    let (Stack_cons (iterator, cell, level, next_stack)) = stack in
    match Iterator.current iterator with
    | Some current_key ->
      Iterator.accept iterator;
      cell.contents <- Some current_key;
      level stack
    | None -> advance next_stack

  let[@loop] rec advance : type s. s continuation =
   fun stack ->
    match stack with
    | Stack_nil -> ()
    | Stack_cons (iterator, _, _, _) as stack ->
      Iterator.advance iterator;
      dispatch ~advance stack

  type t = nil continuation

  let[@inline] execute (type a) ~(evaluate : a -> outcome) instruction =
    let rec execute : type s. (a, s) instruction -> s continuation =
     fun instruction stack ->
      match instruction with
      | Advance -> advance stack
      | Up k ->
        let (Stack_cons (_, _, _, stack)) = stack in
        execute k stack
      | Open (iterator, cell, for_each, k) ->
        Iterator.init iterator;
        execute k (Stack_cons (iterator, cell, execute for_each, stack))
      | Seek (key_ref, iterator, k) -> (
        let key = Option.get (Named_ref.( ! ) key_ref) in
        Iterator.init iterator;
        Iterator.seek iterator key;
        match Iterator.current iterator with
        | Some current_key when Iterator.equal_key iterator current_key key ->
          Iterator.accept iterator;
          execute k stack
        | None | Some _ -> advance stack)
      | Dispatch -> dispatch ~advance stack
      | Action (op, k) -> (
        match (evaluate [@inlined hint]) op with
        | Accept -> execute k stack
        | Skip -> advance stack)
      | Call (f, rs, k, _name) ->
        f (Option_ref.get rs);
        execute k stack
    in
    execute instruction

  let create ~evaluate (instruction : (_, _) instruction) =
    execute ~evaluate instruction

  let run continuation = continuation Stack_nil

  let advance = Advance

  let up i = Up i

  let dispatch = Dispatch

  let seek r it k = Seek (r, it, k)

  let open_ i cell a dispatch = Open (i, cell, a, dispatch)

  let action a k = Action (a, k)

  let call f ~name y k = Call (f, y, k, name)

  let rec refs : type s. s Iterator.hlist -> s Option_ref.hlist = function
    | [] -> []
    | _ :: iterators ->
      { contents = None; printed_name = "_" } :: refs iterators

  type erev = Erev : 's Iterator.hlist * 's Option_ref.hlist -> erev

  let iterate :
      type a.
      (a Constant.hlist -> unit) -> a Iterator.hlist -> (_, nil) instruction =
   fun f iterators ->
    let rec rev0 :
        type s. s Iterator.hlist -> s Option_ref.hlist -> erev -> erev =
     fun iterators refs acc ->
      match iterators, refs with
      | [], [] -> acc
      | iterator :: iterators, r :: refs ->
        let (Erev (rev_iterators, rev_refs)) = acc in
        rev0 iterators refs (Erev (iterator :: rev_iterators, r :: rev_refs))
    in
    let rs = refs iterators in
    let (Erev (rev_iterators, rev_refs)) = rev0 iterators rs (Erev ([], [])) in
    let rec loop :
        type s a.
        (a -> s) Iterator.hlist ->
        (a -> s) Option_ref.hlist ->
        (_, a -> s) instruction ->
        (_, nil) instruction =
     fun iterators refs instruction ->
      match iterators, refs with
      | [iterator], [r] -> open_ iterator r instruction dispatch
      | iterator :: (_ :: _ as iterators), r :: refs ->
        loop iterators refs (open_ iterator r instruction dispatch)
    in
    match rev_iterators with
    | [] -> Advance
    | _ :: _ -> loop rev_iterators rev_refs (call f ~name:"yield" rs advance)

  type 'a iterator =
    | Iterator of ('a Constant.hlist -> unit) ref * nil continuation

  type void = |

  let iterator iterators =
    let evaluate : void -> outcome = function _ -> . in
    let f_ref = ref ignore in
    let f args = !f_ref args in
    Iterator (f_ref, execute ~evaluate (iterate f iterators))

  let[@inline] iter f (Iterator (f_ref, continuation)) =
    f_ref := f;
    continuation Stack_nil;
    f_ref := ignore
end
