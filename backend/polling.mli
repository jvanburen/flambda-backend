(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      Xavier Leroy and Damien Doligez, projet Cambium, INRIA Paris      *)
(*               Sadiq Jaffer, OCaml Labs Consultancy Ltd                 *)
(*          Stephen Dolan and Mark Shinwell, Jane Street Europe           *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2021 OCaml Labs Consultancy Ltd                            *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Analyses related to the insertion of [Ipoll] operations. *)

type unsafe_or_safe = Unsafe | Safe

module Unsafe_or_safe : sig
  type t = unsafe_or_safe

  val bot : t

  val join : t -> t -> t

  val lessequal : t -> t -> bool
end

val instrument_fundecl : future_funcnames:Misc.Stdlib.String.Set.t
    -> Mach.fundecl -> Mach.fundecl

val requires_prologue_poll : future_funcnames:Misc.Stdlib.String.Set.t
    -> fun_name:string -> Mach.instruction -> bool
