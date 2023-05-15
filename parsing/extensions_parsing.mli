(** This module handles the logic around the syntax of our extensions to OCaml
    for [ocaml-jst], keeping the gory details wrapped up behind a clean
    interface.

    As we've started to work on syntactic extensions to OCaml, three concerns
    arose about the mechanics of how we wanted to maintain these changes in our
    fork.

    1. We don't want to extend the AST for our fork, as we really want to make
       sure things like ppxen are cross-compatible between upstream and
       [ocaml-jst].  Thankfully, OCaml already provides places to add extra
       syntax: extension points and annotations!  Thus, we have to come up with
       a way of representing our new syntactic constructs in terms of extension
       points (or annotations, but we went with the former).

    2. We don't want to actually match on extension points whose names are
       specific strings all over the compiler; that's incredibly messy, and it's
       easy to miss cases, etc.

    3. We want to keep different language extensions distinct so that we can add
       them to upstream independently, work on them separately, and so on.

    We have come up with a design that addresses those concerns by providing
    both a nice compiler-level interface for working with our syntactic
    extensions as first-class AST nodes, as well as a uniform scheme for
    translating this to and from OCaml AST values containing extension points.
    One wrinkle is that OCaml has many ASTs, one for each syntactic category
    (expressions, patterns, etc.); we have to provide this facility for each
    syntactic category where we want to provide extensions.

    a. For each language extension, we will define a module (e.g.,
       [Comprehensions]), in which we define a proper AST type per syntactic
       category we care about (e.g., [Comprehensions.expression] and its
       subcomponents).  This addresses concern (3); we've now contained each
       extension in a module.  But just that would leave them too siloed, so…

    b. We define an *overall auxiliary AST* for each syntactic category that's
       just for our language extensions; for expressions, it's called
       [Extensions.Expression.t].  It contains one constructor for each of the
       AST types defined as described in design point (1).  This addresses
       concern (2); we can now match on actual OCaml constructors, as long as we
       can get a hold of them.  And to do that…

    c. We define a general scheme for how we represent language extensions in
       terms of the existing ASTs, and provide a few primitives for
       consuming/creating AST nodes of this form, for each syntactic category.
       There's not a lot of abstraction to be done, or at least it's not (yet)
       apparent what abstraction there is to do, so most of this remains manual.
       (Setting up a full lens-based/otherwise bidirectional approach sounds
       like a great opportunity for yak-shaving, but not *actually* a good
       idea.)  This solves concern (3), and by doing it uniformly helps us
       address multiple cases at one stroke.

    Then, for each syntactic category, we define a module (in extensions.ml)
    that contains functions for converting between the Parsetree representation
    and the extension representation. A little functor magic (see [Make_of_ast])
    then allows us to make nice functions for export.

    This module contains the logic for moving to and from OCaml ASTs; the gory
    details of the encoding are detailed in the implementation.  All the actual
    ASTs should live in [Extensions], which is the only module that should
    directly depend on this one.

    When using this module, we often want to specify what our syntax extensions
    look like when desugared into OCaml ASTs, so that we can validate the
    translation code.  We generally specify this as a BNF grammar, but we don't
    want to depend on the specific details of the desugaring.  Thus, instead of
    writing out extension points or attributes directly, we write the result of
    [Some_ast.make_extension ~loc [name1; name2; ...; NameN] a] as the special
    syntax [{% 'name1.name2.....nameN' | a %}] in the BNF.  Other pieces of the
    OCaml AST are used as normal.

    One detail which we hide as much as possible is locations: whenever
    constructing an OCaml AST node -- whether with [wrap_desc], the functions in
    [Ast_helper], or some other way -- the location should be left to be
    defaulted (and the default, [!Ast_helper.make_default], should be ghost).
    The [make_entire_extension] function will handle making sure this default
    location is set appropriately.  If this isn't done and any locations on
    subterms aren't marked as ghost, the compiler will work fine, but ppxlib may
    detect that you've violated its well-formedness constraints and fail to
    parse the resulting AST. *)

(** An AST-style representation of the names used when generating extension
    nodes for modular extensions.  We use this to abstract over the details of
    how they're encoded, so we have some flexibility in changing them (although
    comments may refer to the specific encoding choices).  This is also why we
    don't expose any functions for rendering or parsing these names; that's all
    handled internally. *)
module Extension_node_name : sig
  (** A modular extension's extension node's name broken down into its
      components: the extension name plus any subparts.  This is a nonempty list
      corresponding to the dot-separated components of the name, less
      [extension.]. *)
  type t = ( :: ) of string * string list

  (** Print out a modular extension extension node name, in quotes; for use in
      error messages. *)
  val pp_quoted_name : Format.formatter -> t -> unit
end

(** The type of modules that lift and lower language extension terms from and
    to an OCaml AST type ([ast]) *)
module type AST = sig
  (** The AST type (e.g., [Parsetree.expression]) *)
  type ast

  (** The "AST description" type, without the location and attributes (e.g.,
      [Parsetree.expression_desc]) *)
  type ast_desc

  (** The name for this syntactic category in the plural form; used for error
      messages (e.g., "expressions") *)
  val plural : string

  (** How to get the location attached to an AST node *)
  val location : ast -> Location.t

  (** Turn an [ast_desc] into an [ast] by adding the appropriate metadata.  When
      creating [ast] nodes afresh for an extension, the location should be
      omitted; in this case, it will default to [!Ast_helper.default_loc], which
      should be [ghost]. *)
  val wrap_desc :
    ?loc:Location.t -> attrs:Parsetree.attributes -> ast_desc -> ast

  (** Embed a language extension term in the AST with the given name and body
      (the [ast]).  The name will be joined with dots and preceded by
      [extension.].  Any locations in the generated AST will be set to
      [!Ast_helper.default_loc], which should be [ghost].  Partial inverse of
      [match_extension]. *)
  val make_extension  : Extension_node_name.t -> ast -> ast_desc

  (** As [make_extension], but specifically for the AST node corresponding to
      the entire piece of extension syntax (e.g., for a list comprehension, the
      whole [[x for x in xs]], and not a subterm like [for x in xs]).  This sets
      [Ast_helper.default_loc] locally to the [ghost] version of the provided
      location, which is why the [ast] is generated from a function call; it is
      during this call that the location is so set. *)
  val make_entire_extension :
    loc:Location.t -> string -> (unit -> ast) -> ast_desc

  (** Given an AST node, check if it's a language extension term; if it is,
      split it back up into its name and the body; the resulting name is split
      on dots and the leading [extension] component is dropped.  If the language
      extension term is malformed in any way, raises an error; if the input
      isn't a language extension term, returns [None].  Partial inverse of
      [make_extension]. *)
  val match_extension : ast -> (Extension_node_name.t * ast) option
end

(** One [AST] module per syntactic category we currently care about; we're
    adding these lazily as we need them. When you add another one, make
    sure also to add special handling in [Ast_iterator] and [Ast_mapper]. *)

module Expression  : AST with type ast      = Parsetree.expression
                          and type ast_desc = Parsetree.expression_desc
module Pattern     : AST with type ast      = Parsetree.pattern
                          and type ast_desc = Parsetree.pattern_desc
module Module_type : AST with type ast      = Parsetree.module_type
                          and type ast_desc = Parsetree.module_type_desc

(** Each syntactic category will include a module that meets this signature.
    Then, the [Make_of_ast] functor produces the functions that actually
    convert from the Parsetree AST to the extensions one. *)
module type Of_ast_parameters = sig

  (** Which syntactic category is this concerning? e.g. [module AST = Expression] *)
  module AST : AST

  (** The extension type of this syntactic category, shared across extensions.
      e.g. [Extension.Expression.t] *)
  type t

  (** A function to convert [Parsetree]'s AST to the extension's.
      The choice of extension is extracted from the e.g.
      [[%extensions.comprehensions]] node, and the argument to that
      node is passed in as the [Parsetree] AST.

      So, for example, if [of_ast] spots the expression

      {[
        [%extensions.comprehensions] blah
      ]}

      then it will call [of_ast_internal Comprehensions blah].

      If the given extension does not actually extend the
      syntactic category, return None; this will be reported
      as an error. (Example: there are no pattern comprehensions,
      so when building the pattern extension AST, this function will
      return [None] when the extension in [Comprehensions].)
  *)
  val of_ast_internal : Language_extension.t -> AST.ast -> t option
end

(** Build the [of_ast] function from [Of_ast_parameters]. The result
    of this functor should be [include]d in modules implementing [Extensions.AST].
*)
module Make_of_ast (Params : Of_ast_parameters) : sig

  (** Interpret an AST term in the specified syntactic category as a term of the
      appropriate auxiliary language extension AST if possible.  Raises an error
      if the extension it finds is disabled or if the language extension
      embedding is malformed.  *)
  val of_ast : Params.AST.ast -> Params.t option
end

(** Require that an extension is enabled, or else throw an exception (of an
    unexported type) at the provided location saying otherwise.  This is
    intended to be used in "extensions.ml" when a certain piece of syntax
    requires two extensions to be enabled at once (e.g., immutable array
    comprehensions such as [[:x for x = 1 to 10:]]). *)
val assert_extension_enabled : loc:Location.t -> Language_extension.t -> unit

(** Errors around the extension representation.  These should mostly just be
    fatal, but they're needed for one test case
    (language-extensions/language_extensions.ml). *)
module Error : sig
  (** An error triggered when desugaring a language extension from an OCaml
      AST; left abstract because it should always be fatal *)
  type error

  (** The exception type thrown when desugaring a language extension from an
      OCaml AST *)
  exception Error of Location.t * error
end
