/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <errno.h>
#include <string.h>
#include <caml/alloc.h>
#include <caml/sys.h>
#include "caml/unixsupport.h"

#ifdef CAML_RUNTIME_5

CAMLprim value caml_unix_error_message(value err)
{
  char buf[1024];
  int errnum = caml_unix_code_of_unix_error(err);
  return caml_copy_string(caml_strerror(errnum, buf, sizeof(buf)));
}

#else

CAMLprim value caml_unix_error_message(value err)
{
  char buf[1024];
  int errnum = caml_unix_code_of_unix_error(err);
  strerror_r(errnum, buf, sizeof(buf));
  return caml_copy_string(
    /* BACKPORT
    caml_strerror (errnum, buf, sizeof(buf)));
    */
    buf);
}

#endif
