/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id: getpeername.c 11156 2011-07-27 14:17:02Z doligez $ */

#include <fail.h>
#include <mlvalues.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"

CAMLprim value unix_getpeername(value sock)
{
  int retcode;
  union sock_addr_union addr;
  socklen_param_type addr_len;

  addr_len = sizeof(addr);
  retcode = getpeername(Int_val(sock), &addr.s_gen, &addr_len);
  if (retcode == -1) uerror("getpeername", Nothing);
  return alloc_sockaddr(&addr, addr_len, -1);
}

#else

CAMLprim value unix_getpeername(value sock)
{ invalid_argument("getpeername not implemented"); }

#endif
