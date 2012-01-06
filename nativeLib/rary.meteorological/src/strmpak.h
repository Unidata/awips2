
// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// strmpak.h
//
// Interface to a fortran streamline drawing routine, strmpak.f
//
// Author: J. Ramer
// ---------------------------------------------------------------------------

#ifndef _strmpak_H
#define _strmpak_H

#ifdef IDENT_H
static const char* const strmpak_h_Id =
  "$Id: strmpak.h,v 1.2 2000/08/16 23:12:36 murray Exp $";
#endif

#include "ExtFtn.h"

EXT_FTN (void, strmpak, (const float * U,
                         const float * V,
                         int * work,
                         const int * mnx,
                         const int * nx,
                         const int * ny,
                         const int * asize,
                         const float * minspc,
                         const float * maxspc,
                         const float * badlo,
                         const float * badhi))  

EXT_FTN (void, strmsmth, (float * smoothness, int * npass))

#endif


