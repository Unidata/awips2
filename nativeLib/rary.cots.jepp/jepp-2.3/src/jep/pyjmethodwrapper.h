/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 4 c-style: "K&R" -*- */
/* 
   jep - Java Embedded Python

   Copyright (c) 2004 - 2008 Mike Johnson.

   This file is licenced under the the zlib/libpng License.

   This software is provided 'as-is', without any express or implied
   warranty. In no event will the authors be held liable for any
   damages arising from the use of this software.
   
   Permission is granted to anyone to use this software for any
   purpose, including commercial applications, and to alter it and
   redistribute it freely, subject to the following restrictions:

   1. The origin of this software must not be misrepresented; you
   must not claim that you wrote the original software. If you use
   this software in a product, an acknowledgment in the product
   documentation would be appreciated but is not required.

   2. Altered source versions must be plainly marked as such, and
   must not be misrepresented as being the original software.

   3. This notice may not be removed or altered from any source
   distribution.   
*/ 	

/*
  August 2, 2012
  Modified by Raytheon (c) 2012 Raytheon Company. All Rights Reserved.
   New file created with modifications to pyjmethod.h
*/



// shut up the compiler
#ifdef _POSIX_C_SOURCE
#  undef _POSIX_C_SOURCE
#endif
#include <jni.h>
#include <Python.h>


#ifndef _Included_pyjmethodwrapper
#define _Included_pyjmethodwrapper

#include "pyjobject.h"
#include "pyjmethod.h"


typedef struct {
    PyObject_HEAD
    PyJmethod_Object *method;
    PyJobject_Object *object;
} PyJmethodWrapper_Object;

PyJmethodWrapper_Object* pyjmethodwrapper_new(
                                PyJobject_Object*, PyJmethod_Object*);

#endif // ndef pyjmethodwrapper
