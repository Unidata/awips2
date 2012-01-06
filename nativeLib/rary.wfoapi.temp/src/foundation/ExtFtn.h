/*
This software is in the public domain, furnished "as is", without technical
support, and with no warranty, express or implied, as to its usefulness for
any purpose.
  
ExtFtn.h
  
Some fortran compilers mangle the external symbols so that they do not
conflict with symbols from modules written in other languages.  In order
to link an executable that calls Fortran functions from a C function, the
C function must reference the mangled name.

This macro performs the following mangling operations on a Fortran function
depending on the following compilation switches.
  
-DFTN_PREPEND_UNDERSCORE  (i.e. _ftnfunc)
-DFTN_APPEND_UNDERSCORE(i.e. ftnfunc_)
-DFTN_PREPEND_DOUBLESCORE (i.e. __ftnfunc)
-DFTN_APPEND_DOUBLESCORE  (i.e. ftnfunc__)
  
If no switches are used, then no name mangling is done.
  
All external fortran declarations should be replaced with a call to
this macro:
  
EXT_FTN (funcReturnValue, funcName, funcArgs)
  
For example, the declaration:
  
extern "C" void gridbarbs(const float* uudd, const float* vvff);
  
becomes
  
EXT_FTN (void, gridbarbs, (const float* uudd, const float* vvff))
  
We know this is kind of ugly.  Ideally, you would just replace
extern "C" with EXT_FTN.  Unfortunately, the C preprocessor won't
allow that.
  
-- implementation ---------------------------------------------------------
  
This macro does two jobs.  First task is to come up with a new
external declaration with the mangled name, which is fairly trivial.
  
The second task is to make sure all invocations of the function
reference the mangled function name.  This is not so trivial.
If CPP allowed us to nest macros definitions, we could do something like this:
  
  #ifdef FTN_APPEND_UNDERSCORE 
       #define EXT_FUN(rval,fn,args)\
       extern "C" rval fn ## _ args;\
       #define fn fn ## _
  #endif

But the CPP is not that sophisticated.  The alternative we came up with
is to use a function pointer.  The name of the pointer is the non-mangled
function name, and it is initialized to point to the mangled function.

This works but there is a few loose ends we had to tie down.  The
function pointer has to be file scope (static).  If its global,
it would be very easy to have duplicate symbols defined.  It is
possible that the declaration could be in a header file with lots
of other Fortran declarations.  If a module never references a
declaration, GCC will spit out an unused variable warning for the
function pointer that goes with the declaration.  To get around
that, we use the GCC builtin unused attribute.  Not pretty, but
does the trick.

One final note.  We originally intended the compiler switches to be more
flexible where a prefix and a suffix string can be specified.  Unfortunately,
due to limitations of the CPP, we couldn't get that to work.
*/

#ifndef _ExtFtn_h
#define _ExtFtn_h

#ifdef __GNUC__
    #define __UNUSED __attribute__ ((unused))
#else
    #define __UNUSED
#endif

#ifdef __cplusplus
    #define EXTERN_C extern "C"
#else
    #define EXTERN_C extern
#endif

#define EXT_FTN__(funcReturnVal,funcName,funcArgs,pre,post)\
   EXTERN_C funcReturnVal pre ## funcName ## post funcArgs;\
   static funcReturnVal (*funcName) funcArgs __UNUSED=pre ## funcName ## post;

#if defined (FTN_APPEND_UNDERSCORE)
    #define EXT_FTN(funcReturnVal,funcName,funcArgs)\
        EXT_FTN__(funcReturnVal,funcName,funcArgs, ,_)
    #define FTN_MANGLE(funcName) funcName ## _
#elif defined (FTN_PREPEND_UNDERSCORE)
    #define EXT_FTN(funcReturnVal,funcName,funcArgs)\
        EXT_FTN__(funcReturnVal,funcName,funcArgs,_, )
    #define FTN_MANGLE(funcName) _ ## funcName
#elif defined (FTN_PREPEND_DOUBLESCORE)
    #define EXT_FTN(funcReturnVal,funcName,funcArgs)\
        EXT_FTN__(funcReturnVal,funcName,funcArgs,__, )
    #define FTN_MANGLE(funcName) __ ## funcName
#elif defined (FTN_APPEND_DOUBLESCORE)
    #define EXT_FTN(funcReturnVal,funcName,funcArgs)\
        EXT_FTN__(funcReturnVal,funcName,funcArgs, ,__)
    #define FTN_MANGLE(funcName) funcName ## __
#else
    #define EXT_FTN(funcReturnVal,funcName,funcArgs)\
        EXTERN_C funcReturnVal funcName funcArgs;
    #define FTN_MANGLE(funcName) funcName
#endif

#endif
