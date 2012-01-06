/****************************************************************************
 * NCSA HDF                                                                 *
 * National Comptational Science Alliance                                   *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf-java/COPYING file.                                                   *
 *                                                                          *
 ****************************************************************************/
/*
 *  This is a utility program used by the HDF Java-C wrapper layer to
 *  generate exceptions.  This may be called from any part of the
 *  Java-C interface.
 *
 */
#ifdef __cplusplus
extern "C" {
#endif

#include "jni.h"
#include <stdlib.h>

#ifdef __cplusplus
#define ENVPTR (env)
#define ENVPAR 
#else
#define ENVPTR (*env)
#define ENVPAR env,
#endif

jboolean h4buildException( JNIEnv *env, jint HDFerr)
{
jmethodID jm;
jclass jc;
int args[2];
jobject ex;
int rval;


    jc = ENVPTR->FindClass(ENVPAR  "ncsa/hdf/hdflib/HDFLibraryException");
    if (jc == NULL) {
        return JNI_FALSE;
    }
    jm = ENVPTR->GetMethodID(ENVPAR  jc, "<init>", "(I)V");
    if (jm == NULL) {
        return JNI_FALSE;
    }
    args[0] = HDFerr;
    args[1] = 0;

    ex = ENVPTR->NewObjectA (ENVPAR jc, jm, (jvalue *)args );

    rval = ENVPTR->Throw(ENVPAR  (jthrowable)ex );

    return JNI_TRUE;
}

jboolean h4NotImplemented( JNIEnv *env, char *functName)
{
jmethodID jm;
jclass jc;
char * args[2];
jobject ex;
jstring str;
int rval;


    jc = ENVPTR->FindClass(ENVPAR  "ncsa/hdf/hdflib/HDFNotImplementedException");
    if (jc == NULL) {
        return JNI_FALSE;
    }
    jm = ENVPTR->GetMethodID(ENVPAR  jc, "<init>", "(Ljava/lang/String;)V");
    if (jm == NULL) {
        return JNI_FALSE;
    }

    str = ENVPTR->NewStringUTF(ENVPAR functName);
    args[0] = (char *)str;
    args[1] = 0;
    ex = ENVPTR->NewObjectA (ENVPAR jc, jm, (jvalue *)args );

    rval = ENVPTR->Throw(ENVPAR  (jthrowable)ex );

    return JNI_TRUE;
}

jboolean h4outOfMemory( JNIEnv *env, char *functName)
{
jmethodID jm;
jclass jc;
char * args[2];
jobject ex;
jstring str;
int rval;

    jc = ENVPTR->FindClass(ENVPAR  "java/lang/OutOfMemoryError");
    if (jc == NULL) {
        return JNI_FALSE;
    }
    jm = ENVPTR->GetMethodID(ENVPAR  jc, "<init>", "(Ljava/lang/String;)V");
    if (jm == NULL) {
        return JNI_FALSE;
    }

    str = ENVPTR->NewStringUTF(ENVPAR functName);
    args[0] = (char *)str;
    args[1] = 0;
    ex = ENVPTR->NewObjectA (ENVPAR jc, jm, (jvalue *)args );

    rval = ENVPTR->Throw(ENVPAR  (jthrowable)ex );

    return JNI_TRUE;
}

/*
 *  A fatal error in a JNI call
 */
jboolean h4JNIFatalError( JNIEnv *env, char *functName)
{
jmethodID jm;
jclass jc;
char * args[2];
jobject ex;
jstring str;
int rval;

    jc = ENVPTR->FindClass(ENVPAR  "java/lang/InternalError");
    if (jc == NULL) {
        return JNI_FALSE;
    }
    jm = ENVPTR->GetMethodID(ENVPAR  jc, "<init>", "(Ljava/lang/String;)V");
    if (jm == NULL) {
        return JNI_FALSE;
    }

    str = ENVPTR->NewStringUTF(ENVPAR functName);
    args[0] = (char *)str;
    args[1] = 0;
    ex = ENVPTR->NewObjectA (ENVPAR jc, jm, (jvalue *)args );

    rval = ENVPTR->Throw(ENVPAR  (jthrowable)ex );

    return JNI_TRUE;
}

jboolean h4raiseException( JNIEnv *env, char *message)
{
jmethodID jm;
jclass jc;
char * args[2];
jobject ex;
jstring str;
int rval;

    jc = ENVPTR->FindClass(ENVPAR  "ncsa/hdf/hdflib/HDFLibraryException");
    if (jc == NULL) {
        return JNI_FALSE;
    }
    jm = ENVPTR->GetMethodID(ENVPAR  jc, "<init>", "(Ljava/lang/String;)V");
    if (jm == NULL) {
        return JNI_FALSE;
    }

    str = ENVPTR->NewStringUTF(ENVPAR message);
    args[0] = (char *)str;
    args[1] = 0;
    ex = ENVPTR->NewObjectA (ENVPAR jc, jm, (jvalue *)args );

    rval = ENVPTR->Throw(ENVPAR  (jthrowable)ex );

    return JNI_TRUE;
}

#ifdef __cplusplus
}
#endif
