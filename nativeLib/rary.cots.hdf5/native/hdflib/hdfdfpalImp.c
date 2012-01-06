
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
 *  This code is the C-interface called by Java programs to access the
 *  HDF 4.1 library.
 *
 *  Each routine wraps a single HDF entry point, generally with the
 *  analogous arguments and return codes.
 *
 *  For details of the HDF libraries, see the HDF Documentation at:
 *     http://hdf.ncsa.uiuc.edu
 *
 */
#ifdef __cplusplus
extern "C" {
#endif


#include "hdf.h"
#include "jni.h"

#ifdef __cplusplus
#define ENVPTR (env)
#define ENVPAR 
#else
#define ENVPTR (*env)
#define ENVPAR env,
#endif

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_DFPaddpal
( JNIEnv *env,
jclass clss,
jstring filename,
jbyteArray palette)  /* IN:  byte[] */
{
    intn rval;
    char * f;
    jbyte *dat;
    jboolean bb;

    f = (char *) ENVPTR->GetStringUTFChars(ENVPAR filename,0);
    dat = ENVPTR->GetByteArrayElements(ENVPAR palette,&bb);

    rval = DFPaddpal((char *)f, (VOIDP) dat);

    ENVPTR->ReleaseStringUTFChars(ENVPAR filename,f);
    ENVPTR->ReleaseByteArrayElements(ENVPAR palette,dat,JNI_ABORT);
    if (rval == FAIL) {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_DFPgetpal
( JNIEnv *env,
jclass clss,
jstring filename,
jbyteArray palette)  /* OUT:  byte[] */
{
    intn rval;
    char * f;
    jbyte *dat;
    jboolean bb;

    f = (char *) ENVPTR->GetStringUTFChars(ENVPAR filename,0);
    dat = ENVPTR->GetByteArrayElements(ENVPAR palette,&bb);

    rval = DFPgetpal((char *)f, (VOIDP) dat);

    ENVPTR->ReleaseStringUTFChars(ENVPAR filename,f);
    if (rval == FAIL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR palette,dat,JNI_ABORT);
        return JNI_FALSE;
    } else {
        ENVPTR->ReleaseByteArrayElements(ENVPAR palette,dat,0);
        return JNI_TRUE;
    }
}

JNIEXPORT jshort JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_DFPlastref
( JNIEnv *env,
jobject obj)
{
    return (DFPlastref( ));
}

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_DFPnpals
( JNIEnv *env,
jclass clss,
jstring filename)
{
    intn rval;
    char * f;
    f = (char *) ENVPTR->GetStringUTFChars(ENVPAR filename,0);
    rval = DFPnpals((char *)f);

    ENVPTR->ReleaseStringUTFChars(ENVPAR filename,f);
    return rval;
}

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_DFPputpal
( JNIEnv *env,
jclass clss,
jstring filename,
jbyteArray palette, /* IN:  byte[] */
jint overwrite,
jstring filemode)
{
    intn rval;
    char * f;
    char * m;
    jbyte *dat;
    jboolean bb;

    f = (char *) ENVPTR->GetStringUTFChars(ENVPAR filename,0);
    m = (char *) ENVPTR->GetStringUTFChars(ENVPAR filemode,0);
    dat = ENVPTR->GetByteArrayElements(ENVPAR palette,&bb);

    rval = DFPputpal ((char *)f, (VOIDP) dat, (intn) overwrite, (char *)m);

    ENVPTR->ReleaseStringUTFChars(ENVPAR filename,f);
    ENVPTR->ReleaseStringUTFChars(ENVPAR filemode,m);
    ENVPTR->ReleaseByteArrayElements(ENVPAR palette,dat,JNI_ABORT);
    if (rval == FAIL) {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_DFPreadref
( JNIEnv *env,
jclass clss,
jstring filename,
jshort ref)
{
    intn rval;
    char * f;
    f = (char *) ENVPTR->GetStringUTFChars(ENVPAR filename,0);

    rval = DFPreadref((char *)f, (uint16) ref);
    ENVPTR->ReleaseStringUTFChars(ENVPAR filename,f);
    if (rval == FAIL) {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}

JNIEXPORT jshort JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_DFPrestart
( JNIEnv *env,
jobject obj)
{
    return (DFPrestart( ));
}

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_DFPwriteref
( JNIEnv *env,
jclass clss,
jstring filename,
jshort ref)
{
    intn rval;
    char * f;
    f = (char *) ENVPTR->GetStringUTFChars(ENVPAR filename,0);

    rval = DFPwriteref((char *)f, (uint16) ref);

    ENVPTR->ReleaseStringUTFChars(ENVPAR filename,f);

    if (rval == FAIL) {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}


#ifdef __cplusplus
}
#endif
