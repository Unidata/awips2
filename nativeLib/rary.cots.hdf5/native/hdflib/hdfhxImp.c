
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

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_HXsetcreatedir
( JNIEnv *env,
jclass clss,
jstring dir)
{
    intn rval;
    char *str;

    if (dir != NULL) {
        str =(char *) ENVPTR->GetStringUTFChars(ENVPAR dir,0);
    } else {
        str = NULL;
    }

    rval = HXsetcreatedir((char *)str);

    if (str != NULL) {
        ENVPTR->ReleaseStringUTFChars(ENVPAR dir,str);
    }

    if (rval == FAIL) {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_HXsetdir
( JNIEnv *env,
jclass clss,
jstring dir)
{
    intn rval;
    char *str;

    if (dir != NULL) {
        str =(char *) ENVPTR->GetStringUTFChars(ENVPAR dir,0);
    } else {
        str = NULL;
    }

    rval = HXsetdir(str);

    if (str != NULL) {
        ENVPTR->ReleaseStringUTFChars(ENVPAR dir,str);
    }

    if (rval == FAIL) {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}

#ifdef __cplusplus
}
#endif
