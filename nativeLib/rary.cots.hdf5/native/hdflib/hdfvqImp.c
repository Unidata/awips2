
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

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_VQueryref
( JNIEnv *env,
jclass clss,
jint vkey)
{
    return VQueryref((int32) vkey);
}

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_VQuerytag
( JNIEnv *env,
jclass clss,
jint vkey)
{
    return VQuerytag((int32) vkey);

}

#ifdef __cplusplus
}
#endif
