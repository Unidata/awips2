
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

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_VFfieldesize
( JNIEnv *env,
jclass clss,
jint vdata_id,
int field_index)
{
    return (VFfieldesize((int32) vdata_id,  (int32) field_index));
}

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_VFfieldisize
( JNIEnv *env,
jclass clss,
jint vdata_id,
int field_index)
{

    return (VFfieldisize((int32) vdata_id,  (int32) field_index));
}

JNIEXPORT jstring JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_VFfieldname
( JNIEnv *env,
jclass clss,
jint vdata_id,
int field_index)
{
    jstring rstring;
    char * str;

    str = VFfieldname((int32) vdata_id,  (int32) field_index);

    /* check for error */

    /* convert it to java string */
    rstring = ENVPTR->NewStringUTF(ENVPAR str);

    return rstring;
}

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_VFfieldorder
( JNIEnv *env,
jclass clss,
jint vdata_id,
int field_index)
{

        return (VFfieldorder((int32) vdata_id,  (int32) field_index));
}

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_VFfieldtype
( JNIEnv *env,
jclass clss,
jint vdata_id,
int field_index)
{

        return (VFfieldtype((int32) vdata_id,  (int32) field_index));
}

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_VFnfields
( JNIEnv *env,
jclass clss,
jint key)
{
    return (VFnfields((int32) key));
}

#ifdef __cplusplus
}
#endif
