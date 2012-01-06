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
 *  Identifier API Functions of the HDF5 library.
 *
 *  Each routine wraps a single HDF entry point, generally with the
 *  analogous arguments and return codes.
 *
 *  For details of the HDF libraries, see the HDF Documentation at:
 *   http://hdf.ncsa.uiuc.edu/HDF5/doc/
 *
 */

#ifdef __cplusplus
extern "C" {
#endif

#include "hdf5.h"
#include <stdlib.h>
#include <jni.h>

#ifdef __cplusplus
#define ENVPTR (env)
#define ENVPAR 
#else
#define ENVPTR (*env)
#define ENVPAR env,
#endif

extern jboolean h5outOfMemory( JNIEnv *env, char *functName);
extern jboolean h5libraryError( JNIEnv *env );
extern jboolean h5badArgument( JNIEnv *env, char *functName);

/*
 * Class:     ncsa_hdf_hdf5lib_H5Header
 * Method:    H5Gget_linkval
 * Signature: (ILjava/lang/String;I[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Iget_1type
  (JNIEnv *env, jclass clss, jint obj_id)
{
    H5I_type_t retVal = H5I_BADID;
    retVal =  H5Iget_type((hid_t)obj_id);
    if (retVal == H5I_BADID) {
        h5libraryError(env);
    }
    return (jint)retVal;
}


/**********************************************************************
 *                                                                    *
 *          New functions release 1.6.2 versus release 1.6.1          *
 *                                                                    *
 **********************************************************************/

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Iget_name(hid_t obj_id, char *name, size_t size )
 * Signature: (IJLjava/lang/String;)J
 */
JNIEXPORT jlong JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Iget_1name
  (JNIEnv *env, jclass clss, jint obj_id, jobjectArray name, jlong buf_size)
{
    char *aName;
    jstring str;
    hssize_t size;
    long bs;

    bs = (long)buf_size;
    if (bs <= 0) {
        h5badArgument( env, "H5Iget_name:  buf_size <= 0");
        return -1;
    }
    aName = (char*)malloc(sizeof(char)*bs);
    if (aName == NULL) {
        h5outOfMemory( env, "H5Iget_name:  malloc failed");
        return -1;
    }

    size = H5Iget_name((hid_t)obj_id, aName, (size_t)buf_size);

    if (size < 0) {
        free(aName);
        h5libraryError(env);
        return -1;
        /*  exception, returns immediately */
    }
    /* successful return -- save the string; */
    str = ENVPTR->NewStringUTF(ENVPAR aName);
    ENVPTR->SetObjectArrayElement(ENVPAR name,0,str);

    free(aName);
    return (jlong)size;
}


/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Signature: int H5Iget_ref(hid_t obj_id)
 * Purpose:   Retrieves the reference count for an object
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Iget_1ref
  (JNIEnv *env, jclass clss, jint obj_id)
{
    int retVal = -1;
    retVal = H5Iget_ref( (hid_t)obj_id);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Signature: int H5Iinc_ref(hid_t obj_id)
 * Purpose:   Increments the reference count for an object
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Iinc_1ref
  (JNIEnv *env, jclass clss, jint obj_id)
{
    int retVal = -1;
    retVal = H5Iinc_ref( (hid_t)obj_id);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Signature: int H5Idec_ref(hid_t obj_id)
 * Purpose:   Decrements the reference count for an object
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Idec_1ref
  (JNIEnv *env, jclass clss, jint obj_id)
{
    int retVal = -1;
    retVal = H5Idec_ref( (hid_t)obj_id);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}



/**********************************************************************
 *                                                                    *
 *          New functions release 1.6.3 versus release 1.6.2          *
 *                                                                    *
 **********************************************************************/

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Signature:  hid_t H5Iget_file_id (hid_t obj_id)
 * Purpose:
 */

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Iget_1file_1id
  (JNIEnv *env, jclass clss, jint obj_id)
{
    hid_t file_id = 0;

    file_id = H5Iget_file_id ((hid_t) obj_id);

    if (file_id < 0) {
        h5libraryError(env);
    }

    return (jint) file_id;
}


#ifdef __cplusplus
}
#endif
