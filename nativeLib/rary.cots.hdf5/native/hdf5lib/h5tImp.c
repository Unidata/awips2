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
 *  Datatype Object API Functions of the HDF5 library.
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
#include <jni.h>
#include <stdlib.h>

#ifdef __cplusplus
#define ENVPTR (env)
#define ENVPAR 
#else
#define ENVPTR (*env)
#define ENVPAR env,
#endif

extern jboolean h5outOfMemory( JNIEnv *env, char *functName);
extern jboolean h5JNIFatalError( JNIEnv *env, char *functName);
extern jboolean h5nullArgument( JNIEnv *env, char *functName);
extern jboolean h5libraryError( JNIEnv *env );
extern jboolean h5badArgument( JNIEnv *env, char *functName);

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Topen
 * Signature: (ILjava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Topen
  (JNIEnv *env, jclass clss, jint loc_id, jstring name)
{
    hid_t status;
    char* tname;
    jboolean isCopy;

    if (name == NULL) {
        h5nullArgument( env, "H5Topen:  name is NULL");
        return -1;
    }
    tname = (char *)ENVPTR->GetStringUTFChars(ENVPAR name,&isCopy);
    if (tname == NULL) {
        h5JNIFatalError(env,  "H5Topen:  name not pinned");
        return -1;
    }

    status = H5Topen(loc_id, tname);

    ENVPTR->ReleaseStringUTFChars(ENVPAR name,tname);
    if (status < 0) {
        h5libraryError(env);
    }
    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tcommit
 * Signature: (ILjava/lang/String;I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tcommit
  (JNIEnv *env, jclass clss, jint loc_id, jstring name, jint type)
{
    herr_t status;
    char* tname;
    jboolean isCopy;

    if (name == NULL) {
        h5nullArgument( env, "H5Tcommit:  name is NULL");
        return -1;
    }
    tname = (char *)ENVPTR->GetStringUTFChars(ENVPAR name,&isCopy);
    if (tname == NULL) {
        h5JNIFatalError(env,  "H5Tcommit:  name not pinned");
        return -1;
    }

    status = H5Tcommit(loc_id, tname, type );

    ENVPTR->ReleaseStringUTFChars(ENVPAR name,tname);
    if (status < 0) {
        h5libraryError(env);
    }
    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tcommitted
 * Signature: (I)B
 */
JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tcommitted
  (JNIEnv *env, jclass clss, jint type)
{
    htri_t bval;
    bval = H5Tcommitted(type);
    if (bval > 0) {
        return JNI_TRUE;
    } else if (bval == 0) {
        return JNI_FALSE;
    } else {
        /* raise exception -- return value is irrelevant */
        h5libraryError(env);
        return JNI_FALSE;
    }
}


/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tcreate
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Tcreate
  (JNIEnv *env, jclass clss, jint dclass, jint size)
{
    hid_t retVal = -1;
    retVal =  H5Tcreate((H5T_class_t )dclass, size );
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tcopy
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Tcopy
  (JNIEnv *env, jclass clss, jint type_id)
{
    hid_t retVal = -1;
    retVal =  H5Tcopy(type_id);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tequal
 * Signature: (II)B
 */
JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tequal
  (JNIEnv *env, jclass clss, jint type_id1, jint type_id2)
{
    htri_t bval;
    bval = H5Tequal(type_id1, type_id2 );
    if (bval > 0) {
        return JNI_TRUE;
    } else if (bval == 0) {
        return JNI_FALSE;
    } else {
        /* raise exception -- return value is irrelevant */
        h5libraryError(env);
        return JNI_FALSE;
    }
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tlock
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tlock
  (JNIEnv *env, jclass clss, jint type_id)
{
    herr_t retVal = -1;
    retVal =  H5Tlock(type_id );
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_class
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1class
  (JNIEnv *env, jclass clss, jint type_id)
{
    H5T_class_t retVal = H5T_NO_CLASS;
    retVal =  H5Tget_class(type_id );
    if (retVal == H5T_NO_CLASS) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_size
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1size
  (JNIEnv *env, jclass clss, jint type_id)
{
    size_t retVal = 0;
    retVal =  H5Tget_size(type_id );
    if (retVal == 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tset_size
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tset_1size
  (JNIEnv *env, jclass clss, jint type_id, jint size)
{
    herr_t retVal = -1;
    retVal =  H5Tset_size(type_id, size );
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_order
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1order
  (JNIEnv *env, jclass clss, jint type_id)
{
    H5T_order_t retVal = H5T_ORDER_ERROR;
    retVal =  H5Tget_order(type_id );
    if (retVal == H5T_ORDER_ERROR) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tset_order
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tset_1order
  (JNIEnv *env, jclass clss, jint type_id, jint order)
{
    herr_t retVal = -1;
    retVal =  H5Tset_order(type_id, (H5T_order_t)order);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_precision
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1precision
  (JNIEnv *env, jclass clss, jint type_id)
{
    size_t retVal = 0;
    retVal =  H5Tget_precision(type_id);
    if (retVal == 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tset_precision
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tset_1precision
  (JNIEnv *env, jclass clss, jint type_id, jint precision)
{
    herr_t retVal = -1;
    retVal =  H5Tset_precision(type_id, precision);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_offset
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1offset
  (JNIEnv *env, jclass clss, jint type_id)
{
    size_t retVal = 0;
    retVal =  H5Tget_offset(type_id);
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tset_offset
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tset_1offset
  (JNIEnv *env, jclass clss, jint type_id, jint offset)
{
    herr_t retVal = -1;
    retVal =  H5Tset_offset(type_id, offset);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_pad
 * Signature: (I[I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1pad
  (JNIEnv *env, jclass clss, jint type_id, jintArray pad)
{
    herr_t status;
    jboolean isCopy;
    jint *P;

    if (pad == NULL) {
        h5nullArgument( env, "H5Tget_pad:  pad is NULL");
        return -1;
    }
    P = ENVPTR->GetIntArrayElements(ENVPAR pad,&isCopy);
    if (P == NULL) {
        h5JNIFatalError(env,  "H5Tget_pad:  pad not pinned");
        return -1;
    }
    status = H5Tget_pad(type_id, (H5T_pad_t *)&(P[0]), (H5T_pad_t *)&(P[1]));
    if (status < 0) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR pad,P,JNI_ABORT);
        h5libraryError(env);
    } else  {
        ENVPTR->ReleaseIntArrayElements(ENVPAR pad,P,0);
    }

    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tset_pad
 * Signature: (III)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tset_1pad
  (JNIEnv *env, jclass clss, jint type_id, jint lsb, jint msb)
{
    herr_t retVal = -1;
    retVal =  H5Tset_pad(type_id, (H5T_pad_t)lsb, (H5T_pad_t)msb);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_sign
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1sign
  (JNIEnv *env, jclass clss, jint type_id)
{
    H5T_sign_t retVal = H5T_SGN_ERROR;
    retVal =  H5Tget_sign(type_id);
    if (retVal == H5T_SGN_ERROR) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tset_sign
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tset_1sign
  (JNIEnv *env, jclass clss, jint type_id, jint sign)
{
    herr_t retVal = -1;
    retVal =  H5Tset_sign(type_id, (H5T_sign_t)sign);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_fields
 * Signature: (I[I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1fields
  (JNIEnv *env, jclass clss, jint type_id, jintArray fields)
{
    herr_t status;
    jboolean isCopy;
    jint *P;

    if (fields == NULL) {
        h5nullArgument( env, "H5Tget_fields:  fields is NULL");
        return -1;
    }
    if (ENVPTR->GetArrayLength(ENVPAR fields) < 5) {
        h5badArgument( env, "H5Tget_fields:  fields input array < order 5");
        return -1;
    }
    P = ENVPTR->GetIntArrayElements(ENVPAR fields,&isCopy);
    if (P == NULL) {
        h5JNIFatalError(env,  "H5Tget_fields:  fields not pinned");
        return -1;
    }

    status = H5Tget_fields(type_id, (size_t *)&(P[0]), (size_t *)&(P[1]), (size_t *)&(P[2]), (size_t *)&(P[3]), (size_t *)&(P[4]));

    if (status < 0) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR fields,P,JNI_ABORT);
        h5libraryError(env);
    } else  {
        ENVPTR->ReleaseIntArrayElements(ENVPAR fields,P,0);
    }

    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tset_fields
 * Signature: (IIIII)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tset_1fields
  (JNIEnv *env, jclass clss, jint type_id, jint spos, jint epos,
  jint esize, jint mpos, jint msiz)
{
    herr_t retVal = -1;
    retVal =  H5Tset_fields(type_id, spos, epos, esize, mpos, msiz);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_ebias
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1ebias
  (JNIEnv *env, jclass clss, jint type_id)
{
    size_t retVal = 0;
    retVal =  H5Tget_ebias(type_id );
    if (retVal == 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tset_ebias
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tset_1ebias
  (JNIEnv *env, jclass clss, jint type_id, jint ebias)
{
    herr_t retVal = -1;
    retVal =  H5Tset_ebias(type_id, ebias);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_norm
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1norm
  (JNIEnv *env, jclass clss, jint type_id)
{
    H5T_norm_t retVal = H5T_NORM_ERROR;
    retVal =  H5Tget_norm(type_id);
    if (retVal == H5T_NORM_ERROR) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tset_norm
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tset_1norm
  (JNIEnv *env, jclass clss, jint type_id, jint norm)
{
    herr_t retVal = -1;
    retVal =  H5Tset_norm(type_id, (H5T_norm_t )norm);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_inpad
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1inpad
  (JNIEnv *env, jclass clss, jint type_id)
{
    H5T_pad_t retVal = H5T_PAD_ERROR;
    retVal =  H5Tget_inpad(type_id );
    if (retVal == H5T_PAD_ERROR) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tset_inpad
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tset_1inpad
  (JNIEnv *env, jclass clss, jint type_id, jint inpad)
{
    herr_t retVal = -1;
    retVal = H5Tset_inpad(type_id, (H5T_pad_t) inpad);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_cset
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1cset
  (JNIEnv *env, jclass clss, jint type_id)
{
    H5T_cset_t retVal = H5T_CSET_ERROR;
    retVal =  H5Tget_cset(type_id);
    if (retVal == H5T_CSET_ERROR) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tset_cset
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tset_1cset
  (JNIEnv *env, jclass clss, jint type_id, jint cset)
{
    herr_t retVal = -1;
    retVal =  H5Tset_cset(type_id, (H5T_cset_t)cset);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_strpad
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1strpad
  (JNIEnv *env, jclass clss, jint type_id)
{
    H5T_str_t retVal = H5T_STR_ERROR;
    retVal =  H5Tget_strpad(type_id);
    if (retVal == H5T_STR_ERROR) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tset_strpad
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tset_1strpad
  (JNIEnv *env, jclass clss, jint type_id, jint strpad)
{
    herr_t retVal = -1;
    retVal =  H5Tset_strpad(type_id, (H5T_str_t)strpad);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_nmembers
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1nmembers
  (JNIEnv *env, jclass clss, jint type_id)
{
    int retVal = -1;
    retVal =  H5Tget_nmembers(type_id);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_member_name
 * Signature: (II)Ljava/lang/String
 */
JNIEXPORT jstring JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1member_1name
  (JNIEnv *env, jclass clss, jint type_id, jint field_idx)
{
    char *name;
    jstring str;

    name = H5Tget_member_name(type_id, field_idx);

    if (name == NULL) {
        return NULL;
    } else {
        /* may throw OutOfMemoryError */
        str = ENVPTR->NewStringUTF(ENVPAR name);
        if (str == NULL)  {
            free(name);
            h5JNIFatalError(env,  "H5Tget_member_name:  returned string not created");
            return NULL;
        }
        free(name);
        return str;
    }
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_member_index
 * Signature: (ILjava/lang/String)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1member_1index
  (JNIEnv *env, jclass clss, jint type_id, jstring field_name)
{
    char *tname;
    int index;
    jboolean isCopy;

    if (field_name == NULL) {
        h5nullArgument( env, "H5Tget_member_index:  field_name is NULL");
        return -1;
    }
    tname = (char *)ENVPTR->GetStringUTFChars(ENVPAR field_name,&isCopy);
    if (tname == NULL) {
        h5JNIFatalError(env,  "H5Tget_member_index:  field_name not pinned");
        return -1;
    }

    index = H5Tget_member_index(type_id, tname);

    ENVPTR->ReleaseStringUTFChars(ENVPAR field_name,tname);

    return index;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_member_type
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Tget_1member_1type
  (JNIEnv *env, jclass clss, jint type_id, jint field_idx)
{
    hid_t retVal = -1;
    retVal =  H5Tget_member_type(type_id, field_idx);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_member_offset
 * Signature: (II)I
 */
JNIEXPORT jlong JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1member_1offset
  (JNIEnv *env, jclass clss, jint type_id, jint memno)
{
    size_t retVal = 0;
    retVal =  H5Tget_member_offset((hid_t)type_id, memno);
    return (jlong)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tinsert
 * Signature: (ILjava/lang/String;JI)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tinsert
  (JNIEnv *env, jclass clss, jint type_id, jstring name, jlong offset, jint field_id)
{
    herr_t status;
    char* tname;
    jboolean isCopy;
    long off;

    off = (long)offset;
    if (name == NULL) {
        h5nullArgument( env, "H5Tinsert:  name is NULL");
        return -1;
    }
    tname =(char *)ENVPTR->GetStringUTFChars(ENVPAR name,&isCopy);
    if (tname == NULL) {
        h5JNIFatalError(env,  "H5Tinsert:  name not pinned");
        return -1;
    }

    status = H5Tinsert(type_id, tname, (size_t)off, field_id);

    ENVPTR->ReleaseStringUTFChars(ENVPAR name,tname);
    if (status < 0) {
        h5libraryError(env);
    }
    return (jint)status;

}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tpack
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tpack
  (JNIEnv *env, jclass clss, jint type_id)
{
    herr_t retVal = -1;
    retVal =  H5Tpack(type_id);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tclose
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Tclose
  (JNIEnv *env, jclass clss, jint type_id)
{
    herr_t retVal = 0;

    if (type_id > 0)
        retVal =  H5Tclose(type_id);

    if (retVal < 0) {
        h5libraryError(env);
    }

    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tenum_create
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Tenum_1create
  (JNIEnv *env, jclass clss, jint base_id)
{
    hid_t status;

    status =  H5Tenum_create((hid_t)base_id);
    if (status < 0)
        h5libraryError(env);

    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tenum_insert
 * Signature: (ILjava/lang/String;[B)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tenum_1insert
  (JNIEnv *env, jclass clss, jint type, jstring name, jintArray value)
{
    herr_t status;
    jint *byteP;
    char *nameP;
    jboolean isCopy;

    if (name == NULL) {
        h5nullArgument( env, "H5Tenum_insert:  name is NULL");
        return -1;
    }

    nameP = (char *)ENVPTR->GetStringUTFChars(ENVPAR name,&isCopy);
    if (nameP == NULL) {
        h5JNIFatalError( env, "H5Tenum_insert:  name not pinned");
        return -1;
    }

    if ( value == NULL ) {
        ENVPTR->ReleaseStringUTFChars(ENVPAR name,nameP);
        h5nullArgument( env, "H5Tenum_insert:  value is NULL");
        return -1;
    }

    byteP = ENVPTR->GetIntArrayElements(ENVPAR value,&isCopy);
    if (byteP == NULL) {
        ENVPTR->ReleaseStringUTFChars(ENVPAR name,nameP);
        h5JNIFatalError( env, "H5Tenum_insert:  value not pinned");
        return -1;
    }

    status = H5Tenum_insert((hid_t)type, nameP, byteP);

    ENVPTR->ReleaseStringUTFChars(ENVPAR name,nameP);
    ENVPTR->ReleaseIntArrayElements(ENVPAR value,byteP,JNI_ABORT);

    if (status < 0) {
        h5libraryError(env);
    }

    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tenum_nameof
 * Signature: (I[B[Ljava/lang/String;I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tenum_1nameof
  (JNIEnv *env, jclass clss, jint type, jintArray value, jobjectArray name, jint size)
{
    hid_t status;
    jint *byteP;
    char *nameP;
    jboolean isCopy;
    jstring str;

    if (size <= 0) {
        h5badArgument( env, "H5Tenum_nameof:  name size < 0");
        return -1;
    }

    nameP = (char *)malloc(sizeof(char)*size);
    if (nameP == NULL) {
        /* exception -- out of memory */
        h5outOfMemory( env, "H5Tenum_nameof:  malloc name size");
        return -1;
    }

    if ( value == NULL ) {
        free(nameP);
        h5nullArgument( env, "H5Tenum_nameof:  value is NULL");
        return -1;
    }

    byteP = ENVPTR->GetIntArrayElements(ENVPAR value,&isCopy);
    if (byteP == NULL) {
        free(nameP);
        h5JNIFatalError( env, "H5Tenum_nameof:  value not pinned");
        return -1;
    }

    status = H5Tenum_nameof((hid_t)type, byteP, nameP, (size_t)size);

    ENVPTR->ReleaseIntArrayElements(ENVPAR value,byteP,JNI_ABORT);

    if (status < 0) {
        free(nameP);
        h5libraryError(env);
        return -1;
    }
    else {
        str = ENVPTR->NewStringUTF(ENVPAR nameP);
        if (str == NULL) {
            free(nameP);
            h5JNIFatalError( env, "H5Tenum_nameof:  return array not created");
            return -1;
        }
        /*  SetObjectArrayElement may raise exceptions */
        ENVPTR->SetObjectArrayElement(ENVPAR name,0,(jobject)str);
    }

    free(nameP);
    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tenum_valueof
 * Signature: (ILjava/lang/String;[B)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tenum_1valueof
  (JNIEnv *env, jclass clss, jint type, jstring name, jintArray value)
{
    hid_t status;
    jint *byteP;
    char *nameP;
    jboolean isCopy;

    if (name == NULL) {
        h5nullArgument( env, "H5Tenum_valueof:  name is NULL");
        return -1;
    }

    nameP = (char *)ENVPTR->GetStringUTFChars(ENVPAR name,&isCopy);
    if (nameP == NULL) {
        h5JNIFatalError( env, "H5Tenum_valueof:  name not pinned");
        return -1;
    }

    if ( value == NULL ) {
        ENVPTR->ReleaseStringUTFChars(ENVPAR name,nameP);
        h5nullArgument( env, "H5Tenum_valueof:  value is NULL");
        return -1;
    }

    byteP = ENVPTR->GetIntArrayElements(ENVPAR value,&isCopy);
    if (byteP == NULL)  {
        ENVPTR->ReleaseStringUTFChars(ENVPAR name,nameP);
        h5JNIFatalError( env, "H5Tenum_valueof:  value not pinned");
        return -1;
    }

    status = H5Tenum_valueof((hid_t)type, nameP, byteP);

    ENVPTR->ReleaseStringUTFChars(ENVPAR name,nameP);

    if (status < 0) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR value,byteP,JNI_ABORT);
        h5libraryError(env);
    } else {
        ENVPTR->ReleaseIntArrayElements(ENVPAR value,byteP,0);
    }

    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tvlen_create
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Tvlen_1create
  (JNIEnv *env, jclass clss, jint base_id)
{
    hid_t status;

    status = H5Tvlen_create((hid_t)base_id);
    if (status < 0)
        h5libraryError(env);

    return status;
}


/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tset_tag
 * Signature: (ILjava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tset_1tag
  (JNIEnv *env, jclass clss, jint type, jstring tag)
{
    herr_t status;
    char *tagP;
    jboolean isCopy;

    if (tag == NULL) {
        h5nullArgument( env, "H5Tset_tag:  tag is NULL");
        return -1;
    }

    tagP = (char *)ENVPTR->GetStringUTFChars(ENVPAR tag,&isCopy);
    if (tagP == NULL) {
        h5JNIFatalError( env, "H5Tset_tag:  tag not pinned");
        return -1;
    }

    status = H5Tset_tag((hid_t)type, tagP);

    ENVPTR->ReleaseStringUTFChars(ENVPAR tag,tagP);

    if (status < 0)
        h5libraryError(env);

    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_tag
 * Signature: (I)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1tag
  (JNIEnv *env, jclass clss, jint type)
{
    jstring str;
    char *tag;

    tag = H5Tget_tag((hid_t)type);

    if (tag == NULL)
        return NULL;

    /* may throw OutOfMemoryError */
    str = ENVPTR->NewStringUTF(ENVPAR tag);
    if (str == NULL)  {
        free(tag);
        h5JNIFatalError(env,  "H5Tget_tag:  returned string not created");
        return NULL;
    }

    free(tag);
    return str;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_super
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Tget_1super
  (JNIEnv *env, jclass clss, jint type)
{
    hid_t status;

    status = H5Tget_super((hid_t)type);
    if (status < 0)
        h5libraryError(env);

    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_member_value
 * Signature: (II[B)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1member_1value
  (JNIEnv *env, jclass clss, jint type, jint membno, jintArray value)
{
    hid_t status;
    jint *byteP;
    jboolean isCopy;

    if ( value == NULL ) {
        h5nullArgument( env, "H5Tget_member_value:  value is NULL");
        return -1;
    }

    byteP = ENVPTR->GetIntArrayElements(ENVPAR value,&isCopy);
    if (byteP == NULL) {
        h5JNIFatalError( env, "H5Tget_member_value:  value not pinned");
        return -1;
    }

    status = H5Tget_member_value((hid_t)type, (int)membno, byteP);

    if (status < 0) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR value,byteP,JNI_ABORT);
        h5libraryError(env);
    } else {
        ENVPTR->ReleaseIntArrayElements(ENVPAR value,byteP,0);
    }

    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tarray_create
 * Signature: (II[B[B)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Tarray_1create
  (JNIEnv *env, jclass clss, jint base, jint rank, jintArray dims, jintArray perms)
{
    hid_t status;
    jint *dimsP;
    jint *permP;
    int dlen;
    hsize_t *cdims=NULL;
    jboolean isCopy;
    int i;

    if (rank <= 0) {
        h5nullArgument( env, "H5Tarray_create:  rank is < 1");
        return -1;
    }
    if ( dims == NULL ) {
        h5nullArgument( env, "H5Tarray_create:  dims is NULL");
        return -1;
    }

    dimsP = ENVPTR->GetIntArrayElements(ENVPAR dims,&isCopy);
    if (dimsP == NULL) {
        h5JNIFatalError( env, "H5Tarray_create:  dimsP not pinned");
        return -1;
    }

    dlen = ENVPTR->GetArrayLength(ENVPAR dims);
    if (dlen != rank) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR dims,dimsP,JNI_ABORT);
        return -1;
    }

    if (perms == NULL) {
        permP = NULL;
    } else {
        permP = ENVPTR->GetIntArrayElements(ENVPAR perms,&isCopy);
        if (permP == NULL) {
            h5JNIFatalError( env, "H5Tarray_create:  permP not pinned");
            ENVPTR->ReleaseIntArrayElements(ENVPAR dims,dimsP,JNI_ABORT);
            return -1;
        }
    }

    cdims = (hsize_t *)malloc(dlen * sizeof(hsize_t));
    for (i = 0; i < dlen; i++) {
        cdims[i] = (hsize_t)dimsP[i];
    }

    status = H5Tarray_create((hid_t)base, (int)rank, (const hsize_t *)cdims, (const int *)permP);

    ENVPTR->ReleaseIntArrayElements(ENVPAR dims,dimsP,JNI_ABORT);
    if (permP != NULL) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR perms,permP,JNI_ABORT);
    }

    free (cdims);
    if (status < 0) {
        h5libraryError(env);
    }

    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_array_dims
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1array_1ndims
  (JNIEnv *env, jclass clss, jint dt)
{
    hid_t status;

    status = H5Tget_array_ndims((hid_t)dt);
    if (status < 0)
        h5libraryError(env);

    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tarray_get_dims
 * Signature: (I[I[I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tget_1array_1dims
  (JNIEnv *env, jclass clss, jint dt, jintArray dims, jintArray perms)
{
    hid_t status;
    jint *dimsP;
    jint *permP;
    int dlen;
    int i;
    hsize_t *cdims=NULL;
    jboolean isCopy;

    if ( dims == NULL ) {
        h5nullArgument( env, "H5Tget_array_dims:  value is NULL");
        return -1;
    }

    dimsP = ENVPTR->GetIntArrayElements(ENVPAR dims,&isCopy);
    if (dimsP == NULL) {
        h5JNIFatalError( env, "H5Tget_array_dims:  dimsP not pinned");
        return -1;
    }

    dlen = ENVPTR->GetArrayLength(ENVPAR dims);
    cdims = (hsize_t *)malloc(dlen * sizeof(hsize_t));

    if (perms == NULL) {
        permP = NULL;
    } else {
        permP = ENVPTR->GetIntArrayElements(ENVPAR perms,&isCopy);
        if (permP == NULL) {
            ENVPTR->ReleaseIntArrayElements(ENVPAR dims,dimsP,JNI_ABORT);
            h5JNIFatalError( env, "H5Tget_array_dims:  permP not pinned");
            return -1;
        }
    }

    status = H5Tget_array_dims((hid_t)dt, (hsize_t *)cdims, (int *)permP);

    if (status < 0) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR dims,dimsP,JNI_ABORT);
        if (permP != NULL) {
            ENVPTR->ReleaseIntArrayElements(ENVPAR perms,permP,JNI_ABORT);
        }
        h5libraryError(env);
    } else {
        for (i = 0; i < dlen; i++) {
            dimsP[i] = (jint) cdims[i];
        }
        ENVPTR->ReleaseIntArrayElements(ENVPAR dims,dimsP,0);
        if (permP != NULL) {
            ENVPTR->ReleaseIntArrayElements(ENVPAR perms,permP,0);
        }
    }

    if (cdims) free(cdims);

    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tis_variable_str(hid_t dtype_id )
 * Signature: (I)J
 */
JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tis_1variable_1str
  (JNIEnv *env, jclass clss, jint dtype_id)
{
    htri_t bval;
    bval = H5Tis_variable_str((hid_t)dtype_id);
    if (bval > 0) {
        return JNI_TRUE;
    } else if (bval == 0) {
        return JNI_FALSE;
    } else {
        h5libraryError(env);
        return JNI_FALSE;
    }
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tget_native_type(hid_t type_id, H5T_direction_t direction )
 * Signature: (I)J
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Tget_1native_1type
  (JNIEnv *env, jclass clss, jint dtype_id, jint direction)
{
    hid_t native_tid;

    native_tid = H5Tget_native_type((hid_t)dtype_id, (H5T_direction_t)direction);

    if (native_tid < 0){
        h5libraryError(env);
        return -1;
    }

    return (jint)native_tid;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Tdetect_class(hid_t dtype_id, H5T_class_t dtype_class )
 * Signature: (I)J
 */
JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Tdetect_1class
  (JNIEnv *env, jclass clss, jint dtype_id, jint dtype_class)
{
    htri_t bval;
    bval = H5Tdetect_class((hid_t)dtype_id, (H5T_class_t)dtype_class);
    if (bval > 0) {
        return JNI_TRUE;
    } else if (bval == 0) {
        return JNI_FALSE;
    } else {
        h5libraryError(env);
        return JNI_FALSE;
    }
}


#ifdef __cplusplus
}
#endif
