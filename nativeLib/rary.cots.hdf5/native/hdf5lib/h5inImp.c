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
 *  Reference API Functions of the HDF5 library.
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

#define GOTO_H5IN_ERROR(error) {h5JNIFatalError(env, error); ret_value=-1; goto done;}

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


JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5INcreate
  (JNIEnv *env, jclass clss,
   jstring grp_name, jint grp_loc_id, jint property_list, jint data_loc_id, 
   jstring data_loc_name, jstring field_name, jlong max_mem_size)
{
    hid_t ret_value;
    char *gname, *dname, *fldname;

    gname = dname = fldname = NULL;

    if (grp_name == NULL)
        GOTO_H5IN_ERROR ("H5INcreate: name of the index group is NULL");

    if ( NULL == (gname = (char *)ENVPTR->GetStringUTFChars(ENVPAR grp_name, NULL)))
        GOTO_H5IN_ERROR ("H5INcreate: name of the index group not pinned");

    if (data_loc_name == NULL)
        GOTO_H5IN_ERROR ("H5INcreate: name of the index dataset is NULL");

    if ( NULL == (dname = (char *)ENVPTR->GetStringUTFChars(ENVPAR  data_loc_name, NULL)))
        GOTO_H5IN_ERROR ("H5INcreate: name of the index dataset not pinned");

    if (field_name != NULL)
    {
        if ( NULL == (fldname = (char *)ENVPTR->GetStringUTFChars(ENVPAR  field_name, NULL)))
            GOTO_H5IN_ERROR ("H5INcreate: name of the index field not pinned");
    }

    ret_value = H5INcreate(gname, (hid_t)grp_loc_id, (hid_t) property_list, 
        (hid_t)data_loc_id, dname, fldname, (hsize_t)max_mem_size);

done:

    if (gname)
        ENVPTR->ReleaseStringUTFChars(ENVPAR  grp_name, gname);

    if (dname)
        ENVPTR->ReleaseStringUTFChars(ENVPAR  data_loc_name, dname);

    if (fldname)
        ENVPTR->ReleaseStringUTFChars(ENVPAR  field_name, fldname);

    return (jint)ret_value;

}


JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5INquery
   (JNIEnv *env, jclass clss,
    jint dset_id, jobjectArray keys, jobject ubounds, jobject lbounds, jint nkeys)
{
    hid_t ret_value;
    char **cKeys;
    int i;
    void *ub, *lb;
    char *upstr, *lowstr;

    cKeys = NULL;
    upstr = lowstr = NULL;

    if (NULL == keys || nkeys <=0)
        GOTO_H5IN_ERROR ("H5INquery: no key to query");

    if (NULL == ubounds || NULL == lbounds)
        GOTO_H5IN_ERROR ("H5INquery: query bound is NULL");

    cKeys = (char **)malloc(nkeys*sizeof (char *));
    memset(cKeys, 0, nkeys*sizeof (char *));
    for (i=0; i<nkeys; i++) {
        jstring theKey;
        char *cstr;
        theKey = (jstring)ENVPTR->GetObjectArrayElement(ENVPAR  keys, i);
        cstr = (char *)ENVPTR->GetStringUTFChars(ENVPAR  theKey, 0);
        cKeys[i] = (char *)malloc(strlen(cstr)+1);
        strcpy(cKeys[i], cstr);
        ENVPTR->ReleaseStringUTFChars(ENVPAR  theKey, cstr);
    }

    if ( ENVPTR->IsInstanceOf(ENVPAR  ubounds, ENVPTR->FindClass(ENVPAR  "[Z")) ) {
        jboolean up, low, *ptr;
        jbooleanArray ja;
        ja =(jbooleanArray)ubounds;
        ptr = ENVPTR->GetBooleanArrayElements(ENVPAR  ja, 0);
        up = ptr[0];
        ub = &up;
        ENVPTR->ReleaseBooleanArrayElements(ENVPAR  ja, ptr, 0);
        ja = (jbooleanArray)lbounds;
        ptr = ENVPTR->GetBooleanArrayElements(ENVPAR  ja, 0);
        low = ptr[0];
        lb = &low;
        ENVPTR->ReleaseBooleanArrayElements(ENVPAR  ja, ptr, 0);
    }
    else if ( ENVPTR->IsInstanceOf(ENVPAR  ubounds, ENVPTR->FindClass(ENVPAR  "[B")) ) {
        jbyte up, low, *ptr;
        jbyteArray ja;
        ja =(jbyteArray)ubounds;
        ptr = ENVPTR->GetByteArrayElements(ENVPAR  ja, 0);
        up = ptr[0];
        ub = &up;
        ENVPTR->ReleaseByteArrayElements(ENVPAR  ja, ptr, 0);
        ja = (jbyteArray)lbounds;
        ptr = ENVPTR->GetByteArrayElements(ENVPAR  ja, 0);
        low = ptr[0];
        lb = &low;
        ENVPTR->ReleaseByteArrayElements(ENVPAR  ja, ptr, 0);
    }
    else if ( ENVPTR->IsInstanceOf(ENVPAR  ubounds, ENVPTR->FindClass(ENVPAR  "[S")) ) {
        jshort up, low, *ptr;
        jshortArray ja;
        ja =(jshortArray)ubounds;
        ptr = ENVPTR->GetShortArrayElements(ENVPAR  ja, 0);
        up = ptr[0];
        ub = &up;
        ENVPTR->ReleaseShortArrayElements(ENVPAR  ja, ptr, 0);
        ja = (jshortArray)lbounds;
        ptr = ENVPTR->GetShortArrayElements(ENVPAR  ja, 0);
        low = ptr[0];
        lb = &low;
        ENVPTR->ReleaseShortArrayElements(ENVPAR  ja, ptr, 0);
    }
    else if ( ENVPTR->IsInstanceOf(ENVPAR  ubounds, ENVPTR->FindClass(ENVPAR  "[I")) ) {
        jint up, low, *ptr;
        jintArray ja;
        ja =(jintArray)ubounds;
        ptr = ENVPTR->GetIntArrayElements(ENVPAR  ja, 0);
        up = ptr[0];
        ub = &up;
        ENVPTR->ReleaseIntArrayElements(ENVPAR  ja, ptr, 0);
        ja = (jintArray)lbounds;
        ptr = ENVPTR->GetIntArrayElements(ENVPAR  ja, 0);
        low = ptr[0];
        lb = &low;
        ENVPTR->ReleaseIntArrayElements(ENVPAR  ja, ptr, 0);
    }
    else if ( ENVPTR->IsInstanceOf(ENVPAR  ubounds, ENVPTR->FindClass(ENVPAR  "[J")) ) {
        jlong up, low, *ptr;
        jlongArray ja;
        ja =(jlongArray)ubounds;
        ptr = ENVPTR->GetLongArrayElements(ENVPAR  ja, 0);
        up = ptr[0];
        ub = &up;
        ENVPTR->ReleaseLongArrayElements(ENVPAR  ja, ptr, 0);
        ja = (jlongArray)lbounds;
        ptr = ENVPTR->GetLongArrayElements(ENVPAR  ja, 0);
        low = ptr[0];
        lb = &low;
        ENVPTR->ReleaseLongArrayElements(ENVPAR  ja, ptr, 0);
    }
    else if ( ENVPTR->IsInstanceOf(ENVPAR  ubounds, ENVPTR->FindClass(ENVPAR  "[F")) ) {
        jfloat up, low, *ptr;
        jfloatArray ja;
        ja =(jfloatArray)ubounds;
        ptr = ENVPTR->GetFloatArrayElements(ENVPAR  ja, 0);
        up = ptr[0];
        ub = &up;
        ENVPTR->ReleaseFloatArrayElements(ENVPAR  ja, ptr, 0);
        ja = (jfloatArray)lbounds;
        ptr = ENVPTR->GetFloatArrayElements(ENVPAR  ja, 0);
        low = ptr[0];
        lb = &low;
        ENVPTR->ReleaseFloatArrayElements(ENVPAR  ja, ptr, 0);
    }
    else if ( ENVPTR->IsInstanceOf(ENVPAR  ubounds, ENVPTR->FindClass(ENVPAR  "[D")) ) {
        jdouble up, low, *ptr;
        jdoubleArray ja;
        ja =(jdoubleArray)ubounds;
        ptr = ENVPTR->GetDoubleArrayElements(ENVPAR  ja, 0);
        up = ptr[0];
        ub = &up;
        ENVPTR->ReleaseDoubleArrayElements(ENVPAR  ja, ptr, 0);
        ja = (jdoubleArray)lbounds;
        ptr = ENVPTR->GetDoubleArrayElements(ENVPAR  ja, 0);
        low = ptr[0];
        lb = &low;
        ENVPTR->ReleaseDoubleArrayElements(ENVPAR  ja, ptr, 0);
    }
    else if ( ENVPTR->IsInstanceOf(ENVPAR  ubounds, ENVPTR->FindClass(ENVPAR  "java/lang/String;")) ) {
        jstring jstr;
        char *cstr;
        jstr = (jstring)ubounds;
        cstr = (char *)ENVPTR->GetStringUTFChars(ENVPAR  jstr, 0);
        upstr = (char *)malloc(strlen(cstr)+1);
        strcpy(upstr, cstr);
        ENVPTR->ReleaseStringUTFChars(ENVPAR  jstr, cstr);
        jstr = (jstring)lbounds;
        cstr = (char *)ENVPTR->GetStringUTFChars(ENVPAR  jstr, 0);
        lowstr = (char *)malloc(strlen(cstr)+1);
        strcpy(lowstr, cstr);
        ENVPTR->ReleaseStringUTFChars(ENVPAR  jstr, cstr);

        ub = upstr;
        lb = lowstr;
    }

    ret_value = H5INquery( (hid_t)dset_id, cKeys, ub, lb, (int)nkeys);

done:

    if (cKeys) {
        for (i=0; i<nkeys; i++) {
            if (cKeys[i])
                free(cKeys[i]);
        }
        free(cKeys);
    }

    if (upstr)
        free( upstr);

    if (lowstr)
        free(lowstr);

    return (jint)ret_value;
}


#ifdef __cplusplus
}
#endif
