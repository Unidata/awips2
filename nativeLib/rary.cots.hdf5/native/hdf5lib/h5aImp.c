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
 *  Attribute API Functions of the HDF5 library.
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
#include "h5util.h"
#include <jni.h>
#include <stdlib.h>
#include <string.h>

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
extern jboolean h5badArgument( JNIEnv *env, char *functName);
extern jboolean h5libraryError( JNIEnv *env );

herr_t H5AreadVL_str (JNIEnv *env, hid_t aid, hid_t tid, jobjectArray buf);
herr_t H5AreadVL_num (JNIEnv *env, hid_t aid, hid_t tid, jobjectArray buf);
herr_t H5AreadVL_comp (JNIEnv *env, hid_t aid, hid_t tid, jobjectArray buf);


/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Acreate
 * Signature: (ILjava/lang/String;III)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Acreate
  (JNIEnv *env, jclass clss, jint loc_id, jstring name, jint type_id,
  jint space_id, jint create_plist)
{
    hid_t status;
    char* aName;
    jboolean isCopy;

    if (name == NULL) {
        h5nullArgument( env, "H5Acreate:  name is NULL");
        return -1;
    }

    aName = (char *)ENVPTR->GetStringUTFChars(ENVPAR name, &isCopy);

    if (aName == NULL) {
        h5JNIFatalError( env, "H5Acreate: aName is not pinned");
        return -1;
    }

    status = H5Acreate((hid_t)loc_id, aName, (hid_t)type_id,
        (hid_t)space_id, (hid_t)create_plist );

    ENVPTR->ReleaseStringUTFChars(ENVPAR name,aName);

    if (status < 0) {
        h5libraryError(env);
    }
    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Aopen_name
 * Signature: (ILjava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Aopen_1name
  (JNIEnv *env, jclass clss, jint loc_id, jstring name)
{
    hid_t status;
    char* aName;
    jboolean isCopy;

    if (name == NULL) {
        h5nullArgument( env,"H5Aopen_name:  name is NULL");
        return -1;
    }

    aName = (char *)ENVPTR->GetStringUTFChars(ENVPAR name,&isCopy);

    if (aName == NULL) {
        h5JNIFatalError( env,"H5Aopen_name: name is not pinned");
        return -1;
    }

    status = H5Aopen_name((hid_t)loc_id, aName);

    ENVPTR->ReleaseStringUTFChars(ENVPAR name,aName);

    if (status < 0) {
        h5libraryError(env);
    }
    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Aopen_idx
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Aopen_1idx
  (JNIEnv *env, jclass clss, jint loc_id, jint idx)
{
    hid_t retVal = -1;
    retVal =  H5Aopen_idx((hid_t)loc_id, (unsigned int) idx );
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Awrite
 * Signature: (II[B)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Awrite
  (JNIEnv *env, jclass clss, jint attr_id, jint mem_type_id, jbyteArray buf)
{
    herr_t status;
    jbyte *byteP;
    jboolean isCopy;

    if (buf == NULL) {
        h5nullArgument( env,"H5Awrite:  buf is NULL");
        return -1;
    }

    byteP = ENVPTR->GetByteArrayElements(ENVPAR buf,&isCopy);

    if (byteP == NULL) {
        h5JNIFatalError( env,"H5Awrite: buf is not pinned");
        return -1;
    }
    status = H5Awrite((hid_t)attr_id, (hid_t)mem_type_id, byteP);

    ENVPTR->ReleaseByteArrayElements(ENVPAR buf, byteP,JNI_ABORT);

    if (status < 0) {
        h5libraryError(env);
    }
    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Aread
 * Signature: (II[B)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Aread
  (JNIEnv *env, jclass clss, jint attr_id, jint mem_type_id, jbyteArray buf)
{
    herr_t status;
    jbyte *byteP;
    jboolean isCopy;

    if (buf == NULL) {
        h5nullArgument( env,"H5Aread:  buf is NULL");
        return -1;
    }

    byteP = ENVPTR->GetByteArrayElements(ENVPAR buf,&isCopy);

    if (byteP == NULL) {
        h5JNIFatalError( env,"H5Aread: buf is not pinned");
        return -1;
    }

    status = H5Aread((hid_t)attr_id, (hid_t)mem_type_id, byteP);

    if (status < 0) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR buf,byteP,JNI_ABORT);
        h5libraryError(env);
    } else  {

        ENVPTR->ReleaseByteArrayElements(ENVPAR buf,byteP,0);
    }

    return (jint)status;

}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Aget_space
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Aget_1space
  (JNIEnv *env, jclass clss, jint attr_id)
{
    hid_t retVal = -1;
    retVal =  H5Aget_space((hid_t)attr_id);
    if (retVal < 0) {
        /* throw exception */
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Aget_type
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Aget_1type
  (JNIEnv *env, jclass clss, jint attr_id)
{
    hid_t retVal = -1;
    retVal =  H5Aget_type((hid_t)attr_id);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Aget_name
 * Signature: (IJLjava/lang/String;)J
 */
JNIEXPORT jlong JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Aget_1name
  (JNIEnv *env, jclass clss, jint attr_id, jlong buf_size, jobjectArray name)
{
    char *aName;
    jstring str;
    hssize_t size;
    long bs;

    bs = (long)buf_size;
    if (bs <= 0) {
        h5badArgument( env, "H5Aget_name:  buf_size <= 0");
        return -1;
    }
    aName = (char*)malloc(sizeof(char)*bs);
    if (aName == NULL) {
        h5outOfMemory( env, "H5Aget_name:  malloc failed");
        return -1;
    }
    size = H5Aget_name((hid_t)attr_id, (size_t)buf_size, aName);
    if (size < 0) {
        free(aName);
        h5libraryError(env);
        return -1;
        /*  exception, returns immediately */
    }
    /* successful return -- save the string; */

    str = ENVPTR->NewStringUTF(ENVPAR aName);

    if (str == NULL) {
        free(aName);
        h5JNIFatalError( env,"H5Aget_name:  return string failed");
        return -1;
    }
    free(aName);
    /*  Note: throws ArrayIndexOutOfBoundsException,
        ArrayStoreException */

    ENVPTR->SetObjectArrayElement(ENVPAR name,0,str);

    return (jlong)size;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Aget_num_attrs
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Aget_1num_1attrs
  (JNIEnv *env, jclass clss, jint loc_id)
{
    int retVal = -1;
    retVal =  H5Aget_num_attrs((hid_t)loc_id);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Adelete
 * Signature: (ILjava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Adelete
  (JNIEnv *env, jclass clss, jint loc_id, jstring name)
{
    herr_t status;
    char* aName;
    jboolean isCopy;

    if (name == NULL) {
        h5nullArgument( env,"H5Adelete:  name is NULL");
        return -1;
    }

    aName = (char *)ENVPTR->GetStringUTFChars(ENVPAR name,&isCopy);

    if (aName == NULL) {
        h5JNIFatalError( env,"H5Adelete: name is not pinned");
        return -1;
    }

    status = H5Adelete((hid_t)loc_id, aName );

    ENVPTR->ReleaseStringUTFChars(ENVPAR name,aName);

    if (status < 0) {
        h5libraryError(env);
    }
    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Aclose
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Aclose
  (JNIEnv *env, jclass clss, jint attr_id)
{
    herr_t retVal = 0;

    if (attr_id > 0)
        retVal =  H5Aclose((hid_t)attr_id);

    if (retVal < 0) {
        h5libraryError(env);
    }

    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Aread
 * Signature: (II[B)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5AreadVL
  (JNIEnv *env, jclass clss, jint attr_id, jint mem_type_id, jobjectArray buf)
{
    htri_t isStr;

    if ( buf == NULL ) {
        h5nullArgument( env, "H5AreadVL:  buf is NULL");
        return -1;
    }

    isStr = H5Tis_variable_str((hid_t)mem_type_id);

    if (H5Tis_variable_str((hid_t)mem_type_id) > 0)
    {
        return (jint) H5AreadVL_str (env, (hid_t)attr_id, (hid_t)mem_type_id, buf);
    }
    else if (H5Tget_class((hid_t)mem_type_id) == H5T_COMPOUND)
    {
        return (jint) H5AreadVL_comp (env, (hid_t)attr_id, (hid_t)mem_type_id, buf);
    }
    else
    {
        return (jint) H5AreadVL_num (env, (hid_t)attr_id, (hid_t)mem_type_id, buf);
    }
}

herr_t H5AreadVL_num (JNIEnv *env, hid_t aid, hid_t tid, jobjectArray buf)
{
    herr_t status;
    int i, n;
    size_t max_len=0;
    h5str_t h5str;
    jstring jstr;
    hvl_t *rdata=NULL;
    size_t size;
    hid_t sid;
    hsize_t dims[H5S_MAX_RANK];

    n = ENVPTR->GetArrayLength(ENVPAR buf);
    rdata = (hvl_t *)calloc(n, sizeof(hvl_t));
    if (rdata == NULL) {
        h5JNIFatalError( env, "H5AreadVL:  failed to allocate buff for read");
        return -1;
    }

    status = H5Aread(aid, tid, rdata);
    dims[0] = n;
    sid = H5Screate_simple(1, dims, NULL);

    if (status < 0) {
        H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, rdata);
        H5Sclose(sid);
        free(rdata);
        h5JNIFatalError(env, "H5AreadVL: failed to read data");
        return -1;
    }

    for (i=0; i<n; i++)
    {
        if ((rdata+i)->len > max_len)
            max_len = (rdata+i)->len;
    }

    size = H5Tget_size(tid);
    memset((void *)&h5str, (int)0, (size_t)sizeof(h5str_t));
    h5str_new(&h5str, 4*size);

    if (h5str.s == NULL)
    {
        H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, rdata);
        H5Sclose(sid);
        free(rdata);
        h5JNIFatalError( env, "H5AreadVL:  failed to allocate strng buf");
        return -1;
    }

    for (i=0; i<n; i++)
    {
        h5str.s[0] = '\0';
        h5str_sprintf(&h5str, aid, tid, rdata+i);
        jstr = ENVPTR->NewStringUTF(ENVPAR h5str.s);
        ENVPTR->SetObjectArrayElement(ENVPAR buf, i, jstr);
    }

    h5str_free(&h5str); 
    H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, rdata);
    H5Sclose(sid);

    if (rdata)
        free(rdata);

    return status;
}

herr_t H5AreadVL_comp (JNIEnv *env, hid_t aid, hid_t tid, jobjectArray buf)
{
    herr_t status;
    int i, n;
    size_t max_len=0;
    h5str_t h5str;
    jstring jstr;
    char *rdata;
    size_t size;

    size = H5Tget_size(tid);
    n = ENVPTR->GetArrayLength(ENVPAR buf);
    rdata = (char *)malloc(n*size);

    if (rdata == NULL) {
        h5JNIFatalError( env, "H5AreadVL:  failed to allocate buff for read");
        return -1;
    }

    status = H5Aread(aid, tid, rdata);

    if (status < 0) {
        free(rdata);
        h5JNIFatalError(env, "H5AreadVL: failed to read data");
        return -1;
    }

    memset(&h5str, 0, sizeof(h5str_t));
    h5str_new(&h5str, 4*size);

    if (h5str.s == NULL)
    {
        free(rdata);
        h5JNIFatalError( env, "H5AreadVL:  failed to allocate strng buf");
        return -1;
    }

    for (i=0; i<n; i++)
    {
        h5str.s[0] = '\0';
        h5str_sprintf(&h5str, aid, tid, rdata+i*size);
        jstr = ENVPTR->NewStringUTF(ENVPAR h5str.s);
        ENVPTR->SetObjectArrayElement(ENVPAR buf, i, jstr);
    }

    h5str_free(&h5str); 
    if (rdata) free(rdata);

    return status;
}

herr_t H5AreadVL_str (JNIEnv *env, hid_t aid, hid_t tid, jobjectArray buf)
{
    herr_t status=-1;
    jstring jstr;
    char **strs;
    int i, n;
    hid_t sid;
    hsize_t dims[H5S_MAX_RANK];

    n = ENVPTR->GetArrayLength(ENVPAR buf);
    strs =(char **)malloc(n*sizeof(char *));

    if (strs == NULL)
    {
        h5JNIFatalError( env, "H5AreadVL:  failed to allocate buff for read variable length strings");
        return -1;
    }

    status = H5Aread(aid, tid, strs);
    dims[0] = n;
    sid = H5Screate_simple(1, dims, NULL);

    if (status < 0) {
        H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, strs);
        H5Sclose(sid);
        free(strs);
        h5JNIFatalError(env, "H5AreadVL: failed to read variable length strings");
        return -1;
    }

    for (i=0; i<n; i++)
    {
        jstr = ENVPTR->NewStringUTF(ENVPAR strs[i]);
        ENVPTR->SetObjectArrayElement(ENVPAR buf, i, jstr);
    }
        
    /*H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, strs);*/
    H5Sclose(sid);

    for (i=0; i<n; i++) {
        if (strs[i]) 
            free(strs[i]);
    }

    if (strs)
        free(strs);

    return status;
}

/*
 * Copies the content of one dataset to another dataset
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Acopy
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Acopy
  (JNIEnv *env, jclass clss, jint src_id, jint dst_id)
{
    jbyte *buf;
    herr_t retVal = -1;
    hid_t src_did = (hid_t)src_id;
    hid_t dst_did = (hid_t)dst_id;
    hid_t tid=-1;
    hid_t sid=-1;
    hsize_t total_size = 0;


    sid = H5Aget_space(src_did);
    if (sid < 0) {
        h5libraryError(env);
        return -1;
    }

    tid = H5Aget_type(src_did);
    if (tid < 0) {
        H5Sclose(sid);
        h5libraryError(env);
        return -1;
    }

    total_size = H5Sget_simple_extent_npoints(sid) * H5Tget_size(tid);

    H5Sclose(sid);

    buf = (jbyte *)malloc( (int) (total_size * sizeof(jbyte)));
    if (buf == NULL) {
    H5Tclose(tid);
        h5outOfMemory( env, "H5Acopy:  malloc failed");
        return -1;
    }

    retVal = H5Aread(src_did, tid, buf);
    H5Tclose(tid);

    if (retVal < 0) {
        free(buf);
        h5libraryError(env);
        return (jint)retVal;
    }

    tid = H5Aget_type(dst_did);
    if (tid < 0) {
        free(buf);
        h5libraryError(env);
        return -1;
    }
    retVal = H5Awrite(dst_did, tid, buf);
    H5Tclose(tid);
    free(buf);

    if (retVal < 0) {
        h5libraryError(env);
    }

    return (jint)retVal;
}

#ifdef __cplusplus
}
#endif
