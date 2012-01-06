/****************************************************************************
    if (strideP != NULL) {
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
 *  Dataspace Object API Functions of the HDF5 library.
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
#include <sys/time.h>

#ifdef __cplusplus
#define ENVPTR (env)
#define ENVPAR 
#else
#define ENVPTR (*env)
#define ENVPAR env,
#endif

extern jboolean h5JNIFatalError( JNIEnv *env, char *functName);
extern jboolean h5nullArgument( JNIEnv *env, char *functName);
extern jboolean h5libraryError( JNIEnv *env );
extern jboolean h5badArgument( JNIEnv *env, char *functName);

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Screate
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Screate
  (JNIEnv *env, jclass clss, jint type)
{
    hid_t retVal = -1;
    retVal =  H5Screate((H5S_class_t)type);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Screate_simple
 * Signature: (I[B[B)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Screate_1simple
  (JNIEnv *env, jclass clss, jint rank, jbyteArray dims, jbyteArray maxdims)
{
    hid_t status;
    jbyte *dimsP, *maxdimsP;
    jboolean isCopy;
    hsize_t *sa=NULL;
    hsize_t *msa=NULL;
    int i;
    hsize_t *lp;
    jlong *jlp;

    if (rank <= 0) {
        h5badArgument( env, "H5Screate_simple:  rank <=0");
        return -1;
    }

    if (dims == NULL) {
        h5nullArgument( env, "H5Screate_simple:  dims is NULL");
        return -1;
    }
    dimsP = ENVPTR->GetByteArrayElements(ENVPAR dims,&isCopy);

    if (dimsP == NULL) {
        h5JNIFatalError(env,  "H5Screate-simple:  dims not pinned");
        return -1;
    }
    sa = lp = (hsize_t *)malloc(rank * sizeof(hsize_t));
    if (sa == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR dims,dimsP,JNI_ABORT);

        h5JNIFatalError(env,  "H5Screate-simple:  dims not converted to hsize_t");
        return -1;
    }
    jlp = (jlong *)dimsP;
    for (i = 0; i < rank; i++) {
        *lp = (hsize_t)*jlp;
        lp++;
        jlp++;
    }

    if (maxdims == NULL) {
        maxdimsP = NULL;
        msa = (hsize_t *)maxdimsP;
    } else
    {
        maxdimsP = ENVPTR->GetByteArrayElements(ENVPAR maxdims,&isCopy);
        if (maxdimsP == NULL)  {
            ENVPTR->ReleaseByteArrayElements(ENVPAR dims,dimsP,JNI_ABORT);
            h5JNIFatalError(env,  "H5Screate-simple:  maxdims not pinned");
            return -1;
        }
        msa = lp = (hsize_t *)malloc(rank * sizeof(hsize_t));
        if (msa == NULL) {
            ENVPTR->ReleaseByteArrayElements(ENVPAR dims,dimsP,JNI_ABORT);
            ENVPTR->ReleaseByteArrayElements(ENVPAR maxdims,maxdimsP,JNI_ABORT);
            free (sa);
            h5JNIFatalError(env,  "H5Screate-simple:  dims not converted to hsize_t");
            return -1;
        }
    jlp = (jlong *)maxdimsP;
    for (i = 0; i < rank; i++) {
        *lp = (hsize_t)*jlp;
        lp++;
        jlp++;
    }
    }

    status = H5Screate_simple(rank, (const hsize_t *)sa, (const hsize_t *)msa);
    if (maxdimsP != NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR maxdims,maxdimsP,JNI_ABORT);
        if (msa) free (msa);
    }

    ENVPTR->ReleaseByteArrayElements(ENVPAR dims,dimsP,0);

    if (sa) free (sa);

    if (status < 0) {
        h5libraryError(env);
    }
    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Scopy
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Scopy
  (JNIEnv *env, jclass clss, jint space_id)
{
    hid_t retVal = -1;
    retVal =  H5Scopy (space_id);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Sselect_elements
 * Signature: (III[J)I
 */

#ifdef notdef
// 10/28/99 -- added code to copy the array -- this is not used,
// but serves as a reminder in case we try to implement this in
// the future....
/*
 *  Note:  the argument coord is actually long coord[][], which has been
 *         flattened by the caller.
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sselect_1elements
  (JNIEnv *env, jclass clss, jint space_id, jint op, jint num_elemn, jlongArray coord)
{
    herr_t status;
    jint i;
    jlong *P;
    jboolean isCopy;
    hssize_t *sa;
    int rank;

    if (coord == NULL) {
        h5nullArgument( env, "H5Sselect_elements:  coord is NULL");
        return -1;
    }

    P = ENVPTR->GetLongArrayElements(ENVPAR env,coord,&isCopy);
    if (P == NULL) {
        h5JNIFatalError(env,  "H5Sselect_elements:  coord not pinned");
        return -1;
    }
    sa = (hssize_t *)malloc( num_elems * 2 * sizeof(hssize_t));
    if (sa == NULL) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR env,coord,P,JNI_ABORT);
        h5JNIFatalError(env,  "H5Sselect_elements:  coord array not converted to hssize_t");
        return -1;
    }
    for (i= 0; i < (num_elsms * 2); i++) {
        sa[i] = P[i];
    }

    status = H5Sselect_elements (space_id, (H5S_seloper_t)op, num_elemn, (const hssize_t **)&sa);
    ENVPTR->ReleaseLongArrayElements(ENVPAR env, coord, P, 0);
    free(sa);

    if (status < 0) {
        h5libraryError(env);
    }
    return (jint)status;
}
#endif

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sselect_1elements
  (JNIEnv *env, jclass clss, jint space_id, jint op, jint num_elemn, jbyteArray coord)
{
    int ii;
    hsize_t *lp=NULL;
    hsize_t *llp;
    jlong *jlp;
    herr_t status;
    jbyte *P;
    jboolean isCopy;
    jsize size;
    int nlongs;

    if (coord == NULL) {
        h5nullArgument( env, "H5Sselect_elements:  coord is NULL");
        return -1;
    }

    P = ENVPTR->GetByteArrayElements(ENVPAR coord,&isCopy);
    if (P == NULL) {
        h5JNIFatalError(env,  "H5Sselect_elements:  coord not pinned");
        return -1;
    }
    size = (int) ENVPTR->GetArrayLength(ENVPAR coord);
    nlongs = size / sizeof(jlong);
    lp = (hsize_t *)malloc(nlongs * sizeof(hsize_t));
    jlp = (jlong *)P;
    llp = lp;
    for (ii = 0; ii < nlongs; ii++) {
        *lp = (hsize_t)*jlp;
        lp++;
        jlp++;
    }

    status = H5Sselect_elements (space_id, (H5S_seloper_t)op, num_elemn, (const hsize_t *)llp);

    ENVPTR->ReleaseByteArrayElements(ENVPAR coord, P, 0);

    if (llp) free (llp);

    if (status < 0) {
        h5libraryError(env);
    }

    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Sselect_all
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sselect_1all
  (JNIEnv *env, jclass clss, jint space_id)
{
    herr_t retVal = -1;
    retVal =  H5Sselect_all(space_id);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Sselect_none
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sselect_1none
  (JNIEnv *env, jclass clss, jint space_id)
{
    herr_t retVal = -1;
    retVal =  H5Sselect_none(space_id);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Sselect_valid
 * Signature: (I)B
 */
JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sselect_1valid
  (JNIEnv *env, jclass clss, jint space_id)
{
    htri_t bval;
    bval = H5Sselect_valid(space_id);
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
 * Method:    H5Sget_simple_extent_npoints
 * Signature: (I)J
 */
JNIEXPORT jlong JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sget_1simple_1extent_1npoints
  (JNIEnv *env, jclass clss, jint space_id)
{
    hssize_t retVal = -1;
    retVal =  H5Sget_simple_extent_npoints(space_id);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jlong)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Sget_select_npoints
 * Signature: (I)J
 */
JNIEXPORT jlong JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sget_1select_1npoints
  (JNIEnv *env, jclass clss, jint space_id)
{
    hssize_t retVal = -1;
    retVal =  H5Sget_select_npoints(space_id);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jlong)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Sget_simple_extent_ndims
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sget_1simple_1extent_1ndims
  (JNIEnv *env, jclass clss, jint space_id)
{
    int retVal = -1;
    retVal =  H5Sget_simple_extent_ndims(space_id);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Sget_simple_extent_dims
 * Signature: (I[J[J)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sget_1simple_1extent_1dims
  (JNIEnv *env, jclass clss, jint space_id, jlongArray dims, jlongArray maxdims)
{
    int status;
    jlong *dimsP, *maxdimsP;
    jboolean isCopy;
    hsize_t *sa;
    hsize_t *msa;
    int i;
    int rank;

    if (dims == NULL) {
        h5nullArgument( env, "H5Sget_simple_extent:  dims is NULL");
        return -1;
    }
    dimsP = ENVPTR->GetLongArrayElements(ENVPAR dims,&isCopy);
    if (dimsP == NULL) {
        h5JNIFatalError(env,  "H5Pget_simple_extent:  dims not pinned");
        return -1;
    }
    rank = (int) ENVPTR->GetArrayLength(ENVPAR dims);
    sa = (hsize_t *)malloc( rank * sizeof(hsize_t));
    if (sa == NULL)  {
        ENVPTR->ReleaseLongArrayElements(ENVPAR dims,dimsP,JNI_ABORT);
        h5JNIFatalError(env,  "H5Sget_simple_extent:  dims not converted to hsize_t");
        return -1;
    }
    if (maxdims == NULL) {
        maxdimsP = NULL;
        msa = (hsize_t *)maxdimsP;
    } else {
        maxdimsP = ENVPTR->GetLongArrayElements(ENVPAR maxdims,&isCopy);
        if (maxdimsP == NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR dims,dimsP,JNI_ABORT);
            free(sa);
            h5JNIFatalError(env,  "H5Pget_simple_extent:  maxdims not pinned");
            return -1;
        }
        msa = (hsize_t *)malloc( rank * sizeof(hsize_t));
        if (msa == NULL)  {
            ENVPTR->ReleaseLongArrayElements(ENVPAR dims,dimsP,JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR maxdims,maxdimsP,JNI_ABORT);
            free(sa);
            h5JNIFatalError(env,  "H5Sget_simple_extent:  maxdims not converted to hsize_t");
            return -1;
        }
    }

    status = H5Sget_simple_extent_dims(space_id, (hsize_t *)sa, (hsize_t *)msa);

    if (status < 0) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR dims,dimsP,JNI_ABORT);
        free(sa);
        if (maxdimsP != NULL)  {
            ENVPTR->ReleaseLongArrayElements(ENVPAR maxdims,maxdimsP,JNI_ABORT);
            free(msa);
        }
        h5libraryError(env);
    } else {
        for (i = 0; i < rank; i++) {
            dimsP[i] = sa[i];
        }
        free(sa);
        ENVPTR->ReleaseLongArrayElements(ENVPAR dims,dimsP,0);
        if (maxdimsP != NULL) {
            for (i = 0; i < rank; i++) {
                maxdimsP[i] = msa[i];
            }
            free(msa);
            ENVPTR->ReleaseLongArrayElements(ENVPAR maxdims,maxdimsP,0);
        }
    }

    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Sget_simple_extent_type
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sget_1simple_1extent_1type
  (JNIEnv *env, jclass clss, jint space_id)
{
    H5S_class_t retVal = H5S_NO_CLASS;
    retVal =  H5Sget_simple_extent_type(space_id);
    if (retVal == H5S_NO_CLASS) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Sset_extent_simple
 * Signature: (II[B[B)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sset_1extent_1simple
  (JNIEnv *env, jclass clss, jint space_id, jint rank, jbyteArray dims, jbyteArray maxdims)
{
    herr_t status;
    jbyte *dimsP, *maxdimsP;
    jboolean isCopy;
    hsize_t *sa;
    hsize_t *msa;
    int i;
    hsize_t *lp;
    jlong *jlp;

    if (dims == NULL) {
        h5nullArgument( env, "H5Sset_simple_extent:  dims is NULL");
        return -1;
    }
    dimsP = ENVPTR->GetByteArrayElements(ENVPAR dims,&isCopy);
    if (dimsP == NULL) {
        h5JNIFatalError(env,  "H5Pset_simple_extent:  dims not pinned");
        return -1;
    }
    sa = lp = (hsize_t *)malloc(rank * sizeof(hsize_t));
    if (sa == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR dims,dimsP,JNI_ABORT);
        h5JNIFatalError(env,  "H5Sset_simple_extent:  dims not converted to hsize_t");
        return -1;
    }
    jlp = (jlong *)dimsP;
    for (i = 0; i < rank; i++) {
        *lp = (hsize_t)*jlp;
        lp++;
        jlp++;
    }
    if (maxdims == NULL) {
        maxdimsP = NULL;
        msa = (hsize_t *)maxdimsP;
    } else {
        maxdimsP = ENVPTR->GetByteArrayElements(ENVPAR maxdims,&isCopy);
        if (maxdimsP == NULL) {
            ENVPTR->ReleaseByteArrayElements(ENVPAR dims,dimsP,JNI_ABORT);
            h5JNIFatalError(env,  "H5Pset_simple_extent:  maxdims not pinned");
            return -1;
        }
        msa = lp = (hsize_t *)malloc(rank * sizeof(hsize_t));
        if (msa == NULL) {
            ENVPTR->ReleaseByteArrayElements(ENVPAR dims,dimsP,JNI_ABORT);
            ENVPTR->ReleaseByteArrayElements(ENVPAR maxdims,maxdimsP,JNI_ABORT);
            free (sa);
            h5JNIFatalError(env,  "H5Sset_simple_extent:  maxdims not converted to hsize_t");
            return -1;
        }
        jlp = (jlong *)maxdimsP;
        for (i = 0; i < rank; i++) {
            *lp = (hsize_t)*jlp;
            lp++;
            jlp++;
        }
    }

    status = H5Sset_extent_simple(space_id, rank, (hsize_t *)sa, (hsize_t *)msa);

    ENVPTR->ReleaseByteArrayElements(ENVPAR dims,dimsP,JNI_ABORT);
    free (sa);
    if (maxdimsP != NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR maxdims,maxdimsP,JNI_ABORT);
        free (msa);
    }

    if (status < 0) {
        h5libraryError(env);
    }
    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Sis_simple
 * Signature: (I)J
 */
JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sis_1simple
  (JNIEnv *env, jclass clss, jint space_id)
{
    htri_t bval;
    bval = H5Sis_simple(space_id);
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
 * Method:    H5Soffset_simple
 * Signature: (I[B)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Soffset_1simple
  (JNIEnv *env, jclass clss, jint space_id, jbyteArray offset)
{
    herr_t status;
    jbyte *P = NULL;
    jboolean isCopy;
    hssize_t *sa;
    int rank;
    int i;
    hssize_t *lp;
    jlong *jlp;

    if (offset != NULL) {
        P = ENVPTR->GetByteArrayElements(ENVPAR offset,&isCopy);
        if (P == NULL) {
            h5JNIFatalError(env,  "H5Soffset_simple:  offset not pinned");
            return -1;
        }
        i = (int) ENVPTR->GetArrayLength(ENVPAR offset);
        rank = i / sizeof(jlong);
        sa = lp = (hssize_t *)malloc(rank * sizeof(hssize_t));
        if (sa == NULL) {
            ENVPTR->ReleaseByteArrayElements(ENVPAR offset,P,JNI_ABORT);
            h5JNIFatalError(env,  "H5Soffset_simple:  offset not converted to hssize_t");
            return -1;
        }
        jlp = (jlong *)P;
        for (i = 0; i < rank; i++) {
            *lp = (hssize_t)*jlp;
            lp++;
            jlp++;
        }
    } else {
        P = NULL;
        sa = (hssize_t *)P;
    }

    status = H5Soffset_simple(space_id, sa);
    if (P != NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR offset,P,JNI_ABORT);
        free(sa);
    }

    if (status < 0) {
        h5libraryError(env);
    }
    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Sextent_copy
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sextent_1copy
  (JNIEnv *env, jclass clss, jint space_id, jint src_id)
{
    herr_t retVal = -1;
    retVal =  H5Sextent_copy(space_id, src_id);
    if (retVal < 0) {
        h5libraryError(env);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Sset_extent_none
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sset_1extent_1none
  (JNIEnv *env, jclass clss, jint space_id)
{
    herr_t retVal = -1;
    retVal =  H5Sset_extent_none(space_id);
    if (retVal < 0) {
        /* throw exception */
        h5libraryError(env);
    }
    return (jint)retVal;
}

static long long tSpent;

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Sselect_hyperslab
 * Signature: (II[B[B[B[B)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sselect_1hyperslab
  (JNIEnv *env, jclass clss, jint space_id, jint op,
  jbyteArray start, jbyteArray stride, jbyteArray count, jbyteArray block)
{
    struct timeval tv;
    long long t0, t1;
    herr_t status;
    jbyte *startP, *strideP, *countP, *blockP;
    jboolean isCopy;
    hsize_t *strt;
    hsize_t *strd;
    hsize_t *cnt;
    hsize_t *blk;
    int rank;
    int i;
    hsize_t *lp;
    jlong *jlp;

    if (start == NULL) {
        h5nullArgument( env, "H5Sselect_hyperslab:  start is NULL");
        return -1;
    }
    if (count == NULL) {
        h5nullArgument( env, "H5Sselect_hyperslab:  count is NULL");
        return -1;
    }
    i = (int)ENVPTR->GetArrayLength(ENVPAR start);
    if (i != ENVPTR->GetArrayLength(ENVPAR count)) {
        h5badArgument( env, "H5Sselect_hyperslab:  count and start have different rank!");
        return -1;
    }
    rank = i / sizeof(jlong);

    startP = ENVPTR->GetByteArrayElements(ENVPAR start,&isCopy);
    if (startP == NULL) {
        h5JNIFatalError(env,  "H5Sselect_hyperslab:  start not pinned");
        return -1;
    }
    strt = lp = (hsize_t *)malloc(rank * sizeof(hsize_t));
    if (strt == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR start,startP,JNI_ABORT);
        h5JNIFatalError(env,  "H5Sselect_hyperslab:  start not converted to hsize_t");
        return -1;
    }
    jlp = (jlong *)startP;
    for (i = 0; i < rank; i++) {
        *lp = (hsize_t)*jlp;
        lp++;
        jlp++;
    }
    countP = ENVPTR->GetByteArrayElements(ENVPAR count,&isCopy);
    if (countP == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR start, startP,JNI_ABORT);
        free(strt);
        h5JNIFatalError(env,  "H5Sselect_hyperslab:  count not pinned");
        return -1;
    }
    cnt = lp = (hsize_t *)malloc(rank * sizeof(hsize_t));
    if (cnt == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR start, startP,JNI_ABORT);
        ENVPTR->ReleaseByteArrayElements(ENVPAR count, countP,JNI_ABORT);
        free(strt);
        h5JNIFatalError(env,  "H5Sselect_hyperslab:  count not converted to hsize_t");
        return -1;
    }
    jlp = (jlong *)countP;
    for (i = 0; i < rank; i++) {
        *lp = (hsize_t)*jlp;
        lp++;
        jlp++;
    }
    if (stride == NULL) {
        strideP = NULL;
        strd = (hsize_t *)strideP;
    } else {
        strideP = ENVPTR->GetByteArrayElements(ENVPAR stride,&isCopy);
        if (strideP == NULL) {
            ENVPTR->ReleaseByteArrayElements(ENVPAR count, countP,JNI_ABORT);
            ENVPTR->ReleaseByteArrayElements(ENVPAR start, startP,JNI_ABORT);
            free(cnt); free(strt);
            h5badArgument( env, "H5Sselect_hyperslab:  stride not pinned");
            return -1;
        }
        strd = lp = (hsize_t *)malloc(rank * sizeof(hsize_t));
        if (strd == NULL) {
            ENVPTR->ReleaseByteArrayElements(ENVPAR count, countP,JNI_ABORT);
            ENVPTR->ReleaseByteArrayElements(ENVPAR start, startP,JNI_ABORT);
            ENVPTR->ReleaseByteArrayElements(ENVPAR stride, strideP,JNI_ABORT);
            free(cnt); free(strt);
            h5JNIFatalError(env,  "H5Sselect_hyperslab:  stride not converted to hsize_t");
            return -1;
        }
        jlp = (jlong *)strideP;
        for (i = 0; i < rank; i++) {
            *lp = (hsize_t)*jlp;
            lp++;
            jlp++;
        }
    }
    if (block == NULL) {
        blockP = NULL;
        blk = (hsize_t *)blockP;
    } else {
        blockP = ENVPTR->GetByteArrayElements(ENVPAR block,&isCopy);
        if (blockP == NULL)  {
            ENVPTR->ReleaseByteArrayElements(ENVPAR stride, strideP,JNI_ABORT);
            ENVPTR->ReleaseByteArrayElements(ENVPAR count, countP,JNI_ABORT);
            ENVPTR->ReleaseByteArrayElements(ENVPAR start, startP,JNI_ABORT);
            free(cnt); free(strt);
            if (strd != NULL) { free(strd); }
            h5JNIFatalError(env,  "H5Sselect_hyperslab:  block not pinned");
            return -1;
        }
        blk = lp = (hsize_t *)malloc(rank * sizeof(hsize_t));
        if (blk == NULL) {
            ENVPTR->ReleaseByteArrayElements(ENVPAR stride, strideP,JNI_ABORT);
            ENVPTR->ReleaseByteArrayElements(ENVPAR count, countP,JNI_ABORT);
            ENVPTR->ReleaseByteArrayElements(ENVPAR start, startP,JNI_ABORT);
            ENVPTR->ReleaseByteArrayElements(ENVPAR block, blockP,JNI_ABORT);
            free(cnt); free(strt);
            if (strd != NULL) { free(strd); }
            h5JNIFatalError(env,  "H5Sget_simple_extent:  block not converted to hsize_t");
            return -1;
        }
        jlp = (jlong *)blockP;
        for (i = 0; i < rank; i++) {
            *lp = (hsize_t)*jlp;
            lp++;
            jlp++;
        }
    }

    status = H5Sselect_hyperslab (space_id, (H5S_seloper_t)op, (const hsize_t *)strt, (const hsize_t *)strd, (const hsize_t *)cnt, (const hsize_t *)blk);


    ENVPTR->ReleaseByteArrayElements(ENVPAR start, startP,0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR count, countP,0);
    free(strt);
    free(cnt);
    if (strideP != NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR stride, strideP,0);
        free(strd);
    }
    if (blockP != NULL)  {
        ENVPTR->ReleaseByteArrayElements(ENVPAR block, blockP,0);
        free(blk);
    }

    if (status < 0) {
        h5libraryError(env);
    }
    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Sclose
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Sclose
  (JNIEnv *env, jclass clss, jint space_id)
{
    herr_t retVal = 0;

    if (space_id > 0)
        retVal =  H5Sclose(space_id);

    if (retVal < 0) {
        h5libraryError(env);
    }

    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Sget_select_hyper_nblocks
 * Signature: (I)J
 */
JNIEXPORT jlong JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sget_1select_1hyper_1nblocks
  (JNIEnv *env, jclass clss, jint spaceid)
{
    hssize_t status;

    status = H5Sget_select_hyper_nblocks((hid_t)spaceid);
    if (status < 0)
        h5libraryError(env);

    return (jlong)status;
}


/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Sget_select_elem_npoints
 * Signature: (I)J
 */
JNIEXPORT jlong JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sget_1select_1elem_1npoints
  (JNIEnv *env, jclass clss, jint spaceid)
{
    hssize_t status;

    status = H5Sget_select_elem_npoints((hid_t)spaceid);
    if (status < 0)
        h5libraryError(env);

    return (jlong)status;
}


/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Sget_select_hyper_blocklist
 * Signature: (IJJ[J)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sget_1select_1hyper_1blocklist
  (JNIEnv *env, jclass clss, jint spaceid, jlong startblock, jlong numblocks, jlongArray buf)
{
    herr_t status;
    jlong *bufP;
    jboolean isCopy;
    hsize_t *ba;
    int i;
    long st;
    long nb;

    st = (long)startblock;
    nb = (long)numblocks;

    if ( buf == NULL ) {
        h5nullArgument( env, "H5Sget_select_hyper_blocklist:  buf is NULL");
        return -1;
    }
    bufP = ENVPTR->GetLongArrayElements(ENVPAR buf,&isCopy);
    if (bufP == NULL) {
        h5JNIFatalError( env, "H5Sget_select_hyper_blocklist:  buf not pinned");
        return -1;
    }
    ba = (hsize_t *)malloc( nb * 2 * sizeof(hsize_t));
    if (ba == NULL) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR buf, bufP,JNI_ABORT);
        h5JNIFatalError(env,  "H5Screate-simple:  buffer not converted to hsize_t");
        return -1;
    }

    status = H5Sget_select_hyper_blocklist((hid_t)spaceid, (hsize_t)st,
        (hsize_t)nb, (hsize_t *)ba);

    if (status < 0) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR buf,bufP,JNI_ABORT);
        free (ba);
        h5libraryError(env);
    } else  {
        for (i = 0; i < (numblocks*2); i++) {
            bufP[i] = ba[i];
        }
        free (ba);
        ENVPTR->ReleaseLongArrayElements(ENVPAR buf,bufP,0);
    }

    return (jint)status;
}


/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Sget_select_elem_pointlist
 * Signature: (IJJ[J)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sget_1select_1elem_1pointlist
  (JNIEnv *env, jclass clss, jint spaceid, jlong startpoint, jlong numpoints, jlongArray buf)
{
    herr_t status;
    jlong *bufP;
    jboolean isCopy;
    hsize_t *ba;
    int i;

    if ( buf == NULL ) {
        h5nullArgument( env, "H5Sget_select_elem_pointlist:  buf is NULL");
        return -1;
    }
    bufP = ENVPTR->GetLongArrayElements(ENVPAR buf,&isCopy);
    if (bufP == NULL) {
        h5JNIFatalError( env, "H5Sget_select_elem_pointlist:  buf not pinned");
        return -1;
    }
    ba = (hsize_t *)malloc( ((long)numpoints) * sizeof(hsize_t));
    if (ba == NULL) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR buf,bufP,JNI_ABORT);
        h5JNIFatalError(env,  "H5Sget_select_elem_pointlist:  buf not converted to hsize_t");
        return -1;
    }

    status = H5Sget_select_elem_pointlist((hid_t)spaceid, (hsize_t)startpoint,
        (hsize_t)numpoints, (hsize_t *)ba);

    if (status < 0) {
        free (ba);
        ENVPTR->ReleaseLongArrayElements(ENVPAR buf,bufP,JNI_ABORT);
        h5libraryError(env);
    } else  {
        for (i = 0; i < numpoints; i++) {
            bufP[i] = ba[i];
        }
        free (ba) ;
        ENVPTR->ReleaseLongArrayElements(ENVPAR buf,bufP,0);
    }

    return (jint)status;
}


/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Sget_select_bounds
 * Signature: (I[J[J)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Sget_1select_1bounds
  (JNIEnv *env, jclass clss, jint spaceid, jlongArray start, jlongArray end)
{
    herr_t status;
    jlong *startP, *endP;
    jboolean isCopy;
    hsize_t *strt;
    hsize_t *en;
    int rank;
    int i;

    if ( start == NULL ) {
        h5nullArgument( env, "H5Sget_select_bounds:  start is NULL");
        return -1;
    }

    if ( end == NULL ) {
        h5nullArgument( env, "H5Sget_select_bounds:  end is NULL");
        return -1;
    }

    startP = ENVPTR->GetLongArrayElements(ENVPAR start,&isCopy);
    if (startP == NULL) {
        h5JNIFatalError( env, "H5Sget_select_bounds:  start not pinned");
        return -1;
    }
    rank = (int)ENVPTR->GetArrayLength(ENVPAR start);
    strt = (hsize_t *)malloc( rank * sizeof(hsize_t));
    if (strt == NULL) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR start,startP,JNI_ABORT);
        h5JNIFatalError(env,  "H5Sget_select_bounds:  start not converted to hsize_t");
        return -1;
    }

    endP = ENVPTR->GetLongArrayElements(ENVPAR end,&isCopy);
    if (endP == NULL) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR start,startP,JNI_ABORT);
        free(strt);
        h5JNIFatalError( env, "H5Sget_select_bounds:  end not pinned");
        return -1;
    }
    en = (hsize_t *)malloc( rank * sizeof(hsize_t));
    if (en == NULL)  {
        ENVPTR->ReleaseLongArrayElements(ENVPAR end,endP,JNI_ABORT);
        ENVPTR->ReleaseLongArrayElements(ENVPAR start,startP,JNI_ABORT);
        free(strt);
        h5JNIFatalError(env,  "H5Sget_simple_extent:  dims not converted to hsize_t");
        return -1;
    }

    status = H5Sget_select_bounds((hid_t) spaceid, (hsize_t *)strt, (hsize_t *)en);

    if (status < 0) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR start,startP,JNI_ABORT);
        ENVPTR->ReleaseLongArrayElements(ENVPAR end,endP,JNI_ABORT);
        free(strt); 
        free(en);
        h5libraryError(env);
    } else  {
        for (i = 0; i < rank; i++) {
            startP[i] = strt[i];
            endP[i] = en[i];
        }
        ENVPTR->ReleaseLongArrayElements(ENVPAR start,startP,0);
        ENVPTR->ReleaseLongArrayElements(ENVPAR end,endP,0);
        free(strt); 
        free(en);
    }

    return (jint)status;
}


#ifdef __cplusplus
}
#endif
