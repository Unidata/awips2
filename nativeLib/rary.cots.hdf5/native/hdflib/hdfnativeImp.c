
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
 *  This module contains the implementation of all the native methods
 *  used for number conversion.  This is represented by the Java
 *  class HDFNativeData.
 *
 *  These routines convert one dimensional arrays of bytes into
 *  one-D arrays of other types (int, float, etc) and vice versa.
 *
 *  These routines are called from the Java parts of the Java-C
 *  interface.
 *
 *  ***Important notes:
 *
 *     1.  These routines are designed to be portable--they use the
 *         C compiler to do the required native data manipulation.
 *     2.  These routines copy the data at least once -- a serious
 *         but unavoidable performance hit.
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

extern jboolean h4outOfMemory( JNIEnv *env, char *functName);
extern jboolean h4JNIFatalError( JNIEnv *env, char *functName);
extern jboolean h4raiseException( JNIEnv *env, char *message);

/* returns int [] */
JNIEXPORT jintArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_byteToInt___3B
( JNIEnv *env,
jclass clss,
jbyteArray bdata)  /* IN: array of bytes */
{
    jbyte *barr;
    jintArray rarray;
    int blen;
    jint *iarray;
    jboolean bb;
    char *bp;
    jint *iap;
    int ii;
    int len;

    if (bdata == NULL) {
        h4raiseException( env, "byteToInt: bdata is NULL?");
        return NULL;
    }
    barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
    if (barr == NULL) {
        h4JNIFatalError( env, "byteToInt: pin failed");
        return NULL;
    }

    blen = ENVPTR->GetArrayLength(ENVPAR bdata);

    len = blen/sizeof(jint);
    rarray = ENVPTR->NewIntArray(ENVPAR len);
    if (rarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4outOfMemory( env, "byteToInt" );
        return NULL;
    }

    iarray = ENVPTR->GetIntArrayElements(ENVPAR rarray,&bb);
    if (iarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4JNIFatalError( env, "byteToInt: pin iarray failed");
        return NULL;
    }

    bp = (char *)barr;
    iap = iarray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jint *)bp;
        iap++;
        bp += sizeof(jint);
    }

    ENVPTR->ReleaseIntArrayElements(ENVPAR rarray,iarray, 0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);

    return rarray;

}

/* returns float [] */
JNIEXPORT jfloatArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_byteToFloat___3B
( JNIEnv *env,
jclass clss,
jbyteArray bdata)  /* IN: array of bytes */
{
    jbyte *barr;
    jfloatArray rarray;
    int blen;
    jfloat *farray;
    jboolean bb;
    char *bp;
    jfloat *iap;
    int ii;
    int len;

    if (bdata == NULL) {
        h4raiseException( env, "byteToFloat: bdata is NULL?");
        return NULL;
    }
    barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
    if (barr == NULL) {
        h4JNIFatalError( env, "byteToFloat: pin failed");
        return NULL;
    }
    blen = ENVPTR->GetArrayLength(ENVPAR bdata);

    len = blen/sizeof(jfloat);
    rarray = ENVPTR->NewFloatArray(ENVPAR len);
    if (rarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4outOfMemory( env, "byteToFloat" );
        return NULL;
    }
    farray = ENVPTR->GetFloatArrayElements(ENVPAR rarray,&bb);
    if (farray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4JNIFatalError( env, "byteToFloat: pin farray failed");
        return NULL;
    }

    bp = (char *)barr;
    iap = farray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jfloat *)bp;
        iap++;
        bp += sizeof(jfloat);
    }

    ENVPTR->ReleaseFloatArrayElements(ENVPAR rarray,farray, 0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);

    return rarray;

}

/* returns short [] */
JNIEXPORT jshortArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_byteToShort___3B
( JNIEnv *env,
jclass clss,
jbyteArray bdata)  /* IN: array of bytes */
{
    jbyte *barr;
    jshortArray rarray;
    int blen;
    jshort *sarray;
    jboolean bb;
    char *bp;
    jshort *iap;
    int ii;
    int len;

    if (bdata == NULL) {
        h4raiseException( env, "byteToShort: bdata is NULL?");
        return NULL;
    }
    barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
    if (barr == NULL) {
        h4JNIFatalError( env, "byteToShort: pin failed");
        return NULL;
    }

    blen = ENVPTR->GetArrayLength(ENVPAR bdata);

    len = blen/sizeof(jshort);
    rarray = ENVPTR->NewShortArray(ENVPAR len);
    if (rarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4outOfMemory( env, "byteToShort" );
        return NULL;
    }

    sarray = ENVPTR->GetShortArrayElements(ENVPAR rarray,&bb);
    if (sarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4JNIFatalError( env, "byteToShort: pin sarray failed");
        return NULL;
    }

    bp = (char *)barr;
    iap = sarray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jshort *)bp;
        iap++;
        bp += sizeof(jshort);
    }

    ENVPTR->ReleaseShortArrayElements(ENVPAR rarray,sarray, 0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);

    return rarray;

}


/* returns long [] */
JNIEXPORT jlongArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_byteToLong___3B
( JNIEnv *env,
jclass clss,
jbyteArray bdata)  /* IN: array of bytes */
{
    jbyte *barr;
    jlongArray rarray;
    int blen;
    jlong *larray;
    jboolean bb;
    char *bp;
    jlong *iap;
    int ii;
    int len;

    if (bdata == NULL) {
        h4raiseException( env, "byteToLong: bdata is NULL?");
        return NULL;
    }
    barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
    if (barr == NULL) {
        h4JNIFatalError( env, "byteToLong: pin failed");
        return NULL;
    }
    blen = ENVPTR->GetArrayLength(ENVPAR bdata);

    len = blen/sizeof(jlong);
    rarray = ENVPTR->NewLongArray(ENVPAR len);
    if (rarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4outOfMemory( env, "byteToLong" );
        return NULL;
    }

    larray = ENVPTR->GetLongArrayElements(ENVPAR rarray,&bb);
    if (larray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4JNIFatalError( env, "byteToLong: pin larray failed");
        return NULL;
    }

    bp = (char *)barr;
    iap = larray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jlong *)bp;
        iap++;
        bp += sizeof(jlong);
    }
    ENVPTR->ReleaseLongArrayElements(ENVPAR rarray,larray, 0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);

    return rarray;

}


/* returns double [] */
JNIEXPORT jdoubleArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_byteToDouble___3B
( JNIEnv *env,
jclass clss,
jbyteArray bdata)  /* IN: array of bytes */
{
    jbyte *barr;
    jdoubleArray rarray;
    int blen;
    jdouble *darray;
    jboolean bb;
    char *bp;
    jdouble *iap;
    int ii;
    int len;

    if (bdata == NULL) {
        h4raiseException( env, "byteToDouble: bdata is NULL?");
        return NULL;
    }
    barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
    if (barr == NULL) {
        h4JNIFatalError( env, "byteToDouble: pin failed");
        return NULL;
    }
    blen = ENVPTR->GetArrayLength(ENVPAR bdata);

    len = blen/sizeof(jdouble);
    rarray = ENVPTR->NewDoubleArray(ENVPAR len);
    if (rarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4outOfMemory( env, "byteToDouble" );
        return NULL;
    }

    darray = ENVPTR->GetDoubleArrayElements(ENVPAR rarray,&bb);
    if (darray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4JNIFatalError( env, "byteToDouble: pin darray failed");
        return NULL;
    }

    bp = (char *)barr;
    iap = darray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jdouble *)bp;
        iap++;
        bp += sizeof(jdouble);
    }

    ENVPTR->ReleaseDoubleArrayElements(ENVPAR rarray,darray,0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);

    return rarray;
}


/* returns int [] */
JNIEXPORT jintArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_byteToInt__II_3B
( JNIEnv *env,
jclass clss,
jint start,
jint len,
jbyteArray bdata)  /* IN: array of bytes */
{
    char *bp;
    jbyte *barr;
    jintArray rarray;
    int blen;
    jint *iarray;
    jint *iap;
    int ii;
    jboolean bb;

    if (bdata == NULL) {
        h4raiseException( env, "byteToInt: bdata is NULL?");
        return NULL;
    }
    barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
    if (barr == NULL) {
        h4JNIFatalError( env, "byteToInt: pin failed");
        return NULL;
    }

    blen = ENVPTR->GetArrayLength(ENVPAR bdata);
    if ((start < 0) || ((int)(start + (len*sizeof(jint))) > blen)) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4JNIFatalError( env, "byteToInt: getLen failed");
        return NULL;
    }

    bp = (char *)barr + start;

    rarray = ENVPTR->NewIntArray(ENVPAR len);
    if (rarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4outOfMemory( env, "byteToInt" );
        return NULL;
    }

    iarray = ENVPTR->GetIntArrayElements(ENVPAR rarray,&bb);
    if (iarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4JNIFatalError( env, "byteToInt: pin iarray failed");
        return NULL;
    }

    iap = iarray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jint *)bp;
        iap++;
        bp += sizeof(jint);
    }

    ENVPTR->ReleaseIntArrayElements(ENVPAR rarray,iarray, 0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);

    return rarray;

}

/* returns short [] */
JNIEXPORT jshortArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_byteToShort__II_3B
( JNIEnv *env,
jclass clss,
jint start,
jint len,
jbyteArray bdata)  /* IN: array of bytes */
{
    char *bp;
    jbyte *barr;
    jshortArray rarray;
    int blen;
    jshort *iarray;
    jshort *iap;
    int ii;
    jboolean bb;

    if (bdata == NULL) {
        h4raiseException( env, "byteToShort: bdata is NULL?");
        return NULL;
    }
    barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
    if (barr == NULL) {
        h4JNIFatalError( env, "byteToShort: getByte failed?");
        return NULL;
    }

    blen = ENVPTR->GetArrayLength(ENVPAR bdata);
    if ((start < 0) || ((int)(start + (len*(sizeof(jshort)))) > blen)) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4raiseException( env, "byteToShort: start or len is out of bounds");
        return NULL;
    }

    bp = (char *)barr + start;

    rarray = ENVPTR->NewShortArray(ENVPAR len);
    if (rarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4outOfMemory( env, "byteToShort" );
        return NULL;
    }

    iarray = ENVPTR->GetShortArrayElements(ENVPAR rarray,&bb);
    if (iarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4JNIFatalError( env, "byteToShort: getShort failed?");
        return NULL;
    }

    iap = iarray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jshort *)bp;
        iap++;
        bp += sizeof(jshort);
    }

    ENVPTR->ReleaseShortArrayElements(ENVPAR rarray,iarray, 0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);

    return rarray;

}

/* returns float [] */
JNIEXPORT jfloatArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_byteToFloat__II_3B
( JNIEnv *env,
jclass clss,
jint start,
jint len,
jbyteArray bdata)  /* IN: array of bytes */
{
    char *bp;
    jbyte *barr;
    jfloatArray rarray;
    int blen;
    jfloat *iarray;
    jfloat *iap;
    int ii;
    jboolean bb;

    if (bdata == NULL) {
        h4raiseException( env, "byteToFloat: bdata is NULL?");
        return NULL;
    }
    barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
    if (barr == NULL) {
        h4JNIFatalError( env, "byteToFloat: getByte failed?");
        return NULL;
    }

    blen = ENVPTR->GetArrayLength(ENVPAR bdata);
    if ((start < 0) || ((int)(start + (len*(sizeof(jfloat)))) > blen)) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4raiseException( env, "byteToFloat: start or len is out of bounds");
        return NULL;
    }

    bp = (char *)barr + start;

    rarray = ENVPTR->NewFloatArray(ENVPAR len);
    if (rarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4outOfMemory( env, "byteToFloat" );
        return NULL;
    }

    iarray = ENVPTR->GetFloatArrayElements(ENVPAR rarray,&bb);
    if (iarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4JNIFatalError( env, "byteToFloat: getFloat failed?");
        return NULL;
    }

    iap = iarray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jfloat *)bp;
        iap++;
        bp += sizeof(jfloat);
    }

    ENVPTR->ReleaseFloatArrayElements(ENVPAR rarray,iarray, 0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);

    return rarray;

}

/* returns long [] */
JNIEXPORT jlongArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_byteToLong__II_3B
( JNIEnv *env,
jclass clss,
jint start,
jint len,
jbyteArray bdata)  /* IN: array of bytes */
{
    char *bp;
    jbyte *barr;
    jlongArray rarray;
    int blen;
    jlong *iarray;
    jlong *iap;
    int ii;
    jboolean bb;

    if (bdata == NULL) {
        h4raiseException( env, "byteToLong: bdata is NULL?");
        return NULL;
    }
    barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
    if (barr == NULL) {
        h4JNIFatalError( env, "byteToLong: getByte failed?");
        return NULL;
    }

    blen = ENVPTR->GetArrayLength(ENVPAR bdata);
    if ((start < 0) || ((int)(start + (len*(sizeof(jlong)))) > blen)) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4raiseException( env, "byteToLong: start or len is out of bounds");
        return NULL;
    }

    bp = (char *)barr + start;

    rarray = ENVPTR->NewLongArray(ENVPAR len);
    if (rarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4outOfMemory( env, "byteToLong" );
        return NULL;
    }

    iarray = ENVPTR->GetLongArrayElements(ENVPAR rarray,&bb);
    if (iarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4JNIFatalError( env, "byteToLong: getLong failed?");
        return NULL;
    }

    iap = iarray;
    for (ii = 0; ii < len; ii++) {

        *iap = *(jlong *)bp;
        iap++;
        bp += sizeof(jlong);
    }

    ENVPTR->ReleaseLongArrayElements(ENVPAR rarray,iarray, 0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);

    return rarray;

}

/* returns double [] */
JNIEXPORT jdoubleArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_byteToDouble__II_3B
( JNIEnv *env,
jclass clss,
jint start,
jint len,
jbyteArray bdata)  /* IN: array of bytes */
{
    char *bp;
    jbyte *barr;
    jdoubleArray rarray;
    int blen;
    jdouble *iarray;
    jdouble *iap;
    int ii;
    jboolean bb;

    if (bdata == NULL) {
        h4raiseException( env, "byteToDouble: bdata is NULL?");
        return NULL;
    }
    barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
    if (barr == NULL) {
        h4JNIFatalError( env, "byteToDouble: getByte failed?");
        return NULL;
    }

    blen = ENVPTR->GetArrayLength(ENVPAR bdata);
    if ((start < 0) || ((int)(start + (len*(sizeof(jdouble)))) > blen)) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4raiseException( env, "byteToDouble: start or len is out of bounds");
        return NULL;
    }

    bp = (char *)barr + start;

    rarray = ENVPTR->NewDoubleArray(ENVPAR len);
    if (rarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h4outOfMemory( env, "byteToDouble" );
        return NULL;
    }

    iarray = ENVPTR->GetDoubleArrayElements(ENVPAR rarray,&bb);
    if (iarray == NULL) {
        h4JNIFatalError( env, "byteToDouble: getDouble failed?");
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        return NULL;
    }

    iap = iarray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jdouble *)bp;
        iap++;
        bp += sizeof(jdouble);
    }

    ENVPTR->ReleaseDoubleArrayElements(ENVPAR rarray,iarray, 0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);

    return rarray;
}

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_intToByte__II_3I
(JNIEnv *env,
jclass clss,
jint start,
jint len,
jintArray idata)  /* IN: array of int */
{
    jint *ip;
    jint *iarr;
    int ilen;
    jbyteArray rarray;
    int blen;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ii;
    int ij;
    union things {
        int ival;
        char bytes[4];
    } u;

    if (idata == NULL) {
        h4raiseException( env, "intToByte: idata is NULL?");
        return NULL;
    }
    iarr = ENVPTR->GetIntArrayElements(ENVPAR idata,&bb);
    if (iarr == NULL) {
        h4JNIFatalError( env, "intToByte: getInt failed?");
        return NULL;
    }

    ilen = ENVPTR->GetArrayLength(ENVPAR idata);
    if ((start < 0) || (((start + len)) > ilen)) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h4raiseException( env, "intToByte: start or len is out of bounds");
        return NULL;
    }

    ip = iarr + start;

    blen = ilen * sizeof(jint);
    rarray = ENVPTR->NewByteArray(ENVPAR blen);
    if (rarray == NULL) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h4outOfMemory( env, "intToByte" );
        return NULL;
    }

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h4JNIFatalError( env, "intToByte: getByte failed?");
        return NULL;
    }

    bap = barray;
    for (ii = 0; ii < len; ii++) {
        u.ival = *ip++;
        for (ij = 0; ij < sizeof(jint); ij++) {
            *bap = u.bytes[ij];
            bap++;
        }
    }

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,barray, 0);
    ENVPTR->ReleaseIntArrayElements(ENVPAR idata,iarr,JNI_ABORT);

    return rarray;

}

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_shortToByte__II_3S
( JNIEnv *env,
jclass clss,
jint start,
jint len,
jshortArray idata)  /* IN: array of short */
{
    jshort *ip;
    jshort *iarr;
    int ilen;
    jbyteArray rarray;
    int blen;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ii;
    int ij;
    union things {
        short ival;
        char bytes[4];
    } u;

    if (idata == NULL) {
        h4raiseException( env, "shortToByte: idata is NULL?");
        return NULL;
    }
    iarr = ENVPTR->GetShortArrayElements(ENVPAR idata,&bb);
    if (iarr == NULL) {
        h4JNIFatalError( env, "shortToByte: getShort failed?");
        return NULL;
    }

    ilen = ENVPTR->GetArrayLength(ENVPAR idata);
    if ((start < 0) || (((start + len)) > ilen)) {
        ENVPTR->ReleaseShortArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h4raiseException( env, "shortToByte: start or len is out of bounds");
        return NULL;
    }

    ip = iarr + start;

    blen = ilen * sizeof(jshort);
    rarray = ENVPTR->NewByteArray(ENVPAR blen);
    if (rarray == NULL) {
        ENVPTR->ReleaseShortArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h4outOfMemory( env, "shortToByte" );
        return NULL;
    }

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        ENVPTR->ReleaseShortArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h4JNIFatalError( env, "shortToByte: getByte failed?");
        return NULL;
    }

    bap = barray;
    for (ii = 0; ii < len; ii++) {
        u.ival = *ip++;
        for (ij = 0; ij < sizeof(jshort); ij++) {
            *bap = u.bytes[ij];
            bap++;
        }
    }

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,barray, 0);
    ENVPTR->ReleaseShortArrayElements(ENVPAR idata,iarr,JNI_ABORT);

    return rarray;

}

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_floatToByte__II_3F
( JNIEnv *env,
jclass clss,
jint start,
jint len,
jfloatArray idata)  /* IN: array of float */
{
    jfloat *ip;
    jfloat *iarr;
    int ilen;
    jbyteArray rarray;
    int blen;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ii;
    int ij;
    union things {
        float ival;
        char bytes[4];
    } u;

    if (idata == NULL) {
        h4raiseException( env, "floatToByte: idata is NULL?");
        return NULL;
    }
    iarr = ENVPTR->GetFloatArrayElements(ENVPAR idata,&bb);
    if (iarr == NULL) {
        h4JNIFatalError( env, "floatToByte: getFloat failed?");
        return NULL;
    }

    ilen = ENVPTR->GetArrayLength(ENVPAR idata);
    if ((start < 0) || (((start + len)) > ilen)) {
        ENVPTR->ReleaseFloatArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h4raiseException( env, "floatToByte: start or len is out of bounds");
        return NULL;
    }

    ip = iarr + start;

    blen = ilen * sizeof(jfloat);
    rarray = ENVPTR->NewByteArray(ENVPAR blen);
    if (rarray == NULL) {
        ENVPTR->ReleaseFloatArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h4outOfMemory( env, "floatToByte" );
        return NULL;
    }

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        ENVPTR->ReleaseFloatArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h4JNIFatalError( env, "floatToByte: getByte failed?");
        return NULL;
    }

    bap = barray;
    for (ii = 0; ii < len; ii++) {
        u.ival = *ip++;
        for (ij = 0; ij < sizeof(jfloat); ij++) {
            *bap = u.bytes[ij];
            bap++;
        }
    }

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,barray, 0);
    ENVPTR->ReleaseFloatArrayElements(ENVPAR idata,iarr,JNI_ABORT);

    return rarray;

}

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_doubleToByte__II_3D
( JNIEnv *env,
jclass clss,
jint start,
jint len,
jdoubleArray idata)  /* IN: array of double */
{
    jdouble *ip;
    jdouble *iarr;
    int ilen;
    jbyteArray rarray;
    int blen;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ii;
    int ij;
    union things {
        double ival;
        char bytes[8];
    } u;

    if (idata == NULL) {
        h4raiseException( env, "doubleToByte: idata is NULL?");
        return NULL;
    }
    iarr = ENVPTR->GetDoubleArrayElements(ENVPAR idata,&bb);
    if (iarr == NULL) {
        h4JNIFatalError( env, "doubleToByte: getDouble failed?");
        return NULL;
    }

    ilen = ENVPTR->GetArrayLength(ENVPAR idata);
    if ((start < 0) || (((start + len)) > ilen)) {
        ENVPTR->ReleaseDoubleArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h4raiseException( env, "doubleToByte: start or len is out of bounds");
        return NULL;
    }

    ip = iarr + start;

    blen = ilen * sizeof(jdouble);
    rarray = ENVPTR->NewByteArray(ENVPAR blen);
    if (rarray == NULL) {
        ENVPTR->ReleaseDoubleArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h4outOfMemory( env, "doubleToByte" );
        return NULL;
    }

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        ENVPTR->ReleaseDoubleArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h4JNIFatalError( env, "doubleToByte: getByte failed?");
        return NULL;
    }

    bap = barray;
    for (ii = 0; ii < len; ii++) {
        u.ival = *ip++;
        for (ij = 0; ij < sizeof(jdouble); ij++) {
            *bap = u.bytes[ij];
            bap++;
        }
    }

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,barray, 0);
    ENVPTR->ReleaseDoubleArrayElements(ENVPAR idata,iarr,JNI_ABORT);

    return rarray;

}


/* returns byte [] */
JNIEXPORT jbyteArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_longToByte__II_3J
( JNIEnv *env,
jclass clss,
jint start,
jint len,
jlongArray idata)  /* IN: array of long */
{
    jlong *ip;
    jlong *iarr;
    int ilen;
    jbyteArray rarray;
    int blen;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ii;
    int ij;
    union things {
        jlong ival;
        char bytes[8];
    } u;

    if (idata == NULL) {
        h4raiseException( env, "longToByte: idata is NULL?");
        return NULL;
    }
    iarr = ENVPTR->GetLongArrayElements(ENVPAR idata,&bb);
    if (iarr == NULL) {
        h4JNIFatalError( env, "longToByte: getLong failed?");
        return NULL;
    }

    ilen = ENVPTR->GetArrayLength(ENVPAR idata);
    if ((start < 0) || (((start + len)) > ilen)) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h4raiseException( env, "longToByte: start or len is out of bounds?\n");
        return NULL;
    }

    ip = iarr + start;

    blen = ilen * sizeof(jlong);
    rarray = ENVPTR->NewByteArray(ENVPAR blen);
    if (rarray == NULL) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h4outOfMemory( env, "longToByte" );
        return NULL;
    }

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h4JNIFatalError( env, "longToByte: getByte failed?");
        return NULL;
    }

    bap = barray;
    for (ii = 0; ii < len; ii++) {
        u.ival = *ip++;
        for (ij = 0; ij < sizeof(jlong); ij++) {
            *bap = u.bytes[ij];
            bap++;
        }
    }

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,barray, 0);
    ENVPTR->ReleaseLongArrayElements(ENVPAR idata,iarr,JNI_ABORT);

    return rarray;

}
 /******/


/* returns byte [] */
JNIEXPORT jbyteArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_intToByte__I
( JNIEnv *env,
jclass clss,
jint idata)  /* IN: int */
{
    jbyteArray rarray;
    jbyte *barray;
    jbyte *bap;
    int ij;
    jboolean bb;
    union things {
        int ival;
        char bytes[sizeof(int)];
    } u;

    rarray = ENVPTR->NewByteArray(ENVPAR sizeof(jint));
    if (rarray == NULL) {
        h4outOfMemory( env, "intToByte" );
        return NULL;
    }

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        h4JNIFatalError( env, "intToByte: getByte failed?");
        return NULL;
    }

    bap = barray;
    u.ival = idata;
    for (ij = 0; ij < sizeof(jint); ij++) {
        *bap = u.bytes[ij];
        bap++;
    }

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,barray, 0);
    return rarray;

}

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_floatToByte__F
( JNIEnv *env,
jclass clss,
jfloat idata)  /* IN: int */
{
    jbyteArray rarray;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ij;
    union things {
        float ival;
        char bytes[sizeof(float)];
    } u;

    rarray = ENVPTR->NewByteArray(ENVPAR sizeof(jfloat));
    if (rarray == NULL) {
        h4outOfMemory( env, "floatToByte" );
        return NULL;
    }

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        h4JNIFatalError( env, "floatToByte: getByte failed?");
        return NULL;
    }

    bap = barray;
    u.ival = idata;
    for (ij = 0; ij < sizeof(jfloat); ij++) {
        *bap = u.bytes[ij];
        bap++;
    }

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,(jbyte *)barray, 0);
    return rarray;

}

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_shortToByte__S
( JNIEnv *env,
jclass clss,
jshort idata)  /* IN: short */
{
    jbyteArray rarray;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ij;
    union things {
        short ival;
        char bytes[sizeof(short)];
    } u;

    rarray = ENVPTR->NewByteArray(ENVPAR sizeof(jshort));
    if (rarray == NULL) {
        h4outOfMemory( env, "shortToByte" );
        return NULL;
    }

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        h4JNIFatalError( env, "shortToByte: getShort failed?");
        return NULL;
    }

    bap = barray;
    u.ival = idata;
    for (ij = 0; ij < sizeof(jshort); ij++) {
        *bap = u.bytes[ij];
        bap++;
    }

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,(jbyte *)barray, 0);

    return rarray;

}


/* returns byte [] */
JNIEXPORT jbyteArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_doubleToByte__D
( JNIEnv *env,
jclass clss,
jdouble idata)  /* IN: double */
{
    jbyteArray rarray;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ij;
    union things {
        double ival;
        char bytes[sizeof(double)];
    } u;

    rarray = ENVPTR->NewByteArray(ENVPAR sizeof(jdouble));
    if (rarray == NULL) {
        h4outOfMemory( env, "doubleToByte" );
        return NULL;
    }

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        h4JNIFatalError( env, "doubleToByte: getDouble failed?");
        return NULL;
    }

    bap = barray;
    u.ival = idata;
    for (ij = 0; ij < sizeof(jdouble); ij++) {
        *bap = u.bytes[ij];
        bap++;
    }

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,(jbyte *)barray, 0);

    return rarray;
}


/* returns byte [] */
JNIEXPORT jbyteArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_longToByte__J
( JNIEnv *env,
jclass clss,
jlong idata)  /* IN: array of long */
{
    jbyteArray rarray;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ij;
    union things {
        jlong ival;
        char bytes[sizeof(jlong)];
    } u;

    rarray = ENVPTR->NewByteArray(ENVPAR sizeof(jlong));
    if (rarray == NULL) {
        h4outOfMemory( env, "longToByte" );
        return NULL;
    }

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        h4JNIFatalError( env, "longToByte: getLong failed?");
        return NULL;
    }

    bap = barray;
    u.ival = idata;
    for (ij = 0; ij < sizeof(jlong); ij++) {
        *bap = u.bytes[ij];
        bap++;
    }

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,(jbyte *)barray, 0);

    return rarray;

}

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL Java_ncsa_hdf_hdflib_HDFNativeData_byteToByte__B
( JNIEnv *env,
jclass clss,
jbyte idata)  /* IN: array of long */
{
    jbyteArray rarray;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ij;
    union things {
        jbyte ival;
        char bytes[sizeof(jbyte)];
    } u;

    rarray = ENVPTR->NewByteArray(ENVPAR sizeof(jbyte));
    if (rarray == NULL) {
        h4outOfMemory( env, "byteToByte" );
        return NULL;
    }

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        h4JNIFatalError( env, "byteToByte: getByte failed?");
        return NULL;
    }

    bap = barray;
    u.ival = idata;
    for (ij = 0; ij < sizeof(jbyte); ij++) {
        *bap = u.bytes[ij];
        bap++;
    }

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,(jbyte *)barray, 0);

    return rarray;
}

#ifdef __cplusplus
}
#endif
