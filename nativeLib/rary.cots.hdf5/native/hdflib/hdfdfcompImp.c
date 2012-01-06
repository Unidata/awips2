
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

extern jboolean getOldCompInfo( JNIEnv *env, jobject ciobj, comp_info *cinf);

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFDeprecated_DFgetcomp
( JNIEnv *env,
jclass clss,
jint file_id,
jshort tag,
jshort ref,
jbyteArray image, /* OUT: byte[] (really) byte[3][][] or byte[][][3] ? */
jint xdim,
jint ydim,
jint method)
{
    int rval;
    jbyte *im;
    jboolean bb;

    im = ENVPTR->GetByteArrayElements(ENVPAR image,&bb);

    rval = DFgetcomp((int32) file_id, (uint16) tag, (uint16) ref,
        (uint8 *)image, (int32) xdim, (int32) ydim,  (int16) method);
    if (rval == FAIL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR image,im,JNI_ABORT);
        return JNI_FALSE;
    } else {
        ENVPTR->ReleaseByteArrayElements(ENVPAR image,im,0);
        return JNI_TRUE;
    }
}


JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFDeprecated_DFputcomp
( JNIEnv *env,
jclass clss,
jint file_id,
jshort tag,
jshort ref,
jbyteArray image, /* IN: byte[] (actually byte[3][][] or byte[][][3] ? */
jint xdim,
jint ydim,
jbyteArray palette, /* IN: byte[3*256] */
jbyteArray newpal, /* IN?: byte[3*256] */
jint scheme,
jobject c_info)  /* IN: HDFCompInfo c_info */
{
    intn rval;
    jboolean bval;
    comp_info cinf;
    jbyte *im;
    jbyte *p;
    jbyte *np;
    jboolean bb;

    bval = getOldCompInfo(env,c_info,&cinf);

    /* check for success... */

    im = ENVPTR->GetByteArrayElements(ENVPAR image,&bb);
    p = ENVPTR->GetByteArrayElements(ENVPAR palette,&bb);
    np = ENVPTR->GetByteArrayElements(ENVPAR newpal,&bb);


    rval = DFputcomp((int32) file_id, (uint16) tag, (uint16) ref,
        (uint8 *)im, (int32) xdim, (int32) ydim, (uint8 *)p,
        (uint8 *)np, (int16) scheme, (comp_info *)&cinf);

    if (rval == FAIL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR image,im,JNI_ABORT);
        ENVPTR->ReleaseByteArrayElements(ENVPAR palette,p,JNI_ABORT);
        ENVPTR->ReleaseByteArrayElements(ENVPAR newpal,np,JNI_ABORT);
        return JNI_FALSE;
    } else {
        ENVPTR->ReleaseByteArrayElements(ENVPAR image,im,0);
        ENVPTR->ReleaseByteArrayElements(ENVPAR palette,p,0);
        ENVPTR->ReleaseByteArrayElements(ENVPAR newpal,np,0);
        return JNI_TRUE;
    }
}


#ifdef __cplusplus
}
#endif
