
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

extern jboolean h4outOfMemory( JNIEnv *env, char *functName);

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_VSQuerycount
( JNIEnv *env,
jclass clss,
jint vdata_id,
jintArray n_records) /* OUT: int */
{
    intn rval;
    jint * theArg;
        jboolean bb;

        theArg = ENVPTR->GetIntArrayElements(ENVPAR n_records,&bb);

    rval = VSQuerycount((int32) vdata_id, (int32 *)&(theArg[0]));

        if (rval == FAIL) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR n_records,theArg,JNI_ABORT);
                return JNI_FALSE;
        } else {
        ENVPTR->ReleaseIntArrayElements(ENVPAR n_records,theArg,0);
                return JNI_TRUE;
        }

}


JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_VSQueryfields
( JNIEnv *env,
jclass clss,
jint vdata_id,
jobjectArray fields)  /* OUT: String */
{
    intn rval;
    char flds[4096];
    jstring rstring;
    jclass jc;
    jobject o;
    jboolean bb;

    rval = VSQueryfields((int32) vdata_id, (char *)flds);
    flds[4095] = '\0';

    if (rval == FAIL) {
        return JNI_FALSE;
    } else {
        /* convert it to java string */
        rstring = ENVPTR->NewStringUTF(ENVPAR flds);

        /*  create a Java String object in the calling environment... */
        jc = ENVPTR->FindClass(ENVPAR  "java/lang/String");
        if (jc == NULL) {
            return JNI_FALSE; /* exception is raised */
        }
        o = ENVPTR->GetObjectArrayElement(ENVPAR fields,0);
        if (o == NULL) {
            return JNI_FALSE;
        }
        bb = ENVPTR->IsInstanceOf(ENVPAR o,jc);
        if (bb == JNI_FALSE) {
            return JNI_FALSE;
        }
        ENVPTR->SetObjectArrayElement(ENVPAR fields,0,(jobject)rstring);
        return JNI_TRUE;
    }
}

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_VSQueryinterlace
( JNIEnv *env,
jclass clss,
jint vdata_id,
jintArray interlace) /* OUT: int */
{
    intn rval;
    jint * theArg;
        jboolean bb;

        theArg = ENVPTR->GetIntArrayElements(ENVPAR interlace,&bb);

    rval = VSQueryinterlace((int32) vdata_id, (int32 *)&(theArg[0]));

        if (rval == FAIL) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR interlace,theArg,JNI_ABORT);
                return JNI_FALSE;
        } else {
        ENVPTR->ReleaseIntArrayElements(ENVPAR interlace,theArg,0);
        return JNI_TRUE;
        }

}



JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_VSQueryname
( JNIEnv *env,
jclass clss,
jint vdata_id,
jobjectArray vdata_name)  /* OUT: String */
{
    intn rval;
    char *nm;
    jstring rstring;
    jclass jc;
    jobject o;
    jboolean bb;

    nm = (char *)malloc(VSNAMELENMAX+1);
    if (nm == NULL) {
        h4outOfMemory(env,  "VSQueryname");
        return JNI_FALSE;
    }
    rval = VSQueryname((int32) vdata_id, (char *)nm);
    nm[VSNAMELENMAX] = '\0';

    if (rval == FAIL) {
        free(nm);
        return JNI_FALSE;
    } else {
        /* convert it to java string */
        rstring = ENVPTR->NewStringUTF(ENVPAR nm);

        /*  create a Java String object in the calling environment... */
        jc = ENVPTR->FindClass(ENVPAR  "java/lang/String");
        if (jc == NULL) {
            free(nm);
            return JNI_FALSE; /* exception is raised */
        }
        o = ENVPTR->GetObjectArrayElement(ENVPAR vdata_name,0);
        if (o == NULL) {
            free(nm);
            return JNI_FALSE;
        }
        bb = ENVPTR->IsInstanceOf(ENVPAR o,jc);
        if (bb == JNI_FALSE) {
            free(nm);
            return JNI_FALSE;
        }
        ENVPTR->SetObjectArrayElement(ENVPAR vdata_name,0,(jobject)rstring);
        free(nm);
        return JNI_TRUE;
    }
}

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_VSQueryref
( JNIEnv *env,
jclass clss,
jint vdata_id)
{
    return (VSQueryref((int32)vdata_id));
}

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_VSQuerytag
( JNIEnv *env,
jclass clss,
jint vdata_id)
{
    return (VSQuerytag((int32)vdata_id));
}

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_VSQueryvsize
( JNIEnv *env,
jclass clss,
jint vdata_id,
jintArray vdata_size) /* OUT: int */
{
    intn rval;
    jint * theArg;
        jboolean bb;

        theArg = ENVPTR->GetIntArrayElements(ENVPAR vdata_size,&bb);

    rval = VSQueryvsize((int32) vdata_id, (int32 *)&(theArg[0]));

        if (rval == FAIL) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR vdata_size,theArg,JNI_ABORT);
                return JNI_FALSE;
        } else {
        ENVPTR->ReleaseIntArrayElements(ENVPAR vdata_size,theArg,0);
                return JNI_TRUE;
        }
}

#ifdef __cplusplus
}
#endif
