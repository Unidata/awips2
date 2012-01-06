
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
#include "hfile.h"
#include "jni.h"

#ifdef __cplusplus
#define ENVPTR (env)
#define ENVPAR 
#else
#define ENVPTR (*env)
#define ENVPAR env,
#endif

extern jboolean h4buildException( JNIEnv *env, jint HDFerr);

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_Hopen
(
JNIEnv *env,
jclass clss,
jstring hdfFile,
jint access)

{

    char* file;
    int  retVal;
    int errval;
    jclass jc;

    file =(char *) ENVPTR->GetStringUTFChars(ENVPAR hdfFile,0);

    if (file == NULL) {
        /* call failed? */
        jc = ENVPTR->FindClass(ENVPAR  "ncsa/hdf/hdflib/HDFJavaException");
        if (jc == NULL) {
            return -1; /* exception is raised */
        }
        ENVPTR->ThrowNew(ENVPAR jc,"Hopen: GetStringUTFChars failed");
    }

    /* open HDF file specified by ncsa_hdf_HDF_file */
    retVal = Hopen((char *)file, access, 0);

    ENVPTR->ReleaseStringUTFChars(ENVPAR hdfFile,file);

    if (retVal == FAIL) {
        /* check for error */
        /* for now:  use top of exception stack:  fix this
                   to do whole stack */
        errval = HEvalue(1);
        if (errval != DFE_NONE) {
            h4buildException( env, errval );
            jc = ENVPTR->FindClass(ENVPAR  "ncsa/hdf/hdflib/HDFLibraryException");
            if (jc == NULL) {
                return -1; /* exception is raised */
            }
            ENVPTR->ThrowNew(ENVPAR jc,HEstring((hdf_err_code_t)errval));
        }
        return -1;
    }
    else {
        return retVal;
    }
}

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_Hclose
( JNIEnv *env,
jclass clss,
jint fid)
{
    int status = 0;

    if (fid < 0) {
        /* maybe not an exception -- the file is already closed? */
        return JNI_FALSE;
    } else {
        /* close the HDF file */
        status = Hclose(fid);
        if (status == FAIL) {
            /* ideally, return an exception explaining the
                       reason, especially for DFE_OPENAID error
                    */
            return JNI_FALSE;
        } else {
            return JNI_TRUE;
        }

    }
}

/*
 * Class:     ncsa_hdf_hdflib_HDFLibrary
 * Method:    HDdont_atexit
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_HDdont_1atexit
  (JNIEnv *env, jclass clss)
{

    return (jint) HDdont_atexit();
}


JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_Hishdf
( JNIEnv *env,
jclass clss,
jstring hdfFile)
{

    char * hfile;
    int  retVal;

    hfile = (char *)ENVPTR->GetStringUTFChars(ENVPAR hdfFile,0);

    /* open HDF file specified by ncsa_hdf_HDF_file */
    retVal = Hishdf((char *)hfile);
    ENVPTR->ReleaseStringUTFChars(ENVPAR hdfFile,hfile);
    if (retVal == FALSE) {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }

}


JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_Hnumber
( JNIEnv *env,
jclass clss,
jint fid)
{
    return (Hnumber(fid, DFTAG_WILDCARD));
}

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_DFKNTsize
( JNIEnv *env,
jclass clss,
jint numbertype)
{
    return (DFKNTsize(numbertype));
}



JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_Hcache
( JNIEnv *env,
jclass clss,
jint file_id,
jint cache_switch)
{

    intn rval;
    rval =  Hcache((int32) file_id, (intn) cache_switch);

    if (rval == FAIL) {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}


JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_Hgetfileversion
( JNIEnv *env,
jclass clss,
jint file_id,
jintArray vers,  /* OUT: int major_v, minor_v, release */
jobjectArray string)  /* OUT: String[] */
{
    intn rval;
    jclass Sjc;
    char s[LIBVSTR_LEN+1] ;
    jstring name;
    jint *theArgs;
    jboolean bb;
    jobject o;

    theArgs = ENVPTR->GetIntArrayElements(ENVPAR vers,&bb);

    rval = Hgetfileversion((int32) file_id, (uint32 *)&(theArgs[0]),
        (uint32 *)&(theArgs[1]), (uint32 *)&(theArgs[2]), s);
    s[LIBVSTR_LEN] = '\0';

    if (rval == FAIL) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR vers,theArgs,JNI_ABORT);
        return JNI_FALSE;
    } else {
        ENVPTR->ReleaseIntArrayElements(ENVPAR vers,theArgs,0);
        Sjc = ENVPTR->FindClass(ENVPAR  "java/lang/String");
        if (Sjc == NULL) {
            return JNI_FALSE;
        }
        o = ENVPTR->GetObjectArrayElement(ENVPAR string,0);
        if (o == NULL) {
            return JNI_FALSE;
        }
        bb = ENVPTR->IsInstanceOf(ENVPAR o,Sjc);
        if (bb == JNI_FALSE) {
            /* exception */
            return JNI_FALSE;
        }
        name = ENVPTR->NewStringUTF(ENVPAR s);
        if (name != NULL) {
                        ENVPTR->SetObjectArrayElement(ENVPAR string,0,(jobject)name);
        }
        return JNI_TRUE;
    }
}

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_Hgetlibversion
( JNIEnv *env,
jclass clss,
jintArray vers,  /* OUT: int major_v, minor_v, release */
jobjectArray string)  /* OUT: String[] */
{
    intn rval;
    jclass Sjc;
    char s[LIBVSTR_LEN+1] ;
    jint *theArgs;
    jstring name;
    jobject o;
    jboolean bb;

    theArgs = ENVPTR->GetIntArrayElements(ENVPAR vers,&bb);
    s[LIBVSTR_LEN] = '\0';

    rval = Hgetlibversion((uint32 *)&(theArgs[0]),
        (uint32 *)&(theArgs[1]), (uint32 *)&(theArgs[2]), s);

    if (rval == FAIL) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR vers,theArgs,JNI_ABORT);
        return JNI_FALSE;
    } else {
        ENVPTR->ReleaseIntArrayElements(ENVPAR vers,theArgs,0);
        Sjc = ENVPTR->FindClass(ENVPAR  "java/lang/String");
        if (Sjc == NULL) {
            return JNI_FALSE;
        }
        o = ENVPTR->GetObjectArrayElement(ENVPAR string,0);
        if (o == NULL) {
            return JNI_FALSE;
        }
        bb = ENVPTR->IsInstanceOf(ENVPAR o,Sjc);
        if (bb == JNI_FALSE) {
            /* exception */
            return JNI_FALSE;
        }
        name = ENVPTR->NewStringUTF(ENVPAR s);
        if (name != NULL) {
                        ENVPTR->SetObjectArrayElement(ENVPAR string,0,(jobject)name);
        }
        return JNI_TRUE;
    }
}


JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_Hsetaccesstype
( JNIEnv *env,
jclass clss,
jint h_id,
jint  access_type)
{
    int32 rval;

    rval = Hsetaccesstype((int32) h_id, (uintn)  access_type);
    if (rval == FAIL) {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_Hsync
( JNIEnv *env,
jclass clss,
jint file_id)
{
    intn rval;

    rval = Hsync((int32) file_id);
    if (rval == FAIL) {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_HDFclose
( JNIEnv *env,
jclass clss,
jint file_id)
{
    intn rval;

    rval = Hclose((int32) file_id);

    if (rval == FAIL)  {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}


JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_HDFopen
( JNIEnv *env,
jclass clss,
jstring filename,
jint access,
jshort n_dds)
{
    int32 rval;
    char * str;

        str =(char *) ENVPTR->GetStringUTFChars(ENVPAR filename,0);

    rval = HDFopen((char *)str, (intn) access, (int16) n_dds);

    ENVPTR->ReleaseStringUTFChars(ENVPAR filename,str);

    return rval;
}


#ifdef not_yet_implemented
JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_HDFflusdd
( JNIEnv *env,
jclass clss,
jint file_id)
{
intn rval;

    rval = Hflushdd((int32)file_id);

    if (rval == FAIL)  {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}
#endif

JNIEXPORT jstring JNICALL Java_ncsa_hdf_hdflib_HDFLibrary_HDgetNTdesc
( JNIEnv *env,
jclass clss,
jint nt)
{
char *rval;
jstring rstring;

      rval = HDgetNTdesc((int32) nt);

      if (rval != NULL) {
              rstring = ENVPTR->NewStringUTF(ENVPAR  rval);
      } else {
              rstring = NULL;
      }

      return rstring;
}

#ifdef __cplusplus
}
#endif
