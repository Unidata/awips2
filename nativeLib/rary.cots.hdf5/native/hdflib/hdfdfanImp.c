
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

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFDeprecated_DFANaddfds
( JNIEnv *env,
jclass clss,
jint file_id,
jstring description,
jint desc_len)
{
    intn rval;
    char * str;

    str =(char *) ENVPTR->GetStringUTFChars(ENVPAR description,0);

    /* should check that str is as long as desc_length.... */

    rval = DFANaddfds((int32) file_id, (char *)str, (int32) desc_len);

    ENVPTR->ReleaseStringUTFChars(ENVPAR description,str);

    if (rval == FAIL) {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFDeprecated_DFANaddfid
( JNIEnv *env,
jclass clss,
jint file_id,
jstring label)
{
    intn rval;
    char *str;

    str =(char *) ENVPTR->GetStringUTFChars(ENVPAR label,0);

    /* should check that str is as long as desc_length.... */

    rval = DFANaddfid((int32) file_id, (char *)str);

    ENVPTR->ReleaseStringUTFChars(ENVPAR label,str);

    if (rval == FAIL) {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}


JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFDeprecated_DFANclear
( JNIEnv *env,
jobject obj)
{
    intn rval;
    rval = DFANclear( );
    if (rval == FAIL) {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFDeprecated_DFANgetdesc
( JNIEnv *env,
jclass clss,
jstring filename,
jshort tag,
jshort ref,
jobjectArray desc_buf,  /* OUT:  String */
jint buf_len)
{
    intn rval;
    char  *data;
    char  *str;
    jclass Sjc;
    jstring rstring;
    jobject o;
    jboolean bb;


    data = (char *)HDmalloc((buf_len*sizeof(char)) + 1);

    if (data == NULL) {
        /* Exception */
        h4outOfMemory(env,"DFANgetdesc");
        return JNI_FALSE;
    }

    str =(char *) ENVPTR->GetStringUTFChars(ENVPAR filename,0);

    /* read annotation */
    rval = DFANgetdesc((char *)str, (uint16) tag, (uint16) ref,
        (char *)data, (int32) buf_len);

    data[buf_len] = '\0';

    if (rval == FAIL) {
        if (data != NULL) HDfree((char *)data);
        ENVPTR->ReleaseStringUTFChars(ENVPAR filename,str);
        return JNI_FALSE;
    } else {

        ENVPTR->ReleaseStringUTFChars(ENVPAR filename,str);

        rstring = ENVPTR->NewStringUTF(ENVPAR  data);
        o = ENVPTR->GetObjectArrayElement(ENVPAR desc_buf,0);
        if (o == NULL) {
            if (data != NULL)
                HDfree((char *)data);
            return JNI_FALSE;
        }
        Sjc = ENVPTR->FindClass(ENVPAR  "java/lang/String");
        if (Sjc == NULL) {
            if (data != NULL)
                HDfree((char *)data);
            return JNI_FALSE;
        }
        bb = ENVPTR->IsInstanceOf(ENVPAR o,Sjc);
        if (bb == JNI_FALSE) {
            if (data != NULL)
                HDfree((char *)data);
            return JNI_FALSE;
        }
        ENVPTR->SetObjectArrayElement(ENVPAR desc_buf,0,(jobject)rstring);

        if (data != NULL)
            HDfree((char *)data);
        return JNI_TRUE;
    }
}

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFDeprecated_DFANgetdesclen
( JNIEnv *env,
jclass clss,
jstring filename,
jshort tag,
jshort ref)
{
    int32 rval;
    char * str;

    str =(char *) ENVPTR->GetStringUTFChars(ENVPAR filename,0);

    rval = DFANgetdesclen((char *)str, (uint16) tag, (uint16) ref);

    ENVPTR->ReleaseStringUTFChars(ENVPAR filename,str);

    return rval;
}

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFDeprecated_DFANgetfds
( JNIEnv *env,
jclass clss,
jint file_id,
jobjectArray desc_buf,  /* OUT: String */
jint buf_len,
jint isfirst)
{
    int32 rval;
    char  *data;
    jclass Sjc;
    jstring rstring;
    jobject o;
    jboolean bb;


    data = (char *)HDmalloc((buf_len + 1)*sizeof(char));

    if (data == NULL) {
        /* Exception */
        h4outOfMemory(env,"DFANgetfds");
        return FAIL;
    }

    /* should check that data is 'buf_len' long */

    rval = DFANgetfds((int32) file_id, (char *)data, (int32) buf_len, (intn) isfirst);
    data[buf_len] = '\0';

    if (rval == FAIL) {
        if (data != NULL)
            HDfree((char *)data);
    } else {

        rstring = ENVPTR->NewStringUTF(ENVPAR  data);
        o = ENVPTR->GetObjectArrayElement(ENVPAR desc_buf,0);
        if (o == NULL) {
            if (data != NULL)
                HDfree((char *)data);
            return FAIL;
        }
        Sjc = ENVPTR->FindClass(ENVPAR  "java/lang/String");
        if (Sjc == NULL) {
            if (data != NULL)
                HDfree((char *)data);
            return FAIL;
        }
        bb = ENVPTR->IsInstanceOf(ENVPAR o,Sjc);
        if (bb == JNI_FALSE) {
            if (data != NULL)
                HDfree((char *)data);
            return FAIL;
        }
        ENVPTR->SetObjectArrayElement(ENVPAR desc_buf,0,(jobject)rstring);

        if (data != NULL)
            HDfree((char *)data);

    }
    return rval;

}

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFDeprecated_DFANgetfdslen
( JNIEnv *env,
jclass clss,
jint file_id,
jint isfirst)
{
    return DFANgetfdslen((int32) file_id, (intn) isfirst);
}

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFDeprecated_DFANgetfid
( JNIEnv *env,
jclass clss,
jint file_id,
jobjectArray desc_buf,
jint buf_len,
jint isfirst)
{
    int32 rval;
    char  *data;
    jclass Sjc;
    jstring rstring;
    jobject o;
    jboolean bb;


    data = (char *)HDmalloc((buf_len + 1)*sizeof(char));

    if (data == NULL) {
        /* Exception */
        h4outOfMemory(env,"DFANgetfid");
        return FAIL;
    }

    /* should check that data is 'buf_len' long */

    rval = DFANgetfid((int32) file_id, (char *)data,  (int32) buf_len, (intn) isfirst);
    data[buf_len] = '\0';

    if (rval == FAIL) {
        if (data != NULL)
            HDfree((char *)data);
    } else {

        rstring = ENVPTR->NewStringUTF(ENVPAR  data);
        o = ENVPTR->GetObjectArrayElement(ENVPAR desc_buf,0);
        if (o == NULL) {
            if (data != NULL)
                HDfree((char *)data);
            return FAIL;
        }
        Sjc = ENVPTR->FindClass(ENVPAR  "java/lang/String");
        if (Sjc == NULL) {
            if (data != NULL)
                HDfree((char *)data);
            return FAIL;
        }
        bb = ENVPTR->IsInstanceOf(ENVPAR o,Sjc);
        if (bb == JNI_FALSE) {
            if (data != NULL)
                HDfree((char *)data);
            return FAIL;
        }
        ENVPTR->SetObjectArrayElement(ENVPAR desc_buf,0,(jobject)rstring);

        if (data != NULL)
            HDfree((char *)data);
    }
    return rval;
}

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFDeprecated_DFANgetfidlen
( JNIEnv *env,
jclass clss,
jint file_id,
jint isfirst)
{
    return(DFANgetfidlen((int32) file_id, (intn) isfirst));
}

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFDeprecated_DFANgetlabel
( JNIEnv *env,
jclass clss,
jstring filename,
jshort tag,
jshort ref,
jobjectArray label_buf,
jint buf_len)
{
    intn rval;
    char  *data;
    jclass Sjc;
    jstring rstring;
    char *str;
    jobject o;
    jboolean bb;


    data = (char *)HDmalloc((buf_len + 1)*sizeof(char));

    if (data == NULL) {
        /* Exception */
        h4outOfMemory(env,"DFANgetlabel");
        return FAIL;
    }

    /* should check lenght of buffer */

    str =(char *) ENVPTR->GetStringUTFChars(ENVPAR filename,0);

    rval = DFANgetlabel((char *)str, (uint16) tag, (uint16) ref,
        (char *)data, (int32) buf_len);

    data[buf_len] = '\0';

    if (rval == FAIL) {
        if (data != NULL)
            HDfree((char *)data);
        ENVPTR->ReleaseStringUTFChars(ENVPAR filename,str);
    } else {

        ENVPTR->ReleaseStringUTFChars(ENVPAR filename,str);
        rstring = ENVPTR->NewStringUTF(ENVPAR  data);
        o = ENVPTR->GetObjectArrayElement(ENVPAR label_buf,0);
        if (o == NULL) {
            if (data != NULL)
                HDfree((char *)data);
            return FAIL;
        }
        Sjc = ENVPTR->FindClass(ENVPAR  "java/lang/String");
        if (Sjc == NULL) {
            if (data != NULL)
                HDfree((char *)data);
            return FAIL;
        }
        bb = ENVPTR->IsInstanceOf(ENVPAR o,Sjc);
        if (bb == JNI_FALSE) {
            if (data != NULL)
                HDfree((char *)data);
            return FAIL;
        }
        ENVPTR->SetObjectArrayElement(ENVPAR label_buf,0,(jobject)rstring);

        if (data != NULL)
            HDfree((char *)data);
    }
    return rval;
}

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFDeprecated_DFANgetlablen
( JNIEnv *env,
jclass clss,
jstring filename,
jshort tag,
jshort ref)
{
    int32 rval;
    char *str;

    str =(char *) ENVPTR->GetStringUTFChars(ENVPAR filename,0);

    rval = DFANgetlablen((char *)str, (uint16) tag, (uint16) ref);

    ENVPTR->ReleaseStringUTFChars(ENVPAR filename,str);

    return rval;

}

JNIEXPORT jint JNICALL Java_ncsa_hdf_hdflib_HDFDeprecated_DFANlablist
( JNIEnv *env,
jclass clss,
jstring filename,
jshort tag,
jshortArray ref_list,  /* OUT:  short[] */
jobjectArray label_list, /* OUT: String */
jint list_len,
jint label_len,
jint start_pos)
{
    int rval;
    char  *data;
    jclass Sjc;
    jstring rstring;
    char *str;
    jshort *sarr;
    jboolean bb;
    jobject o;

    /*  Check that ref_list is long enough? */

    data = (char *)HDmalloc((label_len * (list_len - 1)) + 1);

    if (data == NULL) {
        /* Exception */
        h4outOfMemory(env,"DFANlablist");
        return FAIL;
    }


    sarr = ENVPTR->GetShortArrayElements(ENVPAR ref_list,&bb);

    /* should check length of buffer */
    str =(char *) ENVPTR->GetStringUTFChars(ENVPAR filename,0);


    rval = DFANlablist((char *)str, (uint16) tag, (uint16 *) sarr,
        (char *)data, (int) list_len, (intn) label_len, (intn) start_pos);

    data[(label_len * (list_len - 1))] = '\0';

    if (rval == FAIL) {
        if (data != NULL)
            HDfree((char *)data);
        ENVPTR->ReleaseStringUTFChars(ENVPAR filename,str);
        ENVPTR->ReleaseShortArrayElements(ENVPAR ref_list,(jshort *)sarr, JNI_ABORT);
    } else {

        ENVPTR->ReleaseStringUTFChars(ENVPAR filename,str);
        ENVPTR->ReleaseShortArrayElements(ENVPAR ref_list,(jshort *)sarr, 0);

        rstring = ENVPTR->NewStringUTF(ENVPAR  data);
        o = ENVPTR->GetObjectArrayElement(ENVPAR label_list,0);
        if (o == NULL) {
            if (data != NULL)
                HDfree((char *)data);
            return FAIL;
        }
        Sjc = ENVPTR->FindClass(ENVPAR  "java/lang/String");
        if (Sjc == NULL) {
            if (data != NULL)
                HDfree((char *)data);
            return FAIL;
        }
        bb = ENVPTR->IsInstanceOf(ENVPAR o,Sjc);
        if (bb == JNI_FALSE) {
            if (data != NULL)
                HDfree((char *)data);
            return FAIL;
        }
        ENVPTR->SetObjectArrayElement(ENVPAR label_list,0,(jobject)rstring);

        if (data != NULL)
            HDfree((char *)data);
    }
    return rval;
}

JNIEXPORT jshort JNICALL Java_ncsa_hdf_hdflib_HDFDeprecated_DFANlastref
( JNIEnv *env,
jobject obj)
{
    return (DFANlastref( ));
}

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFDeprecated_DFANputdesc
( JNIEnv *env,
jclass clss,
jstring filename,
jshort tag,
jshort ref,
jstring description,
jint desc_len)
{
    int rval;
    char *fn;
    char *str;

    fn =(char *) ENVPTR->GetStringUTFChars(ENVPAR filename,0);
    str =(char *) ENVPTR->GetStringUTFChars(ENVPAR description,0);

    /* should check length of description */

    rval = DFANputdesc((char *)fn, (uint16) tag, (uint16) ref,
        (char *)str, (int32) desc_len);

    ENVPTR->ReleaseStringUTFChars(ENVPAR filename,fn);
    ENVPTR->ReleaseStringUTFChars(ENVPAR description,str);

    if (rval == FAIL) {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}

JNIEXPORT jboolean JNICALL Java_ncsa_hdf_hdflib_HDFDeprecated_DFANputlabel
( JNIEnv *env,
jclass clss,
jstring filename,
jshort tag,
jshort ref,
jstring label)
{
    intn rval;
    char *fn;
    char *str;

    fn =(char *) ENVPTR->GetStringUTFChars(ENVPAR filename,0);
    str =(char *) ENVPTR->GetStringUTFChars(ENVPAR label,0);

    /* should check length of description */

    rval = DFANputlabel((char *)fn, (uint16) tag, (uint16) ref, (char *)str);

    ENVPTR->ReleaseStringUTFChars(ENVPAR filename,fn);
    ENVPTR->ReleaseStringUTFChars(ENVPAR label,str);

    if (rval == FAIL) {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}


#ifdef __cplusplus
}
#endif
