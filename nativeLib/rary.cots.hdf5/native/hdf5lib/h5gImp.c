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
 *  Group Object API Functions of the HDF5 library.
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

/* missing definitions from hdf5.h */
#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE (!FALSE)
#endif

#ifdef __cplusplus
#define ENVPTR (env)
#define ENVPAR 
#else
#define ENVPTR (*env)
#define ENVPAR env,
#endif

#ifdef __cplusplus
herr_t obj_info_all(hid_t loc_id, const char *name, void *opdata);
herr_t H5Gget_obj_info_all(hid_t, char *, char **, int *, unsigned long *);
#else
static herr_t obj_info_all(hid_t loc_id, const char *name, void *opdata);
static herr_t H5Gget_obj_info_all(hid_t, char *, char **, int *, unsigned long *);
#endif

extern jboolean h5outOfMemory( JNIEnv *env, char *functName);
extern jboolean h5JNIFatalError( JNIEnv *env, char *functName);
extern jboolean h5nullArgument( JNIEnv *env, char *functName);
extern jboolean h5libraryError( JNIEnv *env );
extern jboolean h5badArgument( JNIEnv *env, char *functName);


typedef struct info_all
{
    char **objname;
    int *type;
    unsigned long *objno;
    int count;
} info_all_t;


JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Lcreate_1external
  (JNIEnv *env, jclass clss, jstring file_name, jstring object_name, 
   jint loc_id, jstring link_name, jint lcpl, jint lapl)
{
   char *fileName;
   char *objectName;
   char *linkName;
   jboolean isCopy;
   int retCode;

   
    if (file_name == NULL) {
        h5nullArgument( env, "H5Lcreate_external: file_name is NULL");
        return -1;
    }

    if (object_name == NULL) {
        h5nullArgument( env, "H5Lcreate_external: object_name is NULL");
        return -1;
    }

    if (link_name == NULL) {
        h5nullArgument( env, "H5Lcreate_external: link_name is NULL");
        return -1;
    }
  
    
    fileName = (char *)ENVPTR->GetStringUTFChars(ENVPAR file_name,&isCopy);

    if (fileName == NULL) {
        h5JNIFatalError( env, "H5Gcreate:  file name not pinned");
        return -1;
    }

    objectName = (char *)ENVPTR->GetStringUTFChars(ENVPAR object_name,&isCopy);

    if (objectName == NULL) {
        h5JNIFatalError( env, "H5Gcreate:  object name not pinned");
        return -1;
    }

    linkName = (char *)ENVPTR->GetStringUTFChars(ENVPAR link_name,&isCopy);

    if (linkName == NULL) {
        h5JNIFatalError( env, "H5Gcreate:  link name not pinned");
        return -1;
    }


    retCode = H5Lcreate_external(fileName, objectName, loc_id, linkName, lcpl, lapl);
    ENVPTR->ReleaseStringUTFChars(ENVPAR file_name, fileName);
    ENVPTR->ReleaseStringUTFChars(ENVPAR object_name, objectName);
    ENVPTR->ReleaseStringUTFChars(ENVPAR link_name, linkName);
 
    if (retCode < 0) {
        printf("Error code %i\n", retCode);
        h5libraryError(env);
    }
    return retCode;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Gcreate
 * Signature: (ILjava/lang/String;I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Gcreate
  (JNIEnv *env, jclass clss, jint loc_id, jstring name, jint size_hint)
{
    hid_t status;
    char* gName;
    jboolean isCopy;

    if (name == NULL) {
        h5nullArgument( env, "H5Gcreate:  name is NULL");
        return -1;
    }

    gName = (char *)ENVPTR->GetStringUTFChars(ENVPAR name,&isCopy);

    if (gName == NULL) {
        h5JNIFatalError( env, "H5Gcreate:  file name not pinned");
        return -1;
    }

    status = H5Gcreate((hid_t)loc_id, gName, (size_t)size_hint );

    ENVPTR->ReleaseStringUTFChars(ENVPAR name,gName);
    if (status < 0) {
        h5libraryError(env);
    }
    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Gopen
 * Signature: (ILjava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Gopen
  (JNIEnv *env, jclass clss, jint loc_id, jstring name)
{
    hid_t status;
    char* gName;
    jboolean isCopy;

    if (name == NULL) {
        h5nullArgument( env, "H5Gopen:  name is NULL");
        return -1;
    }

    gName = (char *)ENVPTR->GetStringUTFChars(ENVPAR name,&isCopy);

    if (gName == NULL) {
        h5JNIFatalError( env, "H5Gopen:  file name not pinned");
        return -1;
    }

    status = H5Gopen((hid_t)loc_id, gName );

    ENVPTR->ReleaseStringUTFChars(ENVPAR name,gName);
    if (status < 0) {
        h5libraryError(env);
    }
    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Gclose
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5__1H5Gclose
  (JNIEnv *env, jclass clss, jint group_id)
{
    herr_t retVal = 0;

    if (group_id > 0)
        retVal =  H5Gclose((hid_t)group_id) ;

    if (retVal < 0) {
        h5libraryError(env);
    }

    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Glink
 * Signature: (IILjava/lang/String;Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Glink
  (JNIEnv *env, jclass clss, jint loc_id, jint link_type, jstring
    current_name, jstring new_name)
{
    herr_t status;
    char *cName, *nName;
    jboolean isCopy;

    if (current_name == NULL) {
        h5nullArgument( env, "H5Glink:  current_name is NULL");
        return -1;
    }
    if (new_name == NULL) {
        h5nullArgument( env, "H5Glink:  new_name is NULL");
        return -1;
    }
    cName = (char *)ENVPTR->GetStringUTFChars(ENVPAR current_name,&isCopy);
    if (cName == NULL) {
        h5JNIFatalError( env, "H5Glink:  current_name not pinned");
        return -1;
    }
    nName = (char *)ENVPTR->GetStringUTFChars(ENVPAR new_name,&isCopy);
    if (nName == NULL) {
        ENVPTR->ReleaseStringUTFChars(ENVPAR current_name,cName);
        h5JNIFatalError( env, "H5Glink:  new_name not pinned");
        return -1;
    }

    status = H5Glink((hid_t)loc_id, (H5G_link_t)link_type, cName, nName);

    ENVPTR->ReleaseStringUTFChars(ENVPAR new_name,nName);
    ENVPTR->ReleaseStringUTFChars(ENVPAR current_name,cName);

    if (status < 0) {
        h5libraryError(env);
    }
    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Glink
 * Signature: (IILjava/lang/String;Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Glink2
  (JNIEnv *env, jclass clss, 
    jint current_loc_id, jstring current_name, jint link_type, 
    jint new_loc_id, jstring new_name)
{
    herr_t status;
    char *cName, *nName;
    jboolean isCopy;

    if (current_name == NULL) {
        h5nullArgument( env, "H5Glink2:  current_name is NULL");
        return -1;
    }
    if (new_name == NULL) {
        h5nullArgument( env, "H5Glink2:  new_name is NULL");
        return -1;
    }
    cName = (char *)ENVPTR->GetStringUTFChars(ENVPAR current_name,&isCopy);
    if (cName == NULL) {
        h5JNIFatalError( env, "H5Glink2:  current_name not pinned");
        return -1;
    }
    nName = (char *)ENVPTR->GetStringUTFChars(ENVPAR new_name,&isCopy);
    if (nName == NULL) {
        ENVPTR->ReleaseStringUTFChars(ENVPAR current_name,cName);
        h5JNIFatalError( env, "H5Glink2:  new_name not pinned");
        return -1;
    }

    status = H5Glink2((hid_t)current_loc_id, cName, (H5G_link_t)link_type, (hid_t)new_loc_id, nName);

    ENVPTR->ReleaseStringUTFChars(ENVPAR new_name,nName);
    ENVPTR->ReleaseStringUTFChars(ENVPAR current_name,cName);

    if (status < 0) {
        h5libraryError(env);
    }
    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Gunlink
 * Signature: (ILjava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Gunlink
  (JNIEnv *env, jclass clss, jint loc_id, jstring name)
{
    herr_t status;
    char* gName;
    jboolean isCopy;

    if (name == NULL) {
        h5nullArgument( env, "H5Gunlink:  name is NULL");
        return -1;
    }

    gName = (char *)ENVPTR->GetStringUTFChars(ENVPAR name,&isCopy);

    if (gName == NULL) {
        h5JNIFatalError( env, "H5Gunlink:  name not pinned");
        return -1;
    }

    status = H5Gunlink((hid_t)loc_id, gName );

    ENVPTR->ReleaseStringUTFChars(ENVPAR name,gName);

    if (status < 0) {
        h5libraryError(env);
    }
    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Gmove
 * Signature: (ILjava/lang/String;Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Gmove
  (JNIEnv *env, jclass clss, jint loc_id, jstring src, jstring dst)
{
    herr_t status;
    char *sName, *dName;
    jboolean isCopy;

    if (src == NULL) {
        h5nullArgument( env, "H5Gmove:  src is NULL");
        return -1;
    }
    if (dst == NULL) {
        h5nullArgument( env, "H5Gmove:  dst is NULL");
        return -1;
    }
    sName = (char *)ENVPTR->GetStringUTFChars(ENVPAR src,&isCopy);
    if (sName == NULL) {
        h5JNIFatalError( env, "H5Gmove:  src not pinned");
        return -1;
    }
    dName = (char *)ENVPTR->GetStringUTFChars(ENVPAR dst,&isCopy);
    if (dName == NULL) {
        ENVPTR->ReleaseStringUTFChars(ENVPAR src,sName);
        h5JNIFatalError( env, "H5Gmove:  dst not pinned");
        return -1;
    }

    status = H5Gmove((hid_t)loc_id, sName, dName );

    ENVPTR->ReleaseStringUTFChars(ENVPAR dst,dName);
    ENVPTR->ReleaseStringUTFChars(ENVPAR src,sName);
    if (status < 0) {
        h5libraryError(env);
    }
    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Gget_objinfo
 * Signature: (ILjava/lang/String;Z[J[I)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Gget_1objinfo
  (JNIEnv *env, jclass clss, jint loc_id, jstring name, jboolean follow_link,
  jlongArray fileno, jlongArray objno, jintArray link_info, jlongArray mtime)
{
    char* gName;
    jboolean isCopy;
    herr_t retVal;
    jint *linkInfo;
    jlong *fileInfo, *objInfo, *timeInfo;
    hbool_t follow;
    H5G_stat_t h5gInfo;

    if (name == NULL) {
        h5nullArgument( env, "H5Gget_objinfo:  name is NULL");
        return -1;
    }
    if (follow_link == JNI_TRUE) {
        follow = TRUE;  /*  HDF5 'TRUE' */
    } else if (follow_link == JNI_FALSE) {
        follow = FALSE;  /*  HDF5 'FALSE' */
    } else {
        h5badArgument( env, "H5Gget_objinfo:  follow_link is invalid");
        return -1;
    }
    if (fileno == NULL) {
        h5nullArgument( env, "H5Gget_objinfo:  fileno is NULL");
        return -1;
    }
    if (ENVPTR->GetArrayLength(ENVPAR fileno) < 2) {
        h5badArgument( env, "H5Gget_objinfo:  fileno input array < 2");
        return -1;
    }
    if (objno == NULL) {
        h5nullArgument( env, "H5Gget_objinfo:  objno is NULL");
        return -1;
    }
    if (ENVPTR->GetArrayLength(ENVPAR objno) < 2) {
        h5badArgument( env, "H5Gget_objinfo:  objno input array < 2");
        return -1;
    }
    if (link_info == NULL) {
        h5nullArgument( env, "H5Gget_objinfo:  link_info is NULL");
        return -1;
    }
    if (ENVPTR->GetArrayLength(ENVPAR link_info) < 3) {
        h5badArgument( env, "H5Gget_objinfo:  link_info input array < 3");
        return -1;
    }
    if (mtime == NULL) {
        h5nullArgument( env, "H5Gget_objinfo:  mtime is NULL");
        return -1;
    }

    gName = (char *)ENVPTR->GetStringUTFChars(ENVPAR name,&isCopy);
    if (gName == NULL) {
        h5JNIFatalError( env, "H5Gget_object:  name not pinned");
        return -1;
    }
    fileInfo = (jlong *)ENVPTR->GetLongArrayElements(ENVPAR fileno,&isCopy);
    if (fileInfo == NULL) {
        ENVPTR->ReleaseStringUTFChars(ENVPAR name,gName);
        h5JNIFatalError( env, "H5Gget_object:  fileno not pinned");
        return -1;
    }
    objInfo = (jlong *)ENVPTR->GetLongArrayElements(ENVPAR objno,&isCopy);
    if (objInfo == NULL) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR fileno,fileInfo,JNI_ABORT);
        ENVPTR->ReleaseStringUTFChars(ENVPAR name,gName);
        h5JNIFatalError( env, "H5Gget_object:  objno not pinned");
        return -1;
    }
    linkInfo = (jint *)ENVPTR->GetIntArrayElements(ENVPAR link_info,&isCopy);
    if (linkInfo == NULL) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR objno,objInfo,JNI_ABORT);
        ENVPTR->ReleaseLongArrayElements(ENVPAR fileno,fileInfo,JNI_ABORT);
        ENVPTR->ReleaseStringUTFChars(ENVPAR name,gName);
        h5JNIFatalError( env, "H5Gget_object:  link_info not pinned");
        return -1;
    }
    timeInfo = (jlong *)ENVPTR->GetLongArrayElements(ENVPAR mtime,&isCopy);
    if (timeInfo == NULL) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR link_info,linkInfo,JNI_ABORT);
        ENVPTR->ReleaseLongArrayElements(ENVPAR objno,objInfo,JNI_ABORT);
        ENVPTR->ReleaseLongArrayElements(ENVPAR fileno,fileInfo,JNI_ABORT);
        ENVPTR->ReleaseStringUTFChars(ENVPAR name,gName);
        h5JNIFatalError( env, "H5Gget_object:  mtime not pinned");
        return -1;
    }

    retVal = H5Gget_objinfo((hid_t)loc_id, gName, follow, &h5gInfo);

    if (retVal < 0) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR mtime,timeInfo,JNI_ABORT);
        ENVPTR->ReleaseLongArrayElements(ENVPAR objno,objInfo,JNI_ABORT);
        ENVPTR->ReleaseLongArrayElements(ENVPAR fileno,fileInfo,JNI_ABORT);
        ENVPTR->ReleaseIntArrayElements(ENVPAR link_info,linkInfo,JNI_ABORT);
        ENVPTR->ReleaseStringUTFChars(ENVPAR name,gName);
        h5libraryError(env);
    } else {
        fileInfo[0] = (jlong)h5gInfo.fileno[0];
        fileInfo[1] = (jlong)h5gInfo.fileno[1];
        objInfo[0] = (jlong)h5gInfo.objno[0];
        objInfo[1] = (jlong)h5gInfo.objno[1];
        timeInfo[0] = (jlong)h5gInfo.mtime;
        linkInfo[0] = (jint)h5gInfo.nlink;
        linkInfo[1] = (jint)h5gInfo.type;
        linkInfo[2] = (jint)h5gInfo.linklen;
        ENVPTR->ReleaseLongArrayElements(ENVPAR mtime,timeInfo,0);
        ENVPTR->ReleaseLongArrayElements(ENVPAR objno,objInfo,0);
        ENVPTR->ReleaseLongArrayElements(ENVPAR fileno,fileInfo,0);
        ENVPTR->ReleaseIntArrayElements(ENVPAR link_info,linkInfo,0);
        ENVPTR->ReleaseStringUTFChars(ENVPAR name,gName);
    }
    return (jint)retVal;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Gget_linkval
 * Signature: (ILjava/lang/String;I[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Gget_1linkval
  (JNIEnv *env, jclass clss, jint loc_id, jstring name, jint size, jobjectArray value)
{
    char* gName;
    jboolean isCopy;
    char *lValue;
    jstring str;
    herr_t status;

    if (size < 0) {
        h5badArgument( env, "H5Gget_linkval:  size < 0");
        return -1;
    }
    if (name == NULL) {
        h5nullArgument( env, "H5Gget_linkval:  name is NULL");
        return -1;
    }
    lValue = (char *) malloc(sizeof(char)*size);
    if (lValue == NULL) {
        h5outOfMemory( env, "H5Gget_linkval:  malloc failed ");
        return -1;
    }
    gName = (char *)ENVPTR->GetStringUTFChars(ENVPAR name,&isCopy);
    if (gName == NULL) {
        free(lValue);
        h5JNIFatalError( env, "H5Gget_linkval:  name not pinned");
        return -1;
    }

    status = H5Gget_linkval((hid_t)loc_id, gName, (size_t)size, lValue);

    ENVPTR->ReleaseStringUTFChars(ENVPAR name,gName);
    if (status >= 0)
    {
        /* may throw OutOfMemoryError */
        str = ENVPTR->NewStringUTF(ENVPAR lValue);
        if (str == NULL) {
            /* exception -- fatal JNI error */
            free(lValue);
            h5JNIFatalError( env, "H5Gget_linkval:  return string not created");
            return -1;
        }
        /*  the SetObjectArrayElement may raise exceptions... */
        ENVPTR->SetObjectArrayElement(ENVPAR value,0,(jobject)str);
        free(lValue);
    } else {
        free(lValue);
        h5libraryError(env);
    }

    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Gset_comment
 * Signature: (ILjava/lang/String;Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Gset_1comment
  (JNIEnv *env, jclass clss, jint loc_id, jstring name, jstring comment)
{
    herr_t status;
    char *gName, *gComment;
    jboolean isCopy;

    if (name == NULL) {
        h5nullArgument( env, "H5Gset_comment:  name is NULL");
        return -1;
    }
    if (comment == NULL) {
        h5nullArgument( env, "H5Gset_comment:  comment is NULL");
        return -1;
    }
    gName = (char *)ENVPTR->GetStringUTFChars(ENVPAR name,&isCopy);
    if (gName == NULL) {
        h5JNIFatalError( env, "H5Gset_comment:  name not pinned");
        return -1;
    }
    gComment = (char *)ENVPTR->GetStringUTFChars(ENVPAR comment,&isCopy);
    if (gComment == NULL) {
        ENVPTR->ReleaseStringUTFChars(ENVPAR name,gName);
        h5JNIFatalError( env, "H5Gset_comment:  comment not pinned");
        return -1;
    }

    status = H5Gset_comment((hid_t)loc_id, gName, gComment);

    ENVPTR->ReleaseStringUTFChars(ENVPAR comment,gComment);
    ENVPTR->ReleaseStringUTFChars(ENVPAR name,gName);

    if (status < 0) {
        h5libraryError(env);
    }
    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Gget_comment
 * Signature: (ILjava/lang/String;I[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Gget_1comment
  (JNIEnv *env, jclass clss, jint loc_id, jstring name, jint bufsize,
  jobjectArray comment)
{
    char* gName;
    jboolean isCopy;
    char *gComment;
    jstring str;
    jint status;

    if (bufsize <= 0) {
        h5badArgument( env, "H5Gget_comment:  bufsize <= 0");
        return -1;
    }
    if (name == NULL) {
        h5nullArgument( env, "H5Gget_comment:  name is NULL");
        return -1;
    }
    if (comment == NULL) {
        h5nullArgument( env, "H5Gget_comment:  comment is NULL");
        return -1;
    }
    gComment = (char *)malloc(sizeof(char)*bufsize);
    if (gComment == NULL) {
        /* exception -- out of memory */
        h5outOfMemory( env, "H5Gget_comment:  malloc failed");
        return -1;
    }
    gName = (char *)ENVPTR->GetStringUTFChars(ENVPAR name,&isCopy);
    if (gName == NULL) {
        free(gComment);
        h5JNIFatalError( env, "H5Gget_comment:  name not pinned");
        return -1;
    }
    status = H5Gget_comment((hid_t)loc_id, gName, (size_t)bufsize, gComment);

    ENVPTR->ReleaseStringUTFChars(ENVPAR name,gName);
    if (status >= 0)
    {
        /*  may throw OutOfMemoryError */
        str = ENVPTR->NewStringUTF(ENVPAR gComment);
        if (str == NULL) {
            free(gComment);
            h5JNIFatalError( env, "H5Gget_comment:  return string not allocated");
            return -1;
        }
        /*  The SetObjectArrayElement may raise exceptions */
        ENVPTR->SetObjectArrayElement(ENVPAR comment,0,(jobject)str);
        free(gComment);
    } else {
        free(gComment);
        h5libraryError(env);
    }

    return (jint)status;
}


/***************************************************************
 *                   New APIs for HDF5.1.6                     *
 ***************************************************************/

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Gget_num_objs
 * Signature: (I[J[J)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Gget_1num_1objs
  (JNIEnv *env, jclass clss, jint loc_id, jlongArray num_obj)
{
    int status;
    jlong *num_objP;
    jboolean isCopy;
    hsize_t *num_obja;
    int i;
    int rank;

    if (num_obj == NULL) {
        h5nullArgument( env, "H5Gget_num_objs:  num_obj is NULL");
        return -1;
    }

    num_objP = ENVPTR->GetLongArrayElements(ENVPAR num_obj,&isCopy);
    if (num_objP == NULL) {
        h5JNIFatalError(env,  "H5Gget_num_objs:  num_obj not pinned");
        return -1;
    }
    rank = (int) ENVPTR->GetArrayLength(ENVPAR num_obj);
    num_obja = (hsize_t *)malloc( rank * sizeof(hsize_t));
    if (num_obja == NULL)  {
        ENVPTR->ReleaseLongArrayElements(ENVPAR num_obj,num_objP,JNI_ABORT);
        h5JNIFatalError(env,  "H5Gget_num_objs:  num_obj not converted to hsize_t");
        return -1;
    }

    status = H5Gget_num_objs(loc_id, (hsize_t *)num_obja);

    if (status < 0) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR num_obj,num_objP,JNI_ABORT);
        free(num_obja);
        h5libraryError(env);
        return -1;
    }

    for (i = 0; i < rank; i++) {
        num_objP[i] = num_obja[i];
    }
    ENVPTR->ReleaseLongArrayElements(ENVPAR num_obj,num_objP,0);

    free(num_obja);
    return (jint)status;
}

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Gget_objname_by_idx(hid_t group_id, hsize_t idx, char *name, size_t* size )
 * Signature: (IJLjava/lang/String;)J
 */
JNIEXPORT jlong JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Gget_1objname_1by_1idx
  (JNIEnv *env, jclass clss, jint group_id, jlong idx, jobjectArray name, jlong buf_size)
{
    char *aName;
    jstring str;
    hssize_t size;
    long bs;

    bs = (long)buf_size;
    if (bs <= 0) {
        h5badArgument( env, "H5Gget_objname_by_idx:  buf_size <= 0");
        return -1;
    }
    aName = (char*)malloc(sizeof(char)*bs);
    if (aName == NULL) {
        h5outOfMemory(env, "H5Gget_objname_by_idx:  malloc failed");
        return -1;
    }
    size = H5Gget_objname_by_idx((hid_t)group_id, (hsize_t)idx, aName, (size_t)buf_size);
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
        h5JNIFatalError( env,"H5Gget_objname_by_idx:  return string failed");
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
 * Method:    H5Gget_objtype_by_idx(hid_t group_id, hsize_t idx )
 * Signature: (IJLjava/lang/String;)J
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Gget_1objtype_1by_1idx
  (JNIEnv *env, jclass clss, jint group_id, jlong idx)
{
    int type;

    type = H5Gget_objtype_by_idx((hid_t)group_id, (hsize_t)idx );
    if (type < 0) {
        h5libraryError(env);
        /*  exception, returns immediately */
    }

    return (jint)type;
}

/*
/////////////////////////////////////////////////////////////////////////////////
//
//
// Add these methods so that we don't need to call H5Gget_objtype_by_idx
// in a loop to get information for all the object in a group, which takes
// a lot of time to finish if the number of objects is more than 10,000
//
/////////////////////////////////////////////////////////////////////////////////
*/

/*
 * Class:     ncsa_hdf_hdf5lib_H5
 * Method:    H5Gget_obj_info_idx
 * Signature: (ILjava/lang/String;I[I[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5Gget_1obj_1info_1all
  (JNIEnv *env, jclass clss, jint loc_id, jstring group_name,
    jobjectArray objName, jintArray oType, jlongArray oRef, jint n)
{
    herr_t status;
    char *gName=NULL;
    char **oName=NULL;
    jboolean isCopy;
    jstring str;
    jint *tarr;
    jlong *refP;
    unsigned long *refs;
    int i;

    if (group_name == NULL) {
        h5nullArgument( env, "H5Gget_obj_info_all:  group_name is NULL");
        return -1;
    }
    
    if (oType == NULL) {
        h5nullArgument( env, "H5Gget_obj_info_all:  oType is NULL");
        return -1;
    }
    
    if (oRef == NULL) {
        h5nullArgument( env, "H5Gget_obj_info_all:  oRef is NULL");
        return -1;
    }

    gName = (char *)ENVPTR->GetStringUTFChars(ENVPAR group_name,&isCopy);
    if (gName == NULL) {
        h5JNIFatalError( env, "H5Gget_obj_info_all:  group_name not pinned");
        return -1;
    }

    tarr = ENVPTR->GetIntArrayElements(ENVPAR oType,&isCopy);
    if (tarr == NULL) {
        ENVPTR->ReleaseStringUTFChars(ENVPAR group_name,gName);
        h5JNIFatalError( env, "H5Gget_obj_info_all:  type not pinned");
        return -1;
    }
    
    refP = ENVPTR->GetLongArrayElements(ENVPAR oRef,&isCopy);
    if (refP == NULL) {
        ENVPTR->ReleaseStringUTFChars(ENVPAR group_name,gName);
        ENVPTR->ReleaseIntArrayElements(ENVPAR oType,tarr,JNI_ABORT);
        h5JNIFatalError( env, "H5Gget_obj_info_all:  type not pinned");
        return -1;
    }    

    oName = (char **)calloc(n, sizeof (*oName));
    refs = (unsigned long *)malloc(n * sizeof (unsigned long));
    
    status = H5Gget_obj_info_all( (hid_t) loc_id, gName,  oName, (int *)tarr, refs);

    ENVPTR->ReleaseStringUTFChars(ENVPAR group_name,gName);
    if (status < 0) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR oType,tarr,JNI_ABORT);
        ENVPTR->ReleaseLongArrayElements(ENVPAR oRef,refP,JNI_ABORT);
        h5str_array_free(oName, n);
        free(refs);
        h5libraryError(env);
        return -1;
    } 

    if (refs) {
        for (i=0; i<n; i++) {
            refP[i] = (jlong) refs[i];
        }
    }
        
    if (oName) {
        for (i=0; i<n; i++) {
            if (*(oName+i)) {
                str = ENVPTR->NewStringUTF(ENVPAR *(oName+i));
                ENVPTR->SetObjectArrayElement(ENVPAR objName,i,(jobject)str);
            }
        } /* for (i=0; i<n; i++)*/
    }

    free(refs);
    h5str_array_free(oName, n);
    ENVPTR->ReleaseIntArrayElements(ENVPAR oType,tarr,0);
    ENVPTR->ReleaseLongArrayElements(ENVPAR oRef,refP,0);
    
    return (jint)status;
}

herr_t H5Gget_obj_info_all( hid_t loc_id, char *group_name, char **objname, int *type, unsigned long *objno)
{
    herr_t retVal = 0;

    info_all_t info;
    info.objname = objname;
    info.type = type;
    info.objno = objno;
    info.count = 0;
   
    retVal = H5Giterate(loc_id, group_name, NULL, obj_info_all, (void *)&info);
    
    return retVal;
}

herr_t obj_info_all(hid_t loc_id, const char *name, void *opdata)
{
    int type = -1;
    herr_t retVal = 0;
    info_all_t* info = (info_all_t*)opdata;
    H5G_stat_t statbuf;

    retVal = H5Gget_objinfo(loc_id, name, 0, &statbuf);
    
    if (retVal < 0)
    {
        *(info->type+info->count) = -1;
        *(info->objname+info->count) = NULL;
        *(info->objno+info->count) = -1;
    } else {
        *(info->type+info->count) = statbuf.type;
        *(info->objname+info->count) = (char *) malloc(strlen(name)+1);
        strcpy(*(info->objname+info->count), name);
        *(info->objno+info->count) = statbuf.objno[0];
    }
    info->count++;

    return retVal;
}



#ifdef __cplusplus
}
#endif
