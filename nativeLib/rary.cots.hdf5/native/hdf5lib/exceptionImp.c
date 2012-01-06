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
 *  This is a utility program used by the HDF Java-C wrapper layer to
 *  generate exceptions.  This may be called from any part of the
 *  Java-C interface.
 *
 */
#ifdef __cplusplus
extern "C" {
#endif

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>
#include "jni.h"

#ifdef __cplusplus
#define ENVPTR (env)
#define ENVPAR 
#else
#define ENVPTR (*env)
#define ENVPAR env,
#endif

/*
#include "H5Eprivate.h"
*/
/*  These types are copied from H5Eprivate.h
 *  They should be moved to a public include file, and deleted from
 *  here.
 */
#define H5E_NSLOTS      32      /*number of slots in an error stack */
/*
* The list of error messages in the system is kept as an array of
* error_code/message pairs, one for major error numbers and another for
* minor error numbers.
*/
typedef struct H5E_major_mesg_t {
    H5E_major_t error_code;
    const char  *str;
} H5E_major_mesg_t;

typedef struct H5E_minor_mesg_t {
    H5E_minor_t error_code;
    const char  *str;
} H5E_minor_mesg_t;

/* major and minor error numbers */
typedef struct H5E_num_t {
    int maj_num;
    int min_num;
} H5E_num_t;

int getMajorErrorNumber();
int getMinorErrorNumber();

/* get the major and minor error numbers on the top of the erroe stack */
static
herr_t walk_error_callback(int n, H5E_error_t *err_desc, void *_err_nums)
{
    H5E_num_t *err_nums = (H5E_num_t *)_err_nums;

    if (err_desc) {
        err_nums->maj_num = err_desc->maj_num;
        err_nums->min_num = err_desc->min_num;
    }

    return 0;
}


char *defineHDF5LibraryException(int maj_num);


/*
 * Class:     ncsa_hdf_hdf5lib_exceptions_HDF5Library
 * Method:    H5error_off
 * Signature: ()I
 *
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_H5_H5error_1off
  (JNIEnv *env, jclass clss )
{
    return H5Eset_auto(NULL, NULL);
}


/*
 * Class:     ncsa_hdf_hdf5lib_exceptions_HDFLibraryException
 * Method:    printStackTrace0
 * Signature: (Ljava/lang/Object;)V
 *
 *  Call the HDF-5 library to print the HDF-5 error stack to 'file_name'.
 */
JNIEXPORT void JNICALL Java_ncsa_hdf_hdf5lib_exceptions_HDF5LibraryException_printStackTrace0
  (JNIEnv *env, jobject obj, jstring file_name)
{
    FILE *stream;
    char *file;

    if (file_name == NULL)
        H5Eprint(stderr);
    else
    {
        file = (char *)ENVPTR->GetStringUTFChars(ENVPAR file_name,0);
        stream = fopen(file, "a+");
        H5Eprint(stream);
        ENVPTR->ReleaseStringUTFChars(ENVPAR file_name, file);
        if (stream) fclose(stream);
    }
}

/*
 * Class:     ncsa_hdf_hdf5lib_exceptions_HDFLibraryException
 * Method:    getMajorErrorNumber
 * Signature: ()I
 *
 *  Extract the HDF-5 major error number from the HDF-5 error stack.
 *
 *  Note:  This relies on undocumented, 'private' code in the HDF-5
 *  library.  Later releases will have a public interface for this
 *  purpose.
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_exceptions_HDF5LibraryException_getMajorErrorNumber
  (JNIEnv *env, jobject obj)
{
    H5E_num_t err_nums;

    H5Ewalk(H5E_WALK_DOWNWARD, walk_error_callback, &err_nums);

    return (int) err_nums.maj_num;
}

int getMajorErrorNumber()
{
    H5E_num_t err_nums;

    H5Ewalk(H5E_WALK_DOWNWARD, walk_error_callback, &err_nums);

    return (int) err_nums.maj_num;
}

/*
 * Class:     ncsa_hdf_hdf5lib_exceptions_HDFLibraryException
 * Method:    getMinorErrorNumber
 * Signature: ()I
 *
 *  Extract the HDF-5 minor error number from the HDF-5 error stack.
 *
 *  Note:  This relies on undocumented, 'private' code in the HDF-5
 *  library.  Later releases will have a public interface for this
 *  purpose.
 */
JNIEXPORT jint JNICALL Java_ncsa_hdf_hdf5lib_exceptions_HDF5LibraryException_getMinorErrorNumber
  (JNIEnv *env, jobject obj)
{
    return (jint) getMinorErrorNumber();
}

int getMinorErrorNumber()
{
    H5E_num_t err_nums;

    H5Ewalk(H5E_WALK_DOWNWARD, walk_error_callback, &err_nums);

    return (int) err_nums.min_num;
}

/*
 *  Routine to raise particular Java exceptions from C
 */

/*
 *  Create and throw an 'outOfMemoryException'
 *
 *  Note:  This routine never returns from the 'throw',
 *  and the Java native method immediately raises the
 *  exception.
 */
jboolean h5outOfMemory( JNIEnv *env, char *functName)
{
    jmethodID jm;
    jclass jc;
    char * args[2];
    jobject ex;
    jstring str;
    int rval;

    jc = ENVPTR->FindClass(ENVPAR "java/lang/OutOfMemoryError");
    if (jc == NULL) {
        return JNI_FALSE;
    }
    jm = ENVPTR->GetMethodID(ENVPAR jc, "<init>", "(Ljava/lang/String;)V");
    if (jm == NULL) {
        return JNI_FALSE;
    }

    str = ENVPTR->NewStringUTF(ENVPAR functName);
    args[0] = (char *)str;
    args[1] = 0;

    ex = ENVPTR->NewObjectA (ENVPAR jc, jm, (jvalue *)args );
    rval = ENVPTR->Throw(ENVPAR (jthrowable ) ex );
    if (rval < 0) {
        printf("FATAL ERROR:  OutOfMemoryError: Throw failed\n");
        return JNI_FALSE;
    }

    return JNI_TRUE;
}


/*
 *  A fatal error in a JNI call
 *  Create and throw an 'InternalError'
 *
 *  Note:  This routine never returns from the 'throw',
 *  and the Java native method immediately raises the
 *  exception.
 */
jboolean h5JNIFatalError( JNIEnv *env, char *functName)
{
    jmethodID jm;
    jclass jc;
    char * args[2];
    jobject ex;
    jstring str;
    int rval;

    jc = ENVPTR->FindClass(ENVPAR "java/lang/InternalError");
    if (jc == NULL) {
        return JNI_FALSE;
    }
    jm = ENVPTR->GetMethodID(ENVPAR jc, "<init>", "(Ljava/lang/String;)V");
    if (jm == NULL) {
        return JNI_FALSE;
    }

    str = ENVPTR->NewStringUTF(ENVPAR functName);
    args[0] = (char *)str;
    args[1] = 0;
    ex = ENVPTR->NewObjectA (ENVPAR jc, jm, (jvalue *)args );
    rval = ENVPTR->Throw(ENVPAR  (jthrowable) ex );
    if (rval < 0) {
        printf("FATAL ERROR:  JNIFatal: Throw failed\n");
        return JNI_FALSE;
    }

    return JNI_TRUE;
}

/*
 *  A NULL argument in an HDF5 call
 *  Create and throw an 'NullPointerException'
 *
 *  Note:  This routine never returns from the 'throw',
 *  and the Java native method immediately raises the
 *  exception.
 */
jboolean h5nullArgument( JNIEnv *env, char *functName)
{
    jmethodID jm;
    jclass jc;
    char * args[2];
    jobject ex;
    jstring str;
    int rval;

    jc = ENVPTR->FindClass(ENVPAR "java/lang/NullPointerException");
    if (jc == NULL) {
        return JNI_FALSE;
    }
    jm = ENVPTR->GetMethodID(ENVPAR jc, "<init>", "(Ljava/lang/String;)V");
    if (jm == NULL) {
        return JNI_FALSE;
    }

    str = ENVPTR->NewStringUTF(ENVPAR functName);
    args[0] = (char *)str;
    args[1] = 0;
    ex = ENVPTR->NewObjectA (ENVPAR jc, jm, (jvalue *)args );
    rval = ENVPTR->Throw(ENVPAR (jthrowable) ex );

    if (rval < 0) {
        printf("FATAL ERROR:  NullPoitner: Throw failed\n");
        return JNI_FALSE;
    }

    return JNI_TRUE;
}

/*
 *  A bad argument in an HDF5 call
 *  Create and throw an 'IllegalArgumentException'
 *
 *  Note:  This routine never returns from the 'throw',
 *  and the Java native method immediately raises the
 *  exception.
 */
jboolean h5badArgument( JNIEnv *env, char *functName)
{
    jmethodID jm;
    jclass jc;
    char * args[2];
    jobject ex;
    jstring str;
    int rval;

    jc = ENVPTR->FindClass(ENVPAR "java/lang/IllegalArgumentException");
    if (jc == NULL) {
        return JNI_FALSE;
    }
    jm = ENVPTR->GetMethodID(ENVPAR jc, "<init>", "(Ljava/lang/String;)V");
    if (jm == NULL) {
        return JNI_FALSE;
    }

    str = ENVPTR->NewStringUTF(ENVPAR functName);
    args[0] = (char *)str;
    args[1] = 0;
    ex = ENVPTR->NewObjectA (ENVPAR jc, jm, (jvalue *)args );
    rval = ENVPTR->Throw(ENVPAR (jthrowable) ex );
    if (rval < 0) {
        printf("FATAL ERROR:  BadArgument: Throw failed\n");
        return JNI_FALSE;
    }

    return JNI_TRUE;
}

/*
 *  Some feature Not implemented yet
 *  Create and throw an 'UnsupportedOperationException'
 *
 *  Note:  This routine never returns from the 'throw',
 *  and the Java native method immediately raises the
 *  exception.
 */
jboolean h5unimplemented( JNIEnv *env, char *functName)
{
    jmethodID jm;
    jclass jc;
    char * args[2];
    jobject ex;
    jstring str;
    int rval;

    jc = ENVPTR->FindClass(ENVPAR "java/lang/UnsupportedOperationException");
    if (jc == NULL) {
        return JNI_FALSE;
    }
    jm = ENVPTR->GetMethodID(ENVPAR jc, "<init>", "(Ljava/lang/String;)V");
    if (jm == NULL) {
        return JNI_FALSE;
    }

    str = ENVPTR->NewStringUTF(ENVPAR functName);
    args[0] = (char *)str;
    args[1] = 0;
    ex = ENVPTR->NewObjectA (ENVPAR jc, jm, (jvalue *)args );
    rval = ENVPTR->Throw(ENVPAR (jthrowable) ex );
    if (rval < 0) {
        printf("FATAL ERROR:  Unsupported: Throw failed\n");
        return JNI_FALSE;
    }

    return JNI_TRUE;
}

/*
 *  h5libraryError()   determines the HDF-5 major error code
 *  and creates and throws the appropriate sub-class of
 *  HDF5LibraryException().  This routine should be called
 *  whenever a call to the HDF-5 library fails, i.e., when
 *  the return is -1.
 *
 *  Note:  This routine never returns from the 'throw',
 *  and the Java native method immediately raises the
 *  exception.
 */
jboolean h5libraryError( JNIEnv *env )
{
    jmethodID jm;
    jclass jc;
    char *args[2];
    char *exception;
    jobject ex;
    jstring str;
    char *msg;
    int rval, min_num, maj_num;
    unsigned majnum, minnum, relnum;

    H5get_libversion(&majnum, &minnum, &relnum);

    maj_num = (int)getMajorErrorNumber();
    exception = (char *)defineHDF5LibraryException(maj_num);

    jc = ENVPTR->FindClass(ENVPAR exception);
    if (jc == NULL) {
        return JNI_FALSE;
    }
    jm = ENVPTR->GetMethodID(ENVPAR jc, "<init>", "(Ljava/lang/String;)V");
    if (jm == NULL) {
        return JNI_FALSE;
    }

    min_num = (int)getMinorErrorNumber();
    msg = (char *)H5Eget_minor((H5E_minor_t)min_num);
    str = ENVPTR->NewStringUTF(ENVPAR msg);

    args[0] = (char *)str;
    args[1] = 0;
    ex = ENVPTR->NewObjectA (ENVPAR jc, jm, (jvalue *)args );
    rval = ENVPTR->Throw(ENVPAR (jthrowable) ex );
    if (rval < 0) {
        printf("FATAL ERROR:  h5libraryError: Throw failed\n");
        if (msg && majnum>=1 &&  minnum > 6) /* do not free for HDF5 1.6. It is static memory in 1.6 */
            free(msg);
        return JNI_FALSE;
    }

    if (msg && majnum>=1 &&  minnum > 6) /* do not free for HDF5 1.6. It is static memory in 1.6 */
        free(msg);

    return JNI_TRUE;
}


/*  raiseException().  This routine is called to generate
 *  an arbitrary Java exception with a particular message.
 *
 *  Note:  This routine never returns from the 'throw',
 *  and the Java native method immediately raises the
 *  exception.
 */
jboolean h5raiseException( JNIEnv *env, char *exception, char *message)
{
    jmethodID jm;
    jclass jc;
    char * args[2];
    jobject ex;
    jstring str;
    int rval;

    jc = ENVPTR->FindClass(ENVPAR exception);
    if (jc == NULL) {
        return JNI_FALSE;
    }
    jm = ENVPTR->GetMethodID(ENVPAR jc, "<init>", "(Ljava/lang/String;)V");
    if (jm == NULL) {
        return JNI_FALSE;
    }

    str = ENVPTR->NewStringUTF(ENVPAR message);
    args[0] = (char *)str;
    args[1] = 0;
    ex = ENVPTR->NewObjectA (ENVPAR jc, jm, (jvalue *)args );
    rval = ENVPTR->Throw(ENVPAR (jthrowable)ex );
    if (rval < 0) {
        printf("FATAL ERROR:  raiseException: Throw failed\n");
        return JNI_FALSE;
    }

    return JNI_TRUE;
}

/*
jboolean buildException( JNIEnv *env, char *exception, jint HDFerr)
{
    jmethodID jm;
    jclass jc;
    int args[2];
    jobject ex;
    int rval;


    jc = ENVPTR->FindClass(ENVPAR exception);
    if (jc == NULL) {
        return JNI_FALSE;
    }
    jm = ENVPTR->GetMethodID(ENVPAR jc, "<init>", "(I)V");
    if (jm == NULL) {
        return JNI_FALSE;
    }
    args[0] = HDFerr;
    args[1] = 0;

    ex = ENVPTR->NewObjectA (ENVPAR jc, jm, (jvalue *)args );

    rval = ENVPTR->Throw(ENVPAR  ex );
    if (rval < 0) {
        printf("FATAL ERROR:  raiseException: Throw failed\n");
        return JNI_FALSE;
    }

    return JNI_TRUE;
}
*/

/*
 *  defineHDF5LibraryException()  returns the name of the sub-class
 *  which goes with an HDF-5 error code.
 */
char *defineHDF5LibraryException(int maj_num)
{
    H5E_major_t err_num = (H5E_major_t) maj_num;

    if (H5E_ARGS == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5FunctionArgumentException";
    else if (H5E_RESOURCE == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5ResourceUnavailableException";
    else if (H5E_INTERNAL == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5InternalErrorException";
    else if (H5E_FILE == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5FileInterfaceException";
    else if (H5E_IO == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5LowLevelIOException";
    else if (H5E_FUNC == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5FunctionEntryExitException";
    else if (H5E_ATOM == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5AtomException";
    else if (H5E_CACHE == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5MetaDataCacheException";
    else if (H5E_BTREE == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5BtreeException";
    else if (H5E_SYM == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5SymbolTableException";
    else if (H5E_HEAP == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5HeapException";
    else if (H5E_OHDR == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5ObjectHeaderException";
    else if (H5E_DATATYPE == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5DatatypeInterfaceException";
    else if (H5E_DATASPACE == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5DataspaceInterfaceException";
    else if (H5E_DATASET == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5DatasetInterfaceException";
    else if (H5E_STORAGE == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5DataStorageException";
    else if (H5E_PLIST == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5PropertyListInterfaceException";
    else if (H5E_ATTR == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5AttributeException";
    else if (H5E_PLINE == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5DataFiltersException";
    else if (H5E_EFL == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5ExternalFileListException";
    else if (H5E_REFERENCE == err_num)
        return "ncsa/hdf/hdf5lib/exceptions/HDF5ReferenceException";
    return "ncsa/hdf/hdf5lib/exceptions/HDF5LibraryException";
}

#ifdef __cplusplus
}
#endif
