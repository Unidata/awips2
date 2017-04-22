/* -----------------------------------------------------------------------------
FILE NAME
    jni_msg_funcs.c

FILE DESCRIPTION
    This file contains the JNI C code to allow AWIPS II to interface with MHS.

    The main function is Java_com_raytheon_messaging_mhs_MhsMessage_submitMessage,
    which gets its name from the Java class that calls it.  If the class name
    changes at all or is repackaged, then this function must be renamed to match
    the new class name.

HISTORY
    06/01/09    Brian Rapp
    Creation

----------------------------------------------------------------------------- */

static const char Sccsid_msg_object_c[] = "+[-]msg_object.c(CO.DDM) @(#)msg_object.c 7.25 12/15/2004 09:47:04";

#include <ctype.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>

#include <mcMON.h>

#include <co/ddm_symbols.h>
#include <co/msg_object.h>
#include <co/addr_list.h>
#include <co/addr_object.h>
#include <co/retrans_request.h>
#include <co/enclosure.h>
#include <co/enclosure_list.h>

#include <co/ddm/mqrv/API.h>
#include <coDDM.h>
#include <jni.h>

#ifndef SUCCESS
#undef SUCCESS
#endif

/*
#define SUCCESS             0
#define FAIL_TYPE           1
#define FAIL_CREATE         2
#define FAIL_ADDR           3
#define FAIL_ENCLOSE        4
#define FAIL_SUBJECT        5
#define FAIL_PRIORITY       6
#define FAIL_BODY           7
#define FAIL_SUBMIT         8
#define FAIL_MSGID          9
#define FAIL_VALID_TIME     10
#define FAIL_CODE           11
#define FAIL_PRODID         12
#define FAIL_RETRY_COUNT    13
#define FAIL_TIMEOUT_TIME   14
#define FAIL_USER_ID        15
#define FAIL_JNI            16
*/

#define SUCCESS             "Success"
#define FAIL_TYPE           "FailType"
#define FAIL_CREATE         "FailCreate"
#define FAIL_ADDR           "FailAddress"
#define FAIL_ENCLOSE        "FailEnclosure"
#define FAIL_SUBJECT        "FailSubject"
#define FAIL_PRIORITY       "FailPriority"
#define FAIL_BODY           "FailBody"
#define FAIL_SUBMIT         "FailSubmit"
#define FAIL_MSGID          "FailMessageId"
#define FAIL_VALID_TIME     "FailValidTime"
#define FAIL_CODE           "FailHandlerCode"
#define FAIL_PRODID         "FailProductId"
#define FAIL_RETRY_COUNT    "FailRetryCount"
#define FAIL_TIMEOUT_TIME   "FailTimeoutTime"
#define FAIL_USER_ID        "FailUserId"
#define FAIL_JNI            "FailJNI"

#define PACKAGE_CLASS		"com/raytheon/messaging/mhs"
#define JNI_FUNC_NAME(package, funcname) Java_ ## package ## funcname
#define SUBMIT_MESSAGE		JNI_FUNC_NAME(com_raytheon_messaging_mhs_MhsMessage, _submitMessage)
#define STRINGIFY(s)		#s

/*
 * Class:     com_raytheon_messaging_mhs_MhsMessage
 * Method:    submitMessage
 * Signature: (Lcom/raytheon/messaging/mhs/MhsMessage;)I
 */
JNIEXPORT jstring JNICALL SUBMIT_MESSAGE (JNIEnv *, jobject);
/*    Java_com_raytheon_messaging_mhs_MhsMessage_submitMessage (JNIEnv *, jobject); */

void dwbNETsetEnv(int argc, char *argv[]);
static int buffer_file(char *path, char **p_buf, size_t *p_bufsiz);
static long hhmmToUtime(char *hhmm);
int coDDM_setUserID(MsgObject msg, char *user_id);
static int get_message_type(JNIEnv *env, jobject obj, jclass msg_class, int *type);
static int set_msg_addressees(MsgObject the_msg, JNIEnv *env, jobject obj, jclass msg_class);
static int get_priority(JNIEnv *env, jobject obj, jclass msg_class, int *priority);
static int set_enclosures(MsgObject the_msg, JNIEnv *env, jobject obj, jclass msg_class);
static int get_boolean_field(JNIEnv *env, jobject obj, jclass msg_class, char *field_name, int *bool_ptr);
static int get_int_field(JNIEnv *env, jobject obj, jclass msg_class, char *field_name, int *int_ptr);
static int get_string_field(JNIEnv *env, jobject obj, jclass msg_class, char *field_name, char **str_ptr);
void throw_submit_exception(JNIEnv *env, char *result_code, char *result_text);

static int		show_trace;
static char		statbuf[1024];
static jclass		newExcCls;

/* -----------------------------------------------------------------------------

FUNCTION NAME
    Java_com_raytheon_messaging_mhs_MhsMessage_submitMessage

FUNCTION DESCRIPTION
    JNI function for building and submitting an MHS message from Java.  The
    MhsMessage elements (Java) are used to create a MsgObject, which is then
    passed to coDDM_createMsg, which in turn executes a DWB transaction to
    submit the message to the message request daemon.

    submitMessage is called from an instance of the MhsMessage class once all
    needed message parameters have been specified and validated.  If the call
    is successful, 0 is returned.  A positive return value indicates a messaging
    error.  The exact error can be determined from java by looking at the value
    of the "result" member.  More detailed error text is available by calling
    result.getResultText().  A negative return value indicates a problem with
    a JNI call that will most likely result in an exception being thrown upon
    return to the JVM.

MESSAGE PARAMETERS
    showTrace - Boolean value used to print detailed debugging information to
        stdout and stderr.  Defaults to false.

    type - An integer value from 0 to 20 representing the type of message being
        sent.  See com.raytheon.messaging.mhs.MhsMessageType.java for details.
        Defaults to 0.

    productId - An optional product ID string of up to 32 characters.  If provided
        with multiple enclosures, it is only attached to the first enclosure.

    verifyAddressess - Boolean value.  If true, the addressees are run through a
        validation check, but the message is not submitted.  Default is false.

    addressees - A list of destination addresses.  The list can contain any
        combination of sites (WFO/RFC) mnemonics, an NCF facility, or special
        addresses (e.g. - INET, NWWS, RETRANS, DEFAULTNCF).  Every message must
        have at least one addressee.

    actionCode - Integer code between 0 and 255 inclusive that determines what
        action is to be taken by the recipients.  The action is determined by
        lookup from the text configuration file /awips/ops/data/mhs/rcv_handler.tbl.
        This is a required parameter.

    subject - Optional text string that is the subject of the message.  Can be
        up to 129 bytes.

    priority - Integer message priority from 0 to 2 with 2 being highest.  The
        default value is 0.

    bodyFile - Fully qualified file name for an optional message body.  The
        file can be up to 17K in length.

    enclosures - An optional list of enclosure files.  There is no predefined
        limit to the size of an enclosure.

    validTime - An optional date string (mm/dd/YYYY[:HHMM]) containing the
        absolute time at which the product expires.

    timeoutTime - An optional date string containing the absolute time at which
        the message will time out if it has not been delivered.  The maximum
        value is 1200 seconds from the time of submission.

    retryCount - An optional integer value containing the number of retries that
        will be attempted if the message cannot be successfully delivered.

    userId - The optional mailbox name of the recipient.  The default is awipsmhs,
        which should never be changed since MHS does not look for any other mail
        boxes.

LOCAL DATA FILES
    None

RETURNS
    An integer status code.  The Java result code is populated.

ERRORS REPORTED
    < Description of errors reported to Monitor and Control >

----------------------------------------------------------------------------- */

JNIEXPORT jstring JNICALL SUBMIT_MESSAGE (JNIEnv *env, jobject obj) {

    const char FUNCNAME[] = "submitMessage";
    MsgObject       the_msg;
    char            *tmpstr;
    char            *msg_id;
    int             check_flag;
    int             actionCode;
    int             retry_count;
    char            *user_id;
    char            *subject;
    char            *valid_time;
    long            valid_time_long;
    char            *timeout_time;
    long            timeout_time_long;
    int             priority;
    int             type;
    char            *product;
    char            *body_file;
    char            *bodybuf	= NULL;
    size_t          bodybufsiz;
    jclass          msg_class;
    jstring         jmsg_id;
    char            buf[200];

    /* Start code ----------------------------------------------------------------------------------- */
    dwbNETsetEnv(0, (char **) NULL);

    msg_class = (*env)->GetObjectClass(env, obj);
    sprintf(buf, "%s/MhsSubmitException", PACKAGE_CLASS);
    if ((newExcCls = (*env)->FindClass(env, buf)) == NULL) {
	sprintf(statbuf, "Could not find Exception Class \"%s/MhsSubmitException\"\n", PACKAGE_CLASS);
        printf(statbuf);
        return((*env)->NewStringUTF(env, statbuf));
    }

    /* Note: To get the various signatures, use this command: javap -s -p com.raytheon.messaging.mhs.MhsMessage */

    /* Get the showTrace flag ----------------------------------------------------------------------- */
    if (!get_boolean_field(env, obj, msg_class, "showTrace", &show_trace)) {
        strcpy(statbuf, "Could not get boolean field \"showTrace\"");
        throw_submit_exception(env, FAIL_JNI, statbuf);
        return((*env)->NewStringUTF(env, statbuf));
    }

    /* Get the message type ------------------------------------------------------------------------- */
    if (!get_message_type(env, obj, msg_class, &type)) {
        strcpy(statbuf, "Could not get message type field");
        throw_submit_exception(env, FAIL_JNI, statbuf);
        return((*env)->NewStringUTF(env, statbuf));
    }

    /* Create the message object -------------------------------------------------------------------- */
    if (!(the_msg = coDDM_createMsg(type))) {
        sprintf (statbuf, "Failed createMsg type=%d\n", type);
        throw_submit_exception(env, FAIL_CREATE, statbuf);
        return((*env)->NewStringUTF(env, statbuf));
    }

    /* Get the productID ---------------------------------------------------------------------------- */
    if (!get_string_field(env, obj, msg_class, "productId", &product)) {
        strcpy(statbuf, "Could not get product ID");
        throw_submit_exception(env, FAIL_JNI, statbuf);
        return((*env)->NewStringUTF(env, statbuf));
    }

    if (strlen(product) > 0) {
        if (show_trace) printf ("Product ID: %s\n", product);
        if (coDDM_setProdID(the_msg, product) < 0) {
            sprintf (statbuf, "Failed setProdID '%s'\n", product);
            free(product);
            throw_submit_exception(env, FAIL_PRODID, statbuf);
            return((*env)->NewStringUTF(env, statbuf));
        }
    }
    free(product);

    /* Get the verifyAddressees flag ---------------------------------------------------------------- */
    if (!get_boolean_field(env, obj, msg_class, "verifyAddressees", &check_flag)) {
        strcpy(statbuf, "Could not get verifyAddressees field");
        throw_submit_exception(env, FAIL_JNI, statbuf);
        return((*env)->NewStringUTF(env, statbuf));
    }
    if (show_trace) printf ("Verify addressees flag: %d\n", check_flag);

    /* Get the Addressees --------------------------------------------------------------------------- */
    if (!set_msg_addressees(the_msg, env, obj, msg_class)) {
        return((*env)->NewStringUTF(env, statbuf));
    }

    if (show_trace) {
        AddrObject  addr;
        for (addr = addrListGetFirstItem(the_msg->addr_list); addr; addr = addrListGetNextItem(the_msg->addr_list)) {
            printf ("Addressee from msg object: %s %s\n", addr->addressee_name, addr->ack_required ? "(A)" : "");
        }
    }

    /* If check flag is set, don't send the message -- only verify the addressees ------------------- */
    if (check_flag) {
        int     bufsiz = 0;
        char    *addrbuf;
        char    *p_addrlist_out;

        for (tmpstr = coDDM_getFirstAddrName(the_msg); tmpstr; tmpstr = coDDM_getNextAddrName(the_msg)) {
            bufsiz += strlen(tmpstr)+1;
        }

        if (!(addrbuf = malloc(bufsiz+1))) {
            sprintf (statbuf, "Could not allocate %d bytes in %s\n", bufsiz+1, FUNCNAME);
            coDDM_destroyMsg(&the_msg);
            throw_submit_exception(env, FAIL_JNI, statbuf);
            return((*env)->NewStringUTF(env, statbuf));
        }

        addrbuf[0] = '\0';
        for (tmpstr = coDDM_getFirstAddrName(the_msg); tmpstr; tmpstr = coDDM_getNextAddrName(the_msg)) {
            if (strlen(addrbuf)) {
                strcat (addrbuf, ",");
            }
            strcat(addrbuf, tmpstr);
        }

        if (show_trace) printf ("check string: %s\n", addrbuf);
        p_addrlist_out = NULL;
        if (mqc_check_addr(the_msg->prodid, addrbuf, &p_addrlist_out) < 0) {
            sprintf (statbuf, "Failed check addr list '%s'\n", addrbuf);
            free (addrbuf);
            coDDM_destroyMsg(&the_msg);
            throw_submit_exception(env, FAIL_ADDR, statbuf);
            return((*env)->NewStringUTF(env, statbuf));
        }

        if (p_addrlist_out) {
            printf("Addr list: %s\n", p_addrlist_out);
        } else {
            printf("\n");
        }
        free (addrbuf);
        coDDM_destroyMsg(&the_msg);
        return((*env)->NewStringUTF(env, strlen(p_addrlist_out) ? p_addrlist_out : "\n"));
    }

    /* Get the message action code ------------------------------------------------------------------ */
    if (!get_int_field(env, obj, msg_class, "actionCode", &actionCode)) {
        coDDM_destroyMsg(&the_msg);
        strcpy(statbuf, "Could not get actionCode");
        throw_submit_exception(env, FAIL_JNI, statbuf);
        return((*env)->NewStringUTF(env, statbuf));
    }

    if (show_trace) printf ("Action Code: %d\n", actionCode);

    if (coDDM_setMsgCode(the_msg, actionCode) < 0) {
        sprintf (statbuf, "Failed setMsgCode '%d'\n", actionCode);
        coDDM_destroyMsg(&the_msg);
        throw_submit_exception(env, FAIL_CODE, statbuf);
        return((*env)->NewStringUTF(env, statbuf));
    }

    /* Get the subject string ----------------------------------------------------------------------- */
    if (!get_string_field(env, obj, msg_class, "subject", &subject)) {
        coDDM_destroyMsg(&the_msg);
        strcpy(statbuf, "Could not get subject");
        throw_submit_exception(env, FAIL_JNI, statbuf);
        return((*env)->NewStringUTF(env, statbuf));
    }

    if (strlen(subject) > 0) {
        if (show_trace) printf ("Message subject: %s\n", subject);
        if (coDDM_setSubject(the_msg, subject) < 0) {
            sprintf (statbuf, "Failed setSubject '%s'\n", subject);
            coDDM_destroyMsg(&the_msg);
            free(subject);
            throw_submit_exception(env, FAIL_SUBJECT, statbuf);
            return((*env)->NewStringUTF(env, statbuf));
        }
    }
    free(subject);

    /* Get the message priority code ---------------------------------------------------------------- */
    if (!get_priority(env, obj, msg_class, &priority)) {
        coDDM_destroyMsg(&the_msg);
        strcpy(statbuf, "Could not get priority");
        throw_submit_exception(env, FAIL_JNI, statbuf);
        return((*env)->NewStringUTF(env, statbuf));
    }

    if (show_trace) printf ("Priority: %d\n", priority);

    if (coDDM_setPriority(the_msg, priority) < 0) {
        sprintf (statbuf, "Failed setPriority '%d'\n", priority);
        coDDM_destroyMsg(&the_msg);
        throw_submit_exception(env, FAIL_PRIORITY, statbuf);
        return((*env)->NewStringUTF(env, statbuf));
    }

    /* Get the body file name ----------------------------------------------------------------------- */
    if (!get_string_field(env, obj, msg_class, "bodyFile", &body_file)) {
        coDDM_destroyMsg(&the_msg);
        strcpy(statbuf, "Could not get bodyFile");
        throw_submit_exception(env, FAIL_JNI, statbuf);
        return((*env)->NewStringUTF(env, statbuf));
    }

    if (strlen(body_file) > 0) {
        if (show_trace) printf ("Message body file name: %s\n", body_file);
        if (buffer_file((char *) body_file, &bodybuf, &bodybufsiz) < 0) {
            free(bodybuf);
            free(body_file);
            coDDM_destroyMsg(&the_msg);
            throw_submit_exception(env, FAIL_BODY, statbuf);
            return((*env)->NewStringUTF(env, statbuf));
        }
        if (show_trace) printf ("Message body: %s", bodybuf);

        if (coDDM_setMsgBody(the_msg, bodybuf, bodybufsiz) < 0) {
            sprintf (statbuf, "Failed setMsgBody '%s'\n", body_file);
            free(bodybuf);
            free(body_file);
            coDDM_destroyMsg(&the_msg);
            throw_submit_exception(env, FAIL_BODY, statbuf);
            return((*env)->NewStringUTF(env, statbuf));
        }
        free(bodybuf);
    }

    free(body_file);

    /* Set the Enclosure list ----------------------------------------------------------------------- */
    if (!set_enclosures(the_msg, env, obj, msg_class)) {
        coDDM_destroyMsg(&the_msg);
        return((*env)->NewStringUTF(env, statbuf));
    }

    if (show_trace) {
        Enclosure   encl;
        for (encl = encListGetFirstItem(the_msg->enc_list); encl; encl = encListGetNextItem(the_msg->enc_list)) {
            printf ("Enclosure file from msg object: %s\n", encl->filename);
        }
    }

    /* Get the valid time string -------------------------------------------------------------------- */
    if (!get_string_field(env, obj, msg_class, "validTimeString", &valid_time)) {
        coDDM_destroyMsg(&the_msg);
        strcpy(statbuf, "Could not get validTimeString");
        throw_submit_exception(env, FAIL_JNI, statbuf);
        return((*env)->NewStringUTF(env, statbuf));
    }

    if (strlen(valid_time) > 0) {
        if (show_trace) printf ("Valid Time String: %s\n", valid_time);
        valid_time_long = hhmmToUtime((char *) valid_time);
        if (coDDM_setValidTime(the_msg, valid_time_long) < 0) {
            sprintf (statbuf, "Failed setValidTime: %s", ctime((const time_t *)&valid_time_long));
            coDDM_destroyMsg(&the_msg);
            free(valid_time);
            throw_submit_exception(env, FAIL_VALID_TIME, statbuf);
            return((*env)->NewStringUTF(env, statbuf));
        }
    }

    free(valid_time);

    /* Get the timeout time string ------------------------------------------------------------------ */
    if (!get_string_field(env, obj, msg_class, "timeoutTimeString", &timeout_time)) {
        coDDM_destroyMsg(&the_msg);
        strcpy(statbuf, "Could not get timeoutTimeString");
        throw_submit_exception(env, FAIL_JNI, statbuf);
        return((*env)->NewStringUTF(env, statbuf));
    }

    if (strlen(timeout_time) > 0) {
        if (show_trace) printf ("Time Out String: %s\n", timeout_time);
        timeout_time_long = hhmmToUtime((char *) timeout_time);
        if (coDDM_setTimeoutTime(the_msg, timeout_time_long) < 0) {
            sprintf (statbuf, "Failed setTimeoutTime: %s", ctime((const time_t *)&timeout_time_long));
            coDDM_destroyMsg(&the_msg);
            free(timeout_time);
            throw_submit_exception(env, FAIL_TIMEOUT_TIME, statbuf);
            return((*env)->NewStringUTF(env, statbuf));
        }
    }

    free(timeout_time);

    /* Get the retry count -------------------------------------------------------------------------- */
    if (!get_int_field(env, obj, msg_class, "retryCount", &retry_count)) {
        coDDM_destroyMsg(&the_msg);
        strcpy(statbuf, "Could not get retryCount");
        throw_submit_exception(env, FAIL_JNI, statbuf);
        return((*env)->NewStringUTF(env, statbuf));
    }

    if (retry_count > 0) {
        if (show_trace) printf ("Retry count = %d\n", retry_count);
        if (coDDM_setRetryCount(the_msg, retry_count) < 0) {
            sprintf (statbuf, "Failed setRetryCount '%d'\n", retry_count);
            coDDM_destroyMsg(&the_msg);
            throw_submit_exception(env, FAIL_RETRY_COUNT, statbuf);
            return((*env)->NewStringUTF(env, statbuf));
        }
    }

    /* Get user ID string --------------------------------------------------------------------------- */
    if (!get_string_field(env, obj, msg_class, "userId", &user_id)) {
        coDDM_destroyMsg(&the_msg);
        strcpy(statbuf, "Could not get userId");
        throw_submit_exception(env, FAIL_JNI, statbuf);
        return((*env)->NewStringUTF(env, statbuf));
    }

    if (strlen(user_id) > 0) {
        if (show_trace) printf ("User ID: %s\n", user_id);
        if (coDDM_setUserID(the_msg, (char *) user_id) < 0) {
            sprintf (statbuf, "Failed setUserID '%s'\n", user_id);
            coDDM_destroyMsg(&the_msg);
            free(user_id);
            throw_submit_exception(env, FAIL_USER_ID, statbuf);
            return((*env)->NewStringUTF(env, statbuf));
        }
    }

    free(user_id);

    /* Submit the message  -------------------------------------------------------------------------- */
    if (coDDM_submitMsg(the_msg) < 0) {
        sprintf (statbuf, "Failed submitMsg\n");
        coDDM_destroyMsg(&the_msg);
        throw_submit_exception(env, FAIL_SUBMIT, statbuf);
        return((*env)->NewStringUTF(env, statbuf));
    }

    if ((msg_id = coDDM_getMsgID(the_msg))) {
        if (show_trace) printf ("Message successfully submitted.  Message ID: %s\n", msg_id);
        jmsg_id = (*env)->NewStringUTF(env, msg_id);
        coDDM_destroyMsg(&the_msg);
        return(jmsg_id);
    } else {
        sprintf (statbuf, "Failed getMsgID\n");
        coDDM_destroyMsg(&the_msg);
        throw_submit_exception(env, FAIL_MSGID, statbuf);
        return((*env)->NewStringUTF(env, statbuf));
    }
}

/* ----------------------------------------------------------------------------------------------

FUNCTION NAME
    buffer_file

FUNCTION DESCRIPTION
    Read the fully-qualified file into a buffer. The buffer is allocated from the heap and
    must be disposed when no longer needed. The buffer and the size are passed by reference.

PARAMETERS
    Type	Name		I/O	Description
    char*	path		I	Fully qualified file name to be read.
    char**	p_buf		O	Buffer that will contain the contents of
					the file 'path'.
    size_t*	p_bufsiz	O	Size of the file buffer in bytes.

LOCAL DATA FILES

RETURNS
	0 on success
	-1 on failure

ERRORS REPORTED


---------------------------------------------------------------------------------------------- */


static int buffer_file(char *path, char **p_buf, size_t *p_bufsiz)
{
    const char FUNCNAME[] = "buffer_file";
    int fd;
    struct stat stbuf;

    ENTER4;

    if ((fd = open(path, O_RDONLY)) < 0) {
        sprintf (statbuf, "Failed open file %s, %s", path, strerror(errno));
        RETURN4(-1);
    }

    if (fstat(fd, &stbuf)) {
        sprintf (statbuf, "Failed stat file %s, %s", path, strerror(errno));
        RETURN4(-1);
    }

    *p_bufsiz = stbuf.st_size;

    if (!(*p_buf = malloc(*p_bufsiz + 1))) {
        sprintf (statbuf, "Failed malloc %d bytes", (int) stbuf.st_size);
        RETURN4(-1);
    }

    if (read(fd, *p_buf, *p_bufsiz) < 0) {
        sprintf(statbuf, "Failed to read %d bytes from %s, %s", (int) stbuf.st_size, path, strerror(errno));
        close(fd);
        free(*p_buf);
        *p_buf = NULL;
        RETURN4(-1);
    }

    *(*p_buf + *p_bufsiz) = '\0';

    close(fd);
    RETURN4(0);
}

/* ----------------------------------------------------------------------------------------------

FUNCTION NAME
    hhmmToUtime

FUNCTION DESCRIPTION
    Reads a date/time string formatted as "mm/dd[/yyyy]:HHMM and returns it as a Unix time value.

PARAMETERS
    Type	Name		I/O	Description
    char*	intime		I	Formatted date/time string.

LOCAL DATA FILES

RETURNS
	Unix date value on success.
	-1 on failure

ERRORS REPORTED


---------------------------------------------------------------------------------------------- */

static long hhmmToUtime(char *intime)
{
    struct tm *p_tm;
    time_t now;
    char *mmddyyyy;
    char *hhmm;

    time(&now);
    p_tm = localtime(&now);

    if (!(mmddyyyy = strtok(intime, ":"))) {
        return -1;
    }

    if ((hhmm = strtok(NULL, ":"))) {
        /* looks like mm/dd[/yyyy]:HHMM */
        if (sscanf(mmddyyyy, "%d/%d/%d", &p_tm->tm_mon, &p_tm->tm_mday, &p_tm->tm_year) < 2) {
            return -1;
        }
        p_tm->tm_mon -= 1;      /* tm month starts at zero */
        if (p_tm->tm_year >= 1900) {
            p_tm->tm_year -= 1900;
        }
    }
    else {
        /* looks like HHMM */
        hhmm = mmddyyyy;
    }
    if (sscanf(hhmm, "%2d%2d",
            &p_tm->tm_hour, &p_tm->tm_min) != 2) {
        return -1;
    }

    return(mktime(p_tm));
}

/* ----------------------------------------------------------------------------------------------

FUNCTION NAME
    get_message_type

FUNCTION DESCRIPTION
    Gets the message type value from the Java message object.
    Finds the "type" field ID and object, then gets the value as an int.

PARAMETERS
    Type	Name		I/O	Description
    JNIEnv*	env		I	Pointer to the JVM environment.
    jobject	obj		I	The MhsMessage object containing the message.
    jclass	msg_class	I	The MhsMessage object's class.
    int*	type		O	Pointer to the type integer used for output to the caller.

LOCAL DATA FILES

RETURNS
	0 on success
	-1 on Failure.  Also throws a MhsSubmitMessage exception back to the JVM.

ERRORS REPORTED

---------------------------------------------------------------------------------------------- */

int get_message_type(JNIEnv *env, jobject obj, jclass msg_class, int *type) {

    jfieldID        type_fid;
    jmethodID       type_value_mid;
    jobject         type_obj;
    jclass          type_class;
    jmethodID       type_text_mid;
    jstring         jtype_text;
    char            *type_text;
    char            buf[200];

    sprintf(buf, "L%s/MhsMessageType;", PACKAGE_CLASS);
    if ((type_fid = (*env)->GetFieldID(env, msg_class, "type", buf)) == NULL) {
        return(0);
    }

    if ((type_obj = (*env)->GetObjectField(env, obj, type_fid)) == NULL) {
        sprintf (statbuf, "Error in call to (*env)->GetObjectField(env, obj, type_fid)\n");
        throw_submit_exception(env, FAIL_JNI, statbuf);
        return(0);
    }

    type_class = (*env)->GetObjectClass(env, type_obj);
    type_value_mid = (*env)->GetMethodID(env, type_class, "value", "()I");
    *type = (*env)->CallIntMethod(env, type_obj, type_value_mid);
    type_text_mid = (*env)->GetMethodID(env, type_class, "text", "()Ljava/lang/String;");
    jtype_text = (*env)->CallObjectMethod(env, type_obj, type_text_mid);
    type_text = (char *) (*env)->GetStringUTFChars(env, jtype_text, NULL);
    if (show_trace) printf ("Type: (%d) %s\n", *type, type_text);
    (*env)->ReleaseStringUTFChars(env, jtype_text, type_text);
    return(1);
}

/* ----------------------------------------------------------------------------------------------

FUNCTION NAME
    set_msg_addressees

FUNCTION DESCRIPTION
    Gets the message addressees from the Java message object and adds them to the C message
    object.

PARAMETERS
    Type	Name		I/O	Description
    MsgObject	the_msg		O	Pointer to the message object.
    JNIEnv*	env		I	Pointer to the JVM environment.
    jobject	obj		I	The MhsMessage object containing the message.
    jclass	msg_class	I	The MhsMessage object's class.

LOCAL DATA FILES

RETURNS
	0 on success
	-1 on Failure.  Also throws a MhsSubmitMessage exception back to the JVM.

ERRORS REPORTED

---------------------------------------------------------------------------------------------- */

int set_msg_addressees(MsgObject the_msg, JNIEnv *env, jobject obj, jclass msg_class) {

    jfieldID        addresslist_fid;
    jobject         addresslist_obj;
    jclass          addresslist_class;
    jmethodID       addresslist_getCount_mid;
    jmethodID       addresslist_get_mid;
    jobject         addressee_obj;
    jclass          addressee_class;
    jmethodID       address_getAddress_mid = NULL;
    jmethodID       address_isAckRequired_mid = NULL;
    jstring         jaddressee;
    char            *addressee;
    int             ack_needed;
    int             addressee_count;
    int             i;
    int             first_time = TRUE;
    char            buf[200];

    sprintf(buf, "L%s/AddresseeList;", PACKAGE_CLASS);
    if ((addresslist_fid = (*env)->GetFieldID(env, msg_class, "addressees", buf)) == NULL) {
	strcpy(statbuf, "Could not get fieldID for addressees");
        throw_submit_exception(env, FAIL_JNI, statbuf);
        return(0);
    }

    if ((addresslist_obj = (*env)->GetObjectField(env, obj, addresslist_fid)) == NULL) {
	strcpy(statbuf, "Could not get objectID for addressees");
        throw_submit_exception(env, FAIL_JNI, statbuf);
        return(0);
    }

    addresslist_class = (*env)->GetObjectClass(env, addresslist_obj);
    addresslist_getCount_mid = (*env)->GetMethodID(env, addresslist_class, "getCount", "()I");
    addressee_count = (*env)->CallIntMethod(env, addresslist_obj, addresslist_getCount_mid);
    if (show_trace) printf ("Addresses: %d\n", addressee_count);

    sprintf(buf, "(I)L%s/Addressee;", PACKAGE_CLASS);
    addresslist_get_mid = (*env)->GetMethodID(env, addresslist_class, "get", buf);
    for (i = 0; i < addressee_count; i++) {
        addressee_obj = (*env)->CallObjectMethod(env, addresslist_obj, addresslist_get_mid, i);
        if (first_time) {
            addressee_class = (*env)->GetObjectClass(env, addressee_obj);
            address_getAddress_mid = (*env)->GetMethodID(env, addressee_class, "getAddress", "()Ljava/lang/String;");
            address_isAckRequired_mid = (*env)->GetMethodID(env, addressee_class, "isAckRequired", "()Z");
            first_time = FALSE;
        }
        jaddressee = (*env)->CallObjectMethod(env, addressee_obj, address_getAddress_mid);
        addressee = (char *) (*env)->GetStringUTFChars(env, jaddressee, NULL);
        ack_needed = (*env)->CallBooleanMethod(env, addressee_obj, address_isAckRequired_mid);
        if (show_trace) printf ("Addressee: %s %s\n", addressee, ack_needed ? "(A)" : "");

        if (coDDM_addAddressee(the_msg, addressee, ack_needed) < 0) {
            coDDM_destroyMsg(&the_msg);
            sprintf (statbuf, "Failed addAddressee '%s'\n", addressee);
            throw_submit_exception(env, FAIL_ADDR, statbuf);
            return(0);
        }
        (*env)->ReleaseStringUTFChars(env, jaddressee, addressee);
    }

    return(1);
}

/* ----------------------------------------------------------------------------------------------

FUNCTION NAME
    get_priority

FUNCTION DESCRIPTION
    Gets the message priority value from the Java message object.
    Finds the "priority" field ID and object, then gets the value as an int.

PARAMETERS
    Type	Name		I/O	Description
    JNIEnv*	env		I	Pointer to the JVM environment.
    jobject	obj		I	The MhsMessage object containing the message.
    jclass	msg_class	I	The MhsMessage object's class.
    int*	type		O	Pointer to the priority integer used for output to the caller.

LOCAL DATA FILES

RETURNS
	0 on success
	-1 on Failure.  Also throws a MhsSubmitMessage exception back to the JVM.

ERRORS REPORTED

---------------------------------------------------------------------------------------------- */

int get_priority(JNIEnv *env, jobject obj, jclass msg_class, int *priority) {

    jfieldID        priority_fid;
    jmethodID       priority_value_methodID;
    jobject         priority_obj;
    jclass          priority_class;
    char            buf[200];

    sprintf(buf, "L%s/MhsMessagePriority;", PACKAGE_CLASS);
    if ((priority_fid = (*env)->GetFieldID(env, msg_class, "priority", buf)) == NULL) {
        return(0);
    }

    if ((priority_obj = (*env)->GetObjectField(env, obj, priority_fid)) == NULL) {
        fprintf (stderr, "Error in call to (*env)->GetObjectField(env, msg_class, priority_fid)\n");
        return(0);
    }

    priority_class = (*env)->GetObjectClass(env, priority_obj);
    priority_value_methodID = (*env)->GetMethodID(env, priority_class, "value", "()I");
    *priority = (*env)->CallIntMethod(env, priority_obj, priority_value_methodID);

    return(1);
}

/* ----------------------------------------------------------------------------------------------

FUNCTION NAME
    set_enclosures

FUNCTION DESCRIPTION
    Gets the message enclosures from the Java message object and adds them to the C message
    object.

PARAMETERS
    Type	Name		I/O	Description
    MsgObject	the_msg		O	Pointer to the message object.
    JNIEnv*	env		I	Pointer to the JVM environment.
    jobject	obj		I	The MhsMessage object containing the message.
    jclass	msg_class	I	The MhsMessage object's class.

LOCAL DATA FILES

RETURNS
	0 on success
	-1 on Failure.  Also throws a MhsSubmitMessage exception back to the JVM.

ERRORS REPORTED

---------------------------------------------------------------------------------------------- */

int set_enclosures(MsgObject the_msg, JNIEnv *env, jobject obj, jclass msg_class) {
    jfieldID        enclist_fid;
    jobject         enclist_obj;
    jclass          enclist_class;
    jmethodID       enclist_getCount_mid;
    jmethodID       enclist_get_mid;
    jobject         enc_obj;
    jclass          enc_class;
    jmethodID       enc_getName_mid	= NULL;
    jstring         jenc;
    char            *enc;
    int             enc_count;
    char            *prodid;
    int             i;
    char            first_time = TRUE;
    char            buf[200];

    sprintf(buf, "L%s/EnclosureList;", PACKAGE_CLASS);
    if ((enclist_fid = (*env)->GetFieldID(env, msg_class, "enclosures", buf)) == NULL) {
	strcpy(statbuf, "Could not get fieldID for enclosures");
        throw_submit_exception(env, FAIL_JNI, statbuf);
        return(0);
    }

    if ((enclist_obj = (*env)->GetObjectField(env, obj, enclist_fid)) == NULL) {
	strcpy(statbuf, "Could not get objectID for enclosures");
        throw_submit_exception(env, FAIL_JNI, statbuf);
        return(0);
    }

    enclist_class = (*env)->GetObjectClass(env, enclist_obj);
    enclist_getCount_mid = (*env)->GetMethodID(env, enclist_class, "getCount", "()I");
    enc_count = (*env)->CallIntMethod(env, enclist_obj, enclist_getCount_mid);
    if (show_trace) printf ("Enclosure count: %d\n", enc_count);

    /* Get the Enclosures --------------------------------------------------------------------------- */
    sprintf(buf, "(I)L%s/Enclosure;", PACKAGE_CLASS);
    enclist_get_mid = (*env)->GetMethodID(env, enclist_class, "get", buf);
    prodid = the_msg->prodid;
    for (i = 0; i < enc_count; i++) {
        enc_obj = (*env)->CallObjectMethod(env, enclist_obj, enclist_get_mid, i);
        if (first_time) {
            enc_class = (*env)->GetObjectClass(env, enc_obj);
            enc_getName_mid = (*env)->GetMethodID(env, enc_class, "getEnclosureName", "()Ljava/lang/String;");
            first_time = FALSE;
        }
        jenc = (*env)->CallObjectMethod(env, enc_obj, enc_getName_mid);
        enc = (char *) (*env)->GetStringUTFChars(env, jenc, NULL);
        if (show_trace) printf ("Enclosure: %s\n", enc);

        if (coDDM_addEnclosureProd(the_msg, enc, prodid) < 0) {
            sprintf (statbuf, "Failed addEnclosure '%s'\n", enc);
            coDDM_destroyMsg(&the_msg);
            throw_submit_exception(env, FAIL_ENCLOSE, statbuf);
            return(0);
        }
        prodid = NULL; /* only put the prodid on the first enclosure */
        (*env)->ReleaseStringUTFChars(env, jenc, enc);
    }

    return(1);
}

/* ----------------------------------------------------------------------------------------------

FUNCTION NAME
    get_boolean_field

FUNCTION DESCRIPTION
    Generic function to get the value of a specified boolean field from a Java object

PARAMETERS
    Type	Name		I/O	Description
    JNIEnv*	env		I	Pointer to the JVM environment.
    jobject	obj		I	The parent object containing the named field.
    jclass	msg_class	I	The parent class containing the named field.
    char*	field_name	I	Name of the Java field name as a string.
    int*	bool_ptr	O	Pointer to the variable that is to contain the boolean value.

LOCAL DATA FILES

RETURNS
	0 on success
	-1 on Failure.

ERRORS REPORTED

---------------------------------------------------------------------------------------------- */

int get_boolean_field(JNIEnv *env, jobject obj, jclass msg_class, char *field_name, int *bool_ptr) {
    jfieldID     fid;

    if ((fid = (*env)->GetFieldID(env, msg_class, field_name, "Z")) == NULL) {
        return(0);
    }

    *bool_ptr = (*env)->GetBooleanField(env, obj, fid);
    return(1);
}

/* ----------------------------------------------------------------------------------------------

FUNCTION NAME
    get_int_field

FUNCTION DESCRIPTION
    Generic function to get the value of a specified int field from a Java object

PARAMETERS
    Type	Name		I/O	Description
    JNIEnv*	env		I	Pointer to the JVM environment.
    jobject	obj		I	The parent object containing the named field.
    jclass	msg_class	I	The parent class containing the named field.
    char*	field_name	I	Name of the Java field name as a string.
    int*	int_ptr	O	Pointer to the variable that is to contain the integer value.

LOCAL DATA FILES

RETURNS
	0 on success
	-1 on Failure.

ERRORS REPORTED

---------------------------------------------------------------------------------------------- */

int get_int_field(JNIEnv *env, jobject obj, jclass msg_class, char *field_name, int *int_ptr) {
    jfieldID     fid;

    if ((fid = (*env)->GetFieldID(env, msg_class, field_name, "I")) == NULL) {
        return(0);
    }

    *int_ptr = (*env)->GetIntField(env, obj, fid);
    return(1);
}

/* ----------------------------------------------------------------------------------------------

FUNCTION NAME
    get_string_field

FUNCTION DESCRIPTION
    Generic function to get the value of a specified String field from a Java object.  The
    resultant C string is malloc'd so it must eventually be free'd by the caller.

PARAMETERS
    Type	Name		I/O	Description
    JNIEnv*	env		I	Pointer to the JVM environment.
    jobject	obj		I	The parent object containing the named field.
    jclass	msg_class	I	The parent class containing the named field.
    char*	field_name	I	Name of the Java field name as a string.
    char**	str_ptr		O	Pointer to the char* variable for the specified String.

LOCAL DATA FILES

RETURNS
	0 on success
	-1 on Failure.

ERRORS REPORTED

---------------------------------------------------------------------------------------------- */

int get_string_field(JNIEnv *env, jobject obj, jclass msg_class, char *field_name, char **str_ptr) {
    jfieldID      fid;
    jstring     jstr;
    const char  *str;

    if ((fid = (*env)->GetFieldID(env, msg_class, field_name, "Ljava/lang/String;")) == NULL) {
        return(0);
    }

    jstr = (*env)->GetObjectField(env, obj, fid);
    str = (*env)->GetStringUTFChars(env, jstr, NULL);
    if (str == NULL) {
        return(0);
    }

    if (!(*str_ptr = malloc(strlen(str)+1))) {
        (*env)->ReleaseStringUTFChars(env, jstr, str);
        return(0);
    }

    strcpy(*str_ptr, str);
    (*env)->ReleaseStringUTFChars(env, jstr, str);
    return(1);
}

/* ----------------------------------------------------------------------------------------------

FUNCTION NAME
    throw_submit_exception

FUNCTION DESCRIPTION
    Raises an MhsSubmitException in the JVM. The exception will not actually get thrown until
    the JNI code returns control to the JVM.

PARAMETERS
    Type	Name		I/O	Description
    JNIEnv*	env		I	Pointer to the JVM environment.
    char*	result_code	I	String containing MHS result code (Not currently used.)
    char*	result_text	I	String containing detailed submission result.

LOCAL DATA FILES

RETURNS
	0 on success
	-1 on Failure.

ERRORS REPORTED

---------------------------------------------------------------------------------------------- */

void throw_submit_exception(JNIEnv *env, char *result_code, char *result_text) {
    (*env)->ThrowNew(env, newExcCls, result_text);
}
