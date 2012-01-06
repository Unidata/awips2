/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.messaging.mhs;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;

/**
 * <code>MhsMessage</code> is the base class in the
 * <code>com.raytheon.messaging.mhs</code> package, which allows an application,
 * in the proper AWIPS environment, to generate and transmit an MHS message to
 * one or more destinations. MHS uses the NGIT proprietary DWB libraries written
 * in C. This package wraps those libraries using JNI code to interface between
 * Java and C.
 * <p>
 * The methods and elements in this class allow an application to create and
 * specify all message options and submit it for transmission to the message
 * request daemon (<code>msgreq_svr</code>), which listens on a well-known
 * socket. MHS uses SMTP for message routing and transport. When an MHS message
 * is received, the message receive daemon (<code>msgrcv_svr</code>) performs
 * actions based on message parameters, such as action codes or special
 * destination identifiers. Actions can include writing enclosures to configured
 * directories or pipes, or passing enclosures as parameters to programs or
 * scripts for processing.
 * <p>
 * This package can only be used from within the AWIPS WAN on Linux servers that
 * have been fully configured with NIS, DNS, and a complete AWIPS operating
 * environment.
 * <p>
 * Sample usage:
 * 
 * <pre>
 * MhsMessage        msg         = new MhsMessage(134);      // Set action code to 134
 * 
 * msg.setSubject(&quot;WSWMLB&quot;);                         // The AFOS PIL
 * msg.addAddressee(&quot;DEFAULTNCF&quot;);               // Special address
 * msg.setPriority(MhsMessagePriority.High);         // High priority
 * msg.setType(MhsMessageType.Routine);              // Routine message
 * msg.addEnclosure(&quot;/data/fxa/message/wswmlb.txt&quot;);
 * msg.setValidTime(60 * 60 * 4));               // 4 hours from now (optional)
 * try {
 *       System.out.println(msg.send());                                 // Submit for processing
 * } catch (MhsSubmitException e) {
 *       e.printStackTrace();
 *       // Do some error recovery stuff
 * }
 * </pre>
 * 
 * @author Brian M. Rapp
 * @version 1.0
 */

public class MhsMessage {

    /**
     * Environment variable to set default message trace value. If it is
     * undefined, it is the same as defining it to "NO" or "FALSE".
     */
    static public final String EnvMhsTrace = "MHS_SHOW_TRACE";

    /**
     * Environment variable to determine the MHS interface to use. If undefined,
     * it is the same as defining it to "NO" or "FALSE". If set to "YES" or
     * "TRUE", then the JNI interface to the MHS DWB functions is used to submit
     * messages. Any other value results in message submission occurring via the
     * command line utility msg_send.
     */
    static public final String EnvMhsUseNative = "MHS_USE_NATIVE_INTERFACE";

    /**
     * Minimum message code value
     */
    static public final int MsgCodeMin = 0;

    /**
     * Maximum valid message code value
     */
    static public final int MsgCodeMax = 256;

    /**
     * Maximum length of a message subject string
     */
    static public final int MaxSubjectLength = 129;

    /**
     * Maximum length of a product ID
     */
    static public final int MaxProductIdLength = 32;

    /**
     * Maximum length of a user ID
     */
    static public final int MaxUserIdLength = 10;

    /**
     * Maximum length of message time out
     */
    static public final int MaxTimeoutSeconds = 1200;

    /**
     * Maximum permissible number of retries
     */
    static public final int MaxRetryCount = 10;

    /**
     * Maximum permissible size of message body
     */
    static public final int MaxBodySize = (17 * 1024);

    /**
     * Integer message action code that defines how the recipient should process
     * the message. Its value must be between {@link #MsgCodeMin} and
     * {@link #MsgCodeMax}.
     */
    private int actionCode;

    /**
     * Free format ASCII message subject up to {@link #MaxSubjectLength}
     * characters long
     */
    private String subject;

    /**
     * List of addressees. Each message must have at least one addressee.
     * 
     * @see AddresseeList
     * @see Addressee
     */
    private AddresseeList addressees;

    /**
     * Message priority. Possible values are the MhsMessagePriority enum values
     * Default, Medium, and High.
     * 
     * @see MhsMessagePriority
     */
    private MhsMessagePriority priority;

    /**
     * Type of MHS message. Possible values are defined in the enum
     * {@link MhsMessageType}. The default value is Routine.
     */
    private MhsMessageType type;

    /**
     * List of file enclosures. Enclosures are optional.
     * 
     * @see EnclosureList
     * @see Enclosure
     */
    private EnclosureList enclosures;

    /**
     * File name containing the body text. The body file can be a maximum of
     * {@link #MaxBodySize} bytes.
     */
    private String bodyFile;

    /**
     * Optional product ID of up to {@link #MaxProductIdLength} characters.
     */
    private String productId;

    /**
     * Absolute time after which the message is no longer valid. This message
     * parameter is optional.
     */
    private Date validTime;

    /**
     * Valid time parameter formatted as a string (mm/dd/yyyy:HHMM). This value
     * is derived from {@link #validTime} during message submission.
     */
    @SuppressWarnings("unused")
    private String validTimeString;

    /**
     * Absolute time that the sender will wait for an acknowledgment before
     * reporting a delivery error. This parameter is optional.
     */
    private Date timeoutTime;

    /**
     * Timeout time parameter formatted as a string (mm/dd/yyyy:HHMM). This
     * value is derived from {@link #timeoutTime} during message submission.
     */
    @SuppressWarnings("unused")
    private String timeoutTimeString;

    /**
     * Mailbox user name. This is used for switching messaging gateways and
     * should normally not be specified. The user ID string has a maximum length
     * of {@link #MaxUserIdLength}.
     */
    private String userId;

    /**
     * Optional message maximum retry count. The default is 0 and maximum is
     * {@link #MaxRetryCount}.
     */
    private int retryCount;

    /**
     * Set true to force verification of addressees only. The message will not
     * be sent. This is not normally used. The default value is false.
     */
    public boolean verifyAddressees;

    /**
     * If set true, debugging information will be written to stdout. Defaults to
     * whatever the environment variable MHS_SHOW_TRACE is set to. If not
     * defined, it defaults to no.
     */
    public boolean showTrace;

    /**
     * Used to prevent a successfully submitted message from being submitted
     * multiple times. It is initially set to false by the constructor. The
     * <code>send</code> method will set it true following a successful
     * submission.
     */
    private boolean submitted;

    /**
     * The message ID string assigned by the message request server daemon. This
     * field is only valid following the successful submission of this message.
     */
    private String messageId;

    /**
     * Detailed result String assigned during message submission. If the message
     * was submitted successfully, <code>resultText</code> will contain the
     * <code>String 
     * "Success" </code>. This text can be used to provide additional
     * information about why a submission failure occurred.
     */
    private String resultText;

    /**
          * 
          */
    private boolean useNativeInterface;

    /**
     * Sole constructor. Provides reasonable default values for a message
     * object's fields as follows:
     * <ul>
     * <li>retry count = 0</li>
     * <li>priority = Default</li>
     * <li>message type = Routine</li>
     * <li>verify addressees flag = false</li>
     * <li>show trace = false</li>
     * </ul>
     * 
     * @param code
     *            Integer action code indicating how the recipient should
     *            process the message when received.
     * @see #setRetryCount(int)
     * @see #getRetryCount()
     * @see #setActionCode(int)
     * @see MhsMessagePriority
     * @see MhsMessageType
     */
    public MhsMessage(int code) {
        String traceString = System.getenv(EnvMhsTrace);
        String interfaceString = System.getenv(EnvMhsUseNative);

        showTrace = (traceString != null)
                && ((traceString.equalsIgnoreCase("yes") || traceString
                        .equalsIgnoreCase("true")));
        useNativeInterface = (interfaceString != null)
                && ((interfaceString.equalsIgnoreCase("yes")) || interfaceString
                        .equalsIgnoreCase("true"));

        actionCode = code;
        setRetryCount(0);
        addressees = new AddresseeList();
        enclosures = new EnclosureList();
        timeoutTime = null;
        validTime = null;
        bodyFile = "";
        productId = "";
        subject = "";
        userId = "";
        messageId = "";
        priority = MhsMessagePriority.Default;
        type = MhsMessageType.Routine;
        resultText = "Success";
        verifyAddressees = false;
        submitted = false;
    }

    /**
     * Submits a message to the message request server daemon. This is JNI code
     * to translate a Java message object to a C MHS message object.
     * 
     * @return 0 if successful; a positive integer if a MHS error is detected;
     *         -1 if an error occurs in a JNI call, which will usually result in
     *         a Java exception when control is returned toe the JVM.
     * @see MhsMessage
     */
    private native String submitMessage() throws MhsSubmitException;

    static {
        System.loadLibrary("coDDM_msg_send");
    }

    /**
     * Gets the action code for this message object.
     * 
     * @return An integer between 0 and {@link #MsgCodeMax} inclusive.
     * @see #actionCode
     * @see #setActionCode(int)
     */
    public int getActionCode() {
        return actionCode;
    }

    /**
     * Registers the action code for this message object. The message submission
     * will fail if the code is not within the allowable range.
     * 
     * @param code
     *            an integer between 0 and {@link #MsgCodeMax} inclusive.
     * @return true if the code is valid; false if invalid.
     * @see #actionCode
     * @see #getActionCode()
     */
    public boolean setActionCode(int code) {
        boolean stat = true;
        actionCode = code;
        if (!isCodeValid()) {
            stat = false;
        }
        return stat;
    }

    /**
     * Registers an optional subject for this message object. An error will be
     * thrown during message submission if the subject is set to null. The
     * default is a zero-length string.
     * 
     * @param msgSubject
     *            free format ASCII string up to 129 characters in length.
     * @see #subject
     * @see #getSubject()
     */
    public void setSubject(String msgSubject) {
        subject = msgSubject;
    }

    /**
     * Gets the message subject string.
     * 
     * @return The message subject.
     * @see #subject
     * @see #setSubject(String)
     */
    public String getSubject() {
        return subject;
    }

    /**
     * Adds a destination addressee to the message object address list. This can
     * be a WFO/RFC site ID, NCF facility, or special address such as
     * DEFAULT_NCF, NDFD, SBN, or NWSTG. Each message must have at least one
     * addressee, though any number can be specified.
     * 
     * @param addressee
     *            a single addressee string
     * @return true if the addressee was successfully added; false if a null was
     *         passed for the addressee string or the addressee could not be
     *         added to the list.
     * @see AddresseeList
     * @see Addressee
     * @see #removeAddressee(String)
     */
    public boolean addAddressee(String addressee) {
        return addressees.add(addressee, false);
    }

    /**
     * Removes a specified addressee from the message address list.
     * 
     * @param addressee
     *            a single address string
     * @return true if the address was removed from the list; false if not.
     * @see AddresseeList
     * @see Addressee
     * @see #addAddressee(String)
     */
    public boolean removeAddressee(String addressee) {
        return addressees.remove(addressee, false);
    }

    /**
     * Removes all addressees from the message object address list.
     * 
     * @return integer number of addressees removed from the list.
     * @see AddresseeList
     * @see Addressee
     * @see #addAddressee(String)
     * @see #addAckAddressee(String)
     * @see #removeAddressee(String)
     * @see #removeAckAddressee(String)
     */
    public int removeAllAddressees() {
        return addressees.removeAll();
    }

    /**
     * Adds a destination addressee to the message object address list with a
     * request for an acknowledgment. This can be a WFO/RFC site ID, NCF
     * facility, or special address such as DEFAULT_NCF, NDFD, SBN, or NWSTG.
     * Each message must have at least one addressee, though any number can be
     * specified. When the specified addressee receives this message, the
     * message receive daemon will automatically respond with an acknowledgment
     * message. If the sender does not receive an acknowledgment within the
     * timeout period, a NACK message will automatically be generated by
     * 
     * @param addressee
     *            a single addressee string
     * @return true if the addressee was successfully added; false if a null was
     *         passed for the addressee string or the addressee could not be
     *         added to the list.
     * @see AddresseeList
     * @see Addressee
     * @see #removeAckAddressee(String)
     */
    public boolean addAckAddressee(String addressee) {
        return addressees.add(addressee, true);
    }

    /**
     * Removes a specified addressee that has an acknowledgment request from the
     * message address list.
     * 
     * @param addressee
     *            a single address string
     * @return true if the address was removed from the list; false if not.
     * @see AddresseeList
     * @see Addressee
     * @see #addAckAddressee(String)
     */
    public boolean removeAckAddressee(String addressee) {
        return addressees.remove(addressee, true);
    }

    /**
     * Gets number of addressees for this message.
     * 
     * @return Integer number of message addressees.
     */
    public int getAddresseeCount() {
        return addressees.getCount();
    }

    /**
     * Sets the message priority to one of three values: Default (low), Medium,
     * or High, which are defined as the enum MhsMessagePriority. The priority
     * defaults to MhsMessagePriority.Default unless overridden by this method.
     * 
     * @param msgPriority
     *            the desired message priority of type MhsMessagePriority.
     * @see MhsMessagePriority
     * @see #getPriority()
     */
    public void setPriority(MhsMessagePriority msgPriority) {
        priority = msgPriority;
    }

    /**
     * Gets the currently assigned priority of this message object.
     * 
     * @return one of Default, Medium, or High from the enum MhsMessagePriority.
     * @see #setPriority(MhsMessagePriority)
     */
    public MhsMessagePriority getPriority() {
        return priority;
    }

    /**
     * Set the message object type to one of the values in enum MhsMessageType.
     * The default value is 0 (Routine) unless overridden by a call to this
     * method.
     * 
     * @param msgType
     *            the message type as one of the values from enum
     *            MhsMessageType.
     * @see MhsMessageType
     * @see #getType()
     */
    public void setType(MhsMessageType msgType) {
        type = msgType;
    }

    /**
     * Gets the message objects message type as a value from enum
     * MhsMessageType.
     * 
     * @return the message object's current message type as a value from enum
     *         MhsMessageType.
     * @see MhsMessageType
     * @see #setType(MhsMessageType)
     */
    public MhsMessageType getType() {
        return type;
    }

    /**
     * Retrieves the specific message status text message. Until submission, the
     * status message (error string) contains "Success". After submission, the
     * error string will contain the string returned by the submit call.
     * 
     * @return string containing the message submission status string. If the
     *         submission was successful, it will contain the value "Success",
     *         otherwise it will contain a more detailed description of the
     *         error. This is used in conjunction with the error code.
     * @see #getMessageId()
     */
    public String getResultText() {
        return resultText;
    }

    /**
     * Adds an enclosure file to the message object enclosure list. Multiple
     * enclosures can be added via multiple calls to this method. There is no
     * predefined limit to the number of enclosures, the size of individual
     * enclosures, or the maximum message size, though the practical limit is
     * about 10 MB due to the limited bandwidth available over the AWIPS WAN.
     * The specified enclosure does not have to exist at the time it is added
     * via this method, but it must exist when the message is submitted.
     * Enclosures are optional, but may be required for specific destinations
     * and message action codes. Only a single instance of a given unique file
     * may be inserted into an enclosure list.
     * 
     * @param fileName
     *            full absolute path and file name of the enclosure.
     * @return true if the enclosure file name was successfully added to the
     *         enclosure list; false if it could not be added.
     * @see EnclosureList
     * @see Enclosure
     * @see #removeEnclosure(String)
     * @see #removeAllEnclosures()
     */
    public boolean addEnclosure(String fileName) {
        return enclosures.add(fileName);
    }

    /**
     * Removes a specific enclosure file name from the message object's
     * enclosure list.
     * 
     * @param fileName
     *            the full path and file name of the file to be removed from the
     *            enclosure list.
     * @return true if the specified file name was removed from the enclosure
     *         list; false if not.
     * @see EnclosureList
     * @see Enclosure
     * @see #addEnclosure(String)
     * @see #removeAllEnclosures()
     */
    public boolean removeEnclosure(String fileName) {
        return enclosures.remove(fileName);
    }

    /**
     * Removes all enclosure file names from the message object's enclosure
     * list.
     * 
     * @return integer number of file names removed from the enclosure list.
     * @see EnclosureList
     * @see Enclosure
     * @see #addEnclosure(String)
     * @see #removeEnclosure(String)
     */
    public int removeAllEnclosures() {
        return enclosures.removeAll();
    }

    /**
     * Gets the number of file enclosures for this message.
     * 
     * @return Integer number of file enclosures.
     */
    public int getEnclosureCount() {
        return enclosures.getCount();
    }

    /**
     * Gets an enclosure file name by index value.
     * 
     * @param i
     *            <code>int</code> index of enclosure.
     * @return <code>String</code> enclosure file name
     * @throws IndexOutOfBoundsException
     */
    public String getEnclosureName(int i) {
        return enclosures.get(i).getEnclosureName();
    }

    /**
     * Gets the size of an enclosure file by index value.
     * 
     * @param i
     *            <code>int</code> index of enclosure.
     * @return <code>long</code> size of enclosure in bytes.
     * @throws IndexOutOfBoundsException
     */
    public long getEnclosureSize(int i) {
        return enclosures.get(i).getEnclosureSize();
    }

    /**
     * Gets the total size of all enclosures to this message.
     * 
     * @return <code>long</code> number of bytes of all files enclosed in this
     *         message.
     */
    public long getTotalEnclosureSize() {
        long tsize = 0;
        for (int i = 0; i < enclosures.getCount(); i++) {
            tsize += enclosures.get(i).getEnclosureSize();
        }
        return tsize;
    }

    /**
     * Sets the name of the file that will be included as the message's body
     * upon submission. Only a single body file can be attached and it can be no
     * larger than 17 KB. The body file does not need to exist until the message
     * is submitted. If it doesn't exist at submission time, the submission will
     * fail. The message can be re-submitted once the body file exists. The body
     * file will be buffered and attached to the message object by the C JNI
     * submission function. The body file is optional and none is included by
     * default. To remove the body file after attaching one, simply call this
     * method again with an empty string ("");
     * 
     * @param fileName
     *            full absolute path and name of the body file.
     * @see #getBodyFile()
     */
    public void setBodyFile(String fileName) {
        bodyFile = fileName;
    }

    /**
     * Gets the fully qualified body file name for this message object.
     * 
     * @return string containing the body file name assigned to this message
     *         object. If one has not been assigned, an empty string ("") will
     *         be returned.
     * @see #setBodyFile(String)
     */
    public String getBodyFile() {
        return bodyFile;
    }

    /**
     * Sets the optional product ID for this message object. The default is an
     * empty string (""). If multiple enclosures are attached, the product ID
     * will only be assigned to the first one during message submission. The
     * product ID has a maximum length of 32 ASCII characters. A product ID is
     * not required for most messages, though there are some action codes that
     * may require one to be assigned.
     * 
     * @param product
     *            optional product ID string for this message object.
     * @see #getProductId()
     */
    public void setProductId(String product) {
        productId = product;
    }

    /**
     * Retrieves the product ID assigned to this message object. The default is
     * the empty string (""). The product ID has a maximum length of 32 ASCII
     * characters. A product ID is not required for most messages, though there
     * are some action codes that may require one to be assigned.
     * 
     * @return string containing the product ID assigned to this message object.
     * @see #setProductId(String)
     */
    public String getProductId() {
        return productId;
    }

    /**
     * Sets the expiration time for this message to the specified number of
     * seconds in the future.
     * 
     * @param validSeconds
     *            the number of seconds that this message will be valid relative
     *            to the current time. This must be a positive value.
     */
    public void setValidTime(long validSeconds) {
        this.validTime = new Date(System.currentTimeMillis() + validSeconds
                * 1000);
    }

    /**
     * Sets the expiration time to an absolute date/time rounded to the nearest
     * minute (seconds will be truncated).
     * 
     * @param validTime
     *            Date value representing an absolute time in the future. A null
     *            value has the effect of removing a previously specified
     *            expiration time.
     */
    public void setValidTime(Date validTime) {
        this.validTime = validTime;
    }

    /**
     * Gets the expiration time for this message.
     * 
     * @return Date value containing the message absolute expiration time. A
     *         null value indicates that an expiration time is not indicated.
     */
    public Date getValidTime() {
        return validTime;
    }

    /**
     * Sets an acknowledgment time out value to the specified number of seconds
     * in the future.
     * 
     * @param timeoutSeconds
     *            the number of seconds MHS will wait for an acknowledgment from
     *            addressees. This is only valid for messages that request an
     *            acknowledgment. This must be between 0 and
     *            {@link #MaxTimeoutSeconds} exclusive.
     * @see #addAckAddressee(String)
     */
    public void setTimeoutTime(long timeoutSeconds) {
        this.timeoutTime = new Date(System.currentTimeMillis() + timeoutSeconds
                * 1000);
    }

    /**
     * Sets an acknowledgment time out value to an absolute date/time. The
     * specified value will be rounded to the nearest minute (seconds
     * truncated).
     * 
     * @param timeoutTime
     *            Date value representing an absolute time in the future. A null
     *            value has the effect of removing a previously specified
     *            timeout.
     * @see #addAckAddressee(String)
     */
    public void setTimeoutTime(Date timeoutTime) {
        this.timeoutTime = timeoutTime;
    }

    /**
     * Gets the acknowledgment timeout time for this message
     * 
     * @return Date value containing the acknowledgment timeout value as an
     *         absolute date/time. Null indicates that an acknowledgment timeout
     *         has not been set. The specified date must be less than
     *         {@link #MaxTimeoutSeconds} in the future.
     */
    public Date getTimeoutTime() {
        return timeoutTime;
    }

    /**
     * Registers a mailbox user Id for addressees.
     * 
     * @param userId
     *            string containing non-default mailbox user Id. This is used
     *            only when changing the gateway and it applies to all message
     *            addressees. The user Id should not be changed unless is a
     *            really good reason for doing so. Calling this method with an
     *            empty string ("") will remove a previously set user Id.
     * @see #userId
     */
    public void setUserId(String userId) {
        this.userId = userId;
    }

    /**
     * Gets a previously set user Id for this message.
     * 
     * @return string containing a non-default user Id set previously by a call
     *         of {@link #setUserId(String)}. An empty string indicates that a
     *         non-default value has not be specified.
     * @see #setUserId(String)
     * @see #userId
     */
    public String getUserId() {
        return userId;
    }

    /**
     * Sets a retry value for this message.
     * 
     * @param retryCount
     *            An integer retry value between 0 and {@link #MaxRetryCount}.
     *            The default value is 0.
     * @see #getRetryCount()
     */
    public void setRetryCount(int retryCount) {
        this.retryCount = retryCount;
    }

    /**
     * Gets the retry value for this message, which is 0 unless overridden by a
     * call to {@link #setRetryCount(int)}.
     * 
     * @return Integer message retry value. The default is 0.
     * @see #setRetryCount(int)
     */
    public int getRetryCount() {
        return retryCount;
    }

    /**
     * Call to force the MHS interface to be used for this message. The default
     * value is determined by the environment variable MHS_USE_NATIVE_INTERFACE.
     * Set to true to use the native C DWB functions. Set to false to use the
     * command line utility msg_send.
     * 
     * @param useNativeInterface
     *            the useNativeInterface to set
     */
    public void setUseNativeInterface(boolean useNativeInterface) {
        this.useNativeInterface = useNativeInterface;
    }

    /**
     * Returns the MHS interface that will be used to submit this message. A
     * true value indicates that the native C DWB functions will be used. False
     * indicates that the command line utility msg_send will be used.
     * 
     * @return the useNativeInterface
     */
    public boolean isUseNativeInterface() {
        return useNativeInterface;
    }

    /**
     * Retrieves the message Id of this message.
     * 
     * @return A string containing the message Id assigned by the message
     *         request daemon. The returned value is only valid following
     *         successful message submission.
     * @see #send()
     */
    public String getMessageId() {
        return messageId;
    }

    private boolean isAddresseeListValid() {
        boolean status = true;

        if (addressees.getCount() == 0) {
            resultText = "No addressees specified";
            status = false;
        }
        return status;
    }

    private boolean isCodeValid() {
        boolean status = true;

        if ((actionCode < MsgCodeMin) || (actionCode > MsgCodeMax)) {
            resultText = "Invalid message action code: " + actionCode;
            status = false;
        }
        return status;
    }

    private boolean isSubjectValid() {
        boolean status = true;

        if (subject == null) {
            resultText = "Message subject set to null";
            status = false;
        } else if (subject.length() > MaxSubjectLength) {
            resultText = "Message subject length (" + subject.length()
                    + ") cannot exceed " + MaxSubjectLength + " characters";
            status = false;
        }
        return status;
    }

    private boolean isTypeValid() {
        boolean status = true;

        if (type == null) {
            status = false;
            resultText = "Message Type cannot be null";
        }
        return status;
    }

    private boolean checkBodyFile() {
        boolean status = true;

        if (bodyFile == null) {
            status = false;
            resultText = "Body file set to null";
        } else if (bodyFile.length() > 0) {
            File bFile = new File(bodyFile);
            if (!bFile.exists()) {
                status = false;
                resultText = "Body file " + bodyFile + " does not exist";
            } else if (bFile.length() > MaxBodySize) {
                status = false;
                resultText = "Size of body file " + bodyFile + " ("
                        + bFile.length() + ")" + " exceeds maximum of "
                        + MaxBodySize;
            }
        }
        return status;
    }

    private boolean checkEnclosures() {
        boolean status = true;
        String fName;
        File enc;

        for (int i = 0; i < enclosures.getCount(); i++) {
            fName = enclosures.get(i).getEnclosureName();
            if ((fName == null) || (fName.length() == 0)) {
                status = false;
                resultText = "null enclosure file name found";
                break;
            }

            enc = new File(fName);
            if (!enc.exists()) {
                status = false;
                resultText = "Enclosure " + fName + " does not exist";
                break;
            }
        }

        return status;
    }

    private boolean isPriorityValid() {
        boolean status = true;

        if (priority == null) {
            resultText = "Priority cannot be null";
            status = false;
        }
        return status;
    }

    private boolean isProductIdValid() {
        boolean status = true;

        if (productId == null) {
            resultText = "Product ID set to null";
            status = false;
        } else if (productId.length() > MaxProductIdLength) {
            resultText = "Product ID cannot be longer than "
                    + MaxProductIdLength + " characters";
            status = false;
        }
        return status;
    }

    private boolean isUserIdValid() {
        boolean status = true;

        if (userId == null) {
            resultText = "User ID set to null";
            status = false;
        } else if (userId.length() > MaxUserIdLength) {
            resultText = "User ID cannot be longer than " + MaxUserIdLength
                    + " characters";
            status = false;
        }
        return status;
    }

    private boolean isValidTimeValid() {
        boolean status = true;

        if ((validTime != null) && (validTime.before(new Date()))) {
            status = false;
            resultText = "Valid time must be in the future";
        }
        return status;
    }

    private boolean isTimeoutTimeValid() {
        boolean status = true;

        if (timeoutTime != null) {
            if (timeoutTime.before(new Date())) {
                status = false;
                resultText = "Time out time must be in the future";
            } else if ((timeoutTime.getTime() - (new Date().getTime())) > (MaxTimeoutSeconds * 1000)) {
                status = false;
                resultText = "Time out time must be within "
                        + MaxTimeoutSeconds + " seconds of now";
            }
        }
        return status;
    }

    private static boolean checkVars(String envdir, String envfrag,
            boolean fatal) {
        String filename;
        File dwbFile;
        final String TEMPLATE = "/env.";
        boolean status = true;

        filename = envdir + TEMPLATE + envfrag;

        dwbFile = new File(filename);
        if (!dwbFile.exists() && fatal) {
            status = false;
        }

        return status;
    }

    /**
     * Checks for required DWB components that will result in DWB executing a
     * system exit call causing a shutdown of EDEX as it is currently
     * implemented.
     * 
     * At least one of the environment variables PROJECT or PROJECTWORK must be
     * defined, and the file env.dwb must exist in the DWB path.
     * 
     * @return true if the above conditions are met, false if not.
     */
    private boolean isDwbEnvSet() {
        boolean status = true;
        final String projectDef = "PROJECT";
        final String projectWorkDef = "PROJECTWORK";
        final String dwbEnvDir = "DWB_ENVDIR";
        final String dwbDataDir = "DWB_DATA";
        String dwbEnv;
        String project;
        String data;

        if (((project = System.getenv(projectDef)) == null)
                && (project = System.getenv(projectWorkDef)) == null) {
            status = false;
            resultText = "PROJECT or PROJECTWORK environment variable is not defined";
        } else {
            dwbEnv = System.getenv(dwbEnvDir);
            if (dwbEnv == null) {
                if ((data = System.getenv(dwbDataDir)) != null) {
                    dwbEnv = data;
                } else {
                    dwbEnv = project + "/data";
                }
            }

            if (!(status = checkVars(dwbEnv, "dwb", true))) {
                // This file must be present
                status = false;
                resultText = "Required primary DWB environment file (env.dwb) not found in expected path";
            }
        }

        return status;
    }

    private boolean isRetryCountValid() {
        boolean status = true;

        if ((retryCount < 0) || (retryCount > MaxRetryCount)) {
            status = false;
            resultText = "Maximum retry count must be between 0 and "
                    + MaxRetryCount;
        }
        return status;
    }

    /**
     * Submits the message object for transmission. The message must be
     * completely qualified before submission. A given message object can only
     * be submitted once, though it can be resubmitted if errors were detected
     * during a previously submission. The only required parameters for
     * submission are a valid message code and at least one addressee, though
     * specific messages have other required parameters depending on
     * application. No attempt is made to validate messages for suitability for
     * a given purpose, only that the provided parameters are valid and
     * complete.
     * <p>
     * Debug information can be written to stdout by setting {@link #showTrace}
     * to true.
     * 
     * @return message ID if submission was successful; an exception is thrown
     *         if an error or invalid parameter was detected. A successful
     *         submission does not indicate that the message reached any or all
     *         of its intended recipients, just that the message was
     *         successfully submitted to a local MHS instance.
     */
    public synchronized String send() throws MhsSubmitException {
        if (submitted) {
            throw new MhsSubmitException(
                    "Attempt to resubmit message after previous successful submission");
        }

        resultText = "Success";

        if (!isDwbEnvSet() || !isAddresseeListValid() || !isCodeValid()
                || !isSubjectValid() || !isTypeValid() || !checkEnclosures()
                || !checkBodyFile() || !isProductIdValid() || !isUserIdValid()
                || !isValidTimeValid() || !isTimeoutTimeValid()
                || !isPriorityValid() || !isRetryCountValid()) {
            throw new MhsSubmitException(resultText);
        }

        SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy:HHmm");
        if (validTime != null) {
            validTimeString = dateFormat.format(validTime);
        } else {
            validTimeString = "";
        }
        if (timeoutTime != null) {
            timeoutTimeString = dateFormat.format(timeoutTime);
        } else {
            timeoutTimeString = "";
        }

        SimpleDateFormat timeStampFormat = new SimpleDateFormat(
                "yyyy-MM-dd HH:mm:ss.SSS");
        String timeStamp = timeStampFormat.format(System.currentTimeMillis());
        System.out.println(timeStamp + ": Message submitted");

        if (useNativeInterface) { // Submit the message using the MHS and DWB
                                  // native libraries.
            try {
                messageId = submitMessage();
            } catch (MhsSubmitException e) {
                resultText = e.toString();
                throw new MhsSubmitException(resultText);
            }
        } else { // Submit the message by constructing a command line and
                 // execing a new msg_send process
            LinkedList<String> command = new LinkedList<String>();

            command.add("msg_send");
            command.add("-c" + actionCode);
            String addrStr = "";
            String ackAddrStr = "";
            String encList = "";

            for (int i = 0; i < addressees.getCount(); i++) {
                Addressee addr = addressees.get(i);
                if (addr.isAckRequired()) {
                    if (ackAddrStr.isEmpty()) {
                        ackAddrStr = addr.getAddress();
                    } else {
                        ackAddrStr += "," + addr.getAddress();
                    }
                } else {
                    if (addrStr.isEmpty()) {
                        addrStr = addr.getAddress();
                    } else {
                        addrStr += "," + addr.getAddress();
                    }
                }
            }

            if (!addrStr.isEmpty()) {
                command.add("-a" + addrStr);
            }

            if (!ackAddrStr.isEmpty()) {
                command.add("-A" + ackAddrStr);
            }

            if (retryCount != 0) {
                command.add("-R" + retryCount);
            }

            for (int i = 0; i < enclosures.getCount(); i++) {
                Enclosure enc = enclosures.get(i);
                if (encList.isEmpty()) {
                    encList = enc.getEnclosureName();
                } else {
                    encList += "," + enc.getEnclosureName();
                }
            }

            if (!encList.isEmpty()) {
                command.add("-e" + encList);
            }

            if (!timeoutTimeString.isEmpty()) {
                command.add("-T" + timeoutTimeString);
            }

            if (!validTimeString.isEmpty()) {
                command.add("-v" + validTimeString);
            }

            if (!bodyFile.isEmpty()) {
                command.add("-b" + bodyFile);
            }

            if (!productId.isEmpty()) {
                command.add("-i" + productId);
            }

            if (!subject.isEmpty()) {
                command.add("-s" + subject);
            }

            if (priority != MhsMessagePriority.Default) {
                command.add("-p" + priority.value());
            }

            if (type != MhsMessageType.Routine) {
                command.add("-t" + type.text());
            }

            if (verifyAddressees) {
                command.add("-C");
            }

            if (showTrace) {
                System.out.print("Executing command: ");
                for (int i = 0; i < command.size(); i++) {
                    System.out.print(command.get(i) + " ");
                }
                System.out.println();
            }

            Process proc = null;
            try {
                ProcessBuilder procDesc = new ProcessBuilder(command);
                procDesc.redirectErrorStream(true);

                proc = procDesc.start();
                InputStream stdout = proc.getInputStream();
                InputStreamReader isr = new InputStreamReader(stdout);
                BufferedReader br = new BufferedReader(isr);
                String outp;

                while ((outp = br.readLine()) != null) {
                    if (outp.length() > 0) {
                        // System.out.println(outp);
                        messageId = outp;
                    }
                }
                // System.out.println(line);
                int exitVal = proc.waitFor();
                if (exitVal != 0) {
                    // System.out.println("Abnormal exit code " + exitVal);
                    // resultText = messageId;
                    throw new MhsSubmitException(messageId);
                }

                if (showTrace) {
                    System.out
                            .println("Message successfully submitted.  Message ID: "
                                    + messageId);
                }
            } catch (Throwable t) {
                resultText = t.getMessage();
                // System.out.println("Exception thrown: " + resultText);
                throw new MhsSubmitException(resultText);
            } finally {
                // DR #10955
                if (proc != null) {
                    proc.destroy();
                }
            }
        }
        return messageId;
    }
}
