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

/**
 * 
 */
package com.raytheon.uf.common.message.response;

import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Represents an error response from EDEX. Fields are provided to give an error
 * sourcs, error message, error exception, and an array of error exception
 * causes. The exception values will contain the results of
 * <code>Exception.toString()</code> to the exception and its cause.
 * <P>
 * A static convenience method {@link #generateErrorResponse(String, Exception)
 * generateErrorResponse } is included to facilitate message creation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * 17Jul2006                        MW Fegan            Initial Creation
 * 10Aug2006        Task 19         MW Fegan            Added generateErrorResponse().
 *                                                       Updated JavaDoc.
 * 
 * </PRE>
 * 
 * @author mfegan
 * 
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ResponseMessageError extends AbstractResponseMessage {

    @XmlAttribute
    @DynamicSerializeElement
    protected String errorSource;

    @XmlAttribute
    @DynamicSerializeElement
    protected String errorMsg;

    @XmlAttribute
    @DynamicSerializeElement
    protected String errorCause;

    @XmlAttribute
    @DynamicSerializeElement
    protected String[] errorChain;

    /**
     * Constructor. No argument constructor.
     */
    public ResponseMessageError() {
        super();
    }

    /**
     * Constructor. Provides construction of an Error Response Message
     * containing the specified {@code error } and {@code exception }.
     * 
     * @param message
     *            plain text error message
     * @param exception
     *            the exception that triggered the message creation
     */
    protected ResponseMessageError(String message, Exception exception) {
        super();
        this.errorMsg = message;
        init(exception);
    }

    /**
     * Constructor. Provides construction of an Error Response Message
     * containing the specified {@code error }.
     * 
     * @param message
     *            plain text error message
     */
    protected ResponseMessageError(String message) {
        super();
        this.errorMsg = message;
        init(null);
    }

    /**
     * Convience method to allow creation of an Error Response Message. Clients
     * may use this method to avoid having to generate error details. The
     * {@code cause } argument should be {@code null } when no exception is to
     * be passed along.
     * 
     * @param error
     *            plain text error message
     * @param cause
     *            the exception that caused the error
     * 
     * @return the Error Response Message
     */
    public static ResponseMessageError generateErrorResponse(String error,
            Exception cause) {
        if (cause == null) {
            return new ResponseMessageError(error);
        } else {
            return new ResponseMessageError(error, cause);
        }
    }

    /**
     * Performs detailed initialization needed to create the Error Response
     * Message. In particular, it mines any nested exceptions to complete a
     * chain of underlying causes.
     * 
     * @param exception
     *            the exception that caused the message to be created.
     */
    private void init(Exception exception) {
        /*
         * fillin safe values for unused fields
         */
        this.dataURI = "";
        this.validTime = new Date();

        int count = 0;
        String error = getErrorMsg();
        String[] causes = null;
        Throwable cause;
        setFileType("html");
        setErrorSource("AWIPS EDEX Micro Engine");
        if (exception != null) {
            /*
             * process the exception to find the causes
             */
            setErrorCause(exception.toString());
            cause = exception.getCause();
            while (cause != null) {
                count++;
                cause = cause.getCause();
            }
            if (count > 0) {
                causes = new String[count];
                count = 0;
                cause = exception.getCause();
                while (cause != null) {
                    causes[count] = cause.toString();
                    if (cause.getMessage() != null) {
                        error = cause.getMessage();
                    }
                    cause = cause.getCause();
                    count++;
                }
                setErrorMsg(error);
            }
        } else {
            /*
             * provide dummy values for the message
             */
            setErrorCause("");
            causes = new String[1];
            causes[0] = "";
        }
        /*
         * set the causes list
         */
        setErrorChain(causes);
    }

    /**
     * @return the errorCause
     */
    public String getErrorCause() {
        return errorCause;
    }

    /**
     * @param errorCause
     *            the errorCause to set
     */
    public void setErrorCause(String errorCause) {
        this.errorCause = errorCause;
    }

    /**
     * @return the errorChain
     */
    public String[] getErrorChain() {
        return errorChain;
    }

    /**
     * @param errorChain
     *            the errorChain to set
     */
    public void setErrorChain(String[] errorChain) {
        this.errorChain = errorChain;
    }

    /**
     * @return the errorMsg
     */
    public String getErrorMsg() {
        return errorMsg;
    }

    /**
     * @param errorMsg
     *            the errorMsg to set
     */
    public void setErrorMsg(String errorMsg) {
        this.errorMsg = errorMsg;
    }

    /**
     * @return the errorSource
     */
    public String getErrorSource() {
        return errorSource;
    }

    /**
     * @param errorSource
     *            the errorSource to set
     */
    public void setErrorSource(String errorSource) {
        this.errorSource = errorSource;
    }

    @Override
    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append("Source: ");
        sb.append(this.errorSource);
        sb.append("\nMessage: ");
        sb.append(this.errorMsg);
        sb.append("\nCause: ");
        sb.append(this.errorCause);
        sb.append("\nStacktrace:\n");
        for (String st : this.errorChain) {
            sb.append(st);
            sb.append("\n");
        }

        return sb.toString();
    }

}
