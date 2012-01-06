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
package com.raytheon.edex.msg;

import java.util.Date;

import com.raytheon.uf.common.message.response.ResponseMessageError;

/**
 * Represents a script validation response from EDEX. Fields are provided to
 * specify validation result (success or failure) and for a result message and
 * the original script. 
 * <P>
 * A static convenience method 
 * {@link #generateValidateResponse(boolean, String, String, Exception) generateValidateResponse }
 * is included to facilitate message creation.
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * 19Jul2006                        MW Fegan            Initial Creation
 * 10Aug2006        Task 19         MW Fegan            Updated JavaDoc.
 * 
 * </PRE>
 * @author mfegan
 *
 */
public class ResponseMessageValidate extends ResponseMessageError {
    private Boolean result;
    private String message;
    private String script;
    /**
     * Constructor. Basic no argument constructor.
     */
    public ResponseMessageValidate() {
        super();
    }
    /**
     * Constructor. Creates a Validate Response Message for the specified
     * {@code result}, {@code message } and {@code script}.
     * 
     * @param result true if the validation was successful
     * @param message plain text validation result message
     * @param script the script being validated
     */
    protected ResponseMessageValidate(boolean result, String message, String script) {
        super();
        this.result = result;
        this.message = message;
        this.script = script;
        init(null);
    }
    /**
     * Constructor. Creates a Validate Response Message for the specified
     * {@code result}, {@code message}, {@code script} and {@code exception}.
     * 
     * @param result true if the validation was successful
     * @param message plain text validation result message
     * @param script the script being validated
     * @param exception the exception causing the validation to fail
     */
    protected ResponseMessageValidate(boolean result, String message, String script,Exception exception) {
        super();
        this.result = result;
        this.message = message;
        this.script = script;
        init(exception);
    }
    /**
     * Performs detailed initialization needed to create the Validate Response
     * Message. In particular, it mines any nested exceptions to complete a
     * chain of underlying causes.
     * 
     * @param exception the exception that cause validation to fail
     */
    private void init(Exception exception) {
        /*
         * fillin safe values for unused fields
         */
        this.dataURI = "";
        this.validTime = new Date();

        setFileType("html");
        setErrorSource("AWIPS EDEX Micro Engine Validation");
        setErrorMsg(this.message);
        String[] causes = null;
        if (exception != null) {
            Throwable cause;
            setErrorCause(exception.toString());
            cause = exception.getCause();
            int count = 0;        
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
                    cause = cause.getCause();
                    count++;
                }
            } else {
                causes = new String[1];
                causes[0] = "";
            }
            setErrorChain(causes);
        } else {
            // provide dummy values
            setErrorCause("");
            causes = new String[1];
            causes[0] = "";
            setErrorChain(causes);
        }
    }
    /**
     * Convience method to allow creation of an Validate Response Message.
     * Clients may use this method to avoid having to generate error details.
     * The {@code cause } argument should be {@code null } when no exception
     * is to be passed along.
     * 
     * @param result true if the validation was successful
     * @param message plain text validation result message
     * @param script the script being validated
     * @param cause the exception causing the validation to fail
     * 
     * @return the Validate Response Message
     */
    public static ResponseMessageValidate generateValidateResponse(boolean result, String message, String script, Exception cause) {
        if (cause == null) {
            return new ResponseMessageValidate(result, message, script);
        } else {
            return new ResponseMessageValidate(result, message, script,cause);
        }
    }

    /**
     * @return the message
     */
    public String getMessage() {
        return message;
    }
    /**
     * @param message the message to set
     */
    public void setMessage(String message) {
        this.message = message;
    }
    /**
     * @return the result
     */
    public Boolean getResult() {
        return result;
    }
    /**
     * @param result the result to set
     */
    public void setResult(Boolean result) {
        this.result = result;
    }
    /**
     * @return the script
     */
    public String getScript() {
        return script;
    }
    /**
     * @param script the script to set
     */
    public void setScript(String script) {
        this.script = script;
    }
    
}
