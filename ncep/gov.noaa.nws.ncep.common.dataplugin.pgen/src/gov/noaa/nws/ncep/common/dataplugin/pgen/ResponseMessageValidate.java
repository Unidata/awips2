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
package gov.noaa.nws.ncep.common.dataplugin.pgen;

import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Represents a validation response from EDEX. Fields are provided to specify
 * validation result (success or failure) and for a result message. Modified
 * from com.raytheon.edex.msg.ResponseMessageValidate: Added annotations for use
 * with ThriftClient and removed script field.
 * <P>
 * A static convenience method
 * {@link #generateValidateResponse(boolean, String, String, Exception)
 * generateValidateResponse } is included to facilitate message creation.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * 19Jul2006                        MW Fegan            Initial Creation
 * 10Aug2006        Task 19         MW Fegan            Updated JavaDoc.
 * 
 * </PRE>
 * 
 * @author mfegan
 * 
 */
@DynamicSerialize
public class ResponseMessageValidate extends AbstractResponseMessage {
    @DynamicSerializeElement
    private Boolean result;

    @DynamicSerializeElement
    private String message;

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
     * @param result
     *            true if the validation was successful
     * @param message
     *            plain text validation result message
     * @param script
     *            the script being validated
     */
    public ResponseMessageValidate(boolean result, String message) {
        super();
        this.result = result;
        this.message = message;
    }

    /**
     * @return the message
     */
    public String getMessage() {
        return message;
    }

    /**
     * @param message
     *            the message to set
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
     * @param result
     *            the result to set
     */
    public void setResult(Boolean result) {
        this.result = result;
    }

}
