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
package com.raytheon.uf.common.dataplugin.warning.response;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Response sent from EDEX when making a {@code VerifyProductDecodeRequest}.
 * Tells requestor whether product successfully decoder, and if it failed, the
 * error trace returned by the decoder.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 23, 2015  #4320     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */
@DynamicSerialize
public final class VerifyProductDecodeResponse {

    @DynamicSerializeElement
    private boolean success;

    @DynamicSerializeElement
    private String errorMessage;

    /**
     * Default constructor--should only be used by DynamicSerialize.
     */
    public VerifyProductDecodeResponse() {
    }

    /**
     * Build a response with the specified success value.
     * 
     * @param success
     *            Whether or not product successfully decoded.
     */
    public VerifyProductDecodeResponse(boolean success) {
        this(success, "");
    }

    /**
     * Build a response with specified success value and error message.
     * 
     * @param success
     *            Whether or not product successfully decoded.
     * @param errorMessage
     *            Error message returned by decoder.
     */
    public VerifyProductDecodeResponse(boolean success, String errorMessage) {
        this.success = success;
        this.errorMessage = errorMessage;
    }

    @Override
    public String toString() {
        return "VerifyProductDecodeResponse [success=" + success
                + ", errorMessage=" + errorMessage + "]";
    }

    public boolean isSuccess() {
        return success;
    }

    public void setSuccess(boolean success) {
        this.success = success;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }
}
