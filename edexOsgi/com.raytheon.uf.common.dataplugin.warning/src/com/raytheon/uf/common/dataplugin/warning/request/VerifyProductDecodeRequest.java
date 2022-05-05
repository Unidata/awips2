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
package com.raytheon.uf.common.dataplugin.warning.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * {@code IServerRequest} to verify the specified raw message for a warning
 * product successfully can be decoded by the WarningDecoder.
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
public final class VerifyProductDecodeRequest implements IServerRequest {

    @DynamicSerializeElement
    private String productText;

    /**
     * Default constructor--should only be used by DynamicSerialize.
     */
    public VerifyProductDecodeRequest() {

    }

    /**
     * Build a request with the specified raw message.
     * 
     * @param productText
     *            Raw message of the warning product to test through decoder.
     */
    public VerifyProductDecodeRequest(String productText) {
        this.productText = productText;
    }

    @Override
    public String toString() {
        return "VerifyProductDecodeRequest [productText=" + productText + "]";
    }

    public String getProductText() {
        return productText;
    }

    public void setProductText(String productText) {
        this.productText = productText;
    }
}
