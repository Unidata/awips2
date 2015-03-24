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
package com.raytheon.edex.plugin.warning.handler;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.warning.request.VerifyProductDecodeRequest;
import com.raytheon.uf.common.dataplugin.warning.response.VerifyProductDecodeResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.python.decoder.PythonDecoder;

/**
 * Request handler for {@code VerifyProductDecodeRequest}. Takes specified raw
 * message and runs it through the WarningDecoder (injected via spring/camel)
 * and verifies the product successfully decodes. Returns error trace from
 * decoder if it fails.
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

public final class VerifyProductDecodeHandler implements
        IRequestHandler<VerifyProductDecodeRequest> {

    private final PythonDecoder decoder;

    /**
     * Constructor for this request handler. Intended to be called via
     * spring/camel.
     * 
     * @param decoder
     *            {@code PythonDecoder} instance to use for decoding
     *            verification.
     */
    public VerifyProductDecodeHandler(final PythonDecoder decoder) {
        this.decoder = decoder;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public VerifyProductDecodeResponse handleRequest(
            VerifyProductDecodeRequest request) throws Exception {
        try {
            Map<String, Object> decoderArgs = new HashMap<>(1, 1f);
            decoderArgs.put("text", request.getProductText());
            decoder.decode(decoderArgs);
        } catch (Exception e) {
            return new VerifyProductDecodeResponse(false,
                    e.getLocalizedMessage());
        }

        return new VerifyProductDecodeResponse(true);
    }
}
