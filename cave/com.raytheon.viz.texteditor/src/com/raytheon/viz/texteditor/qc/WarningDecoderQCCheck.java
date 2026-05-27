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
package com.raytheon.viz.texteditor.qc;

import java.text.DateFormat;
import java.text.SimpleDateFormat;

import com.raytheon.uf.common.dataplugin.warning.request.VerifyProductDecodeRequest;
import com.raytheon.uf.common.dataplugin.warning.response.VerifyProductDecodeResponse;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * {@code IQCCheck} implementation that makes a thrift request to decode the
 * potential WarnGen product with the WarningDecoder to verify a product will
 * successfully decode.
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

public final class WarningDecoderQCCheck implements IQCCheck {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(WarningDecoderQCCheck.class);

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.texteditor.qc.IQCCheck#runQC(java.lang.String,
     * java.lang.String, java.lang.String)
     */
    @Override
    public String runQC(String header, String body, String nnn) {
        try {
            IServerRequest request = new VerifyProductDecodeRequest(
                    constructTextProduct(header, body));
            VerifyProductDecodeResponse response = (VerifyProductDecodeResponse) ThriftClient
                    .sendRequest(request);

            if (response.isSuccess()) {
                return "";
            } else {
                statusHandler.error("Product failed WarningDecoder QC check.\n"
                        + response.getErrorMessage());
                return "Product failed WarningDecoder. Check AlertViz for more information.";
            }
        } catch (VizException e) {
            statusHandler
                    .error("Unable to contact EDEX server to perform WarningDecoder QC check.",
                            e);
            return "Unable to contact EDEX server.";
        }
    }

    private String constructTextProduct(String header, String body) {
        DateFormat wmoTimeFormat = new SimpleDateFormat("ddHHmm");
        wmoTimeFormat.setTimeZone(TimeUtil.GMT_TIME_ZONE);
        String wmoTimeString = wmoTimeFormat.format(SimulatedTime
                .getSystemTime().getTime());
        String fixedHeader = header.replace("DDHHMM", wmoTimeString);
        return fixedHeader + "\n" + body;
    }
}
