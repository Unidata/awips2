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
package com.raytheon.viz.gfe.textformatter;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.TimeZone;
import java.util.concurrent.atomic.AtomicInteger;

import com.raytheon.uf.common.activetable.SendPracticeProductRequest;
import com.raytheon.uf.common.dataplugin.text.db.MixedCaseProductSupport;
import com.raytheon.uf.common.dissemination.OUPRequest;
import com.raytheon.uf.common.dissemination.OUPResponse;
import com.raytheon.uf.common.dissemination.OfficialUserProduct;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.gfe.dialogs.formatterlauncher.ConfigData;
import com.raytheon.viz.gfe.dialogs.formatterlauncher.ConfigData.ProductStateEnum;
import com.raytheon.viz.ui.simulatedtime.SimulatedTimeOperations;
import com.raytheon.viz.ui.simulatedtime.SimulatedTimeProhibitedOpException;

/**
 * Handles product transmission for GFE text products, using handleOUP for
 * operational products and the {@code SendPracticeProduct} thrift request for
 * practice products.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 28, 2015  #4806     dgilling     Initial creation
 * Oct 01, 2015  #4888     dgilling     Prevent transmission in DRT mode.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class TextProductTransmitter {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextProductTransmitter.class);

    private static final AtomicInteger SEQ_NUMBER = new AtomicInteger();

    private final String productText;

    private final String pil;

    private final String wmoType;

    /**
     * Constructor
     * 
     * @param productText
     *            Product text to transmit
     * @param pil
     *            AWIPS WAN PIL of the product to transmit
     * @param wmoType
     *            WMO type for the product.
     */
    public TextProductTransmitter(String productText, String pil, String wmoType) {
        this.pil = pil;
        this.productText = MixedCaseProductSupport.conditionalToUpper(
                this.pil.substring(4, 7), productText);
        this.wmoType = wmoType;
    }

    /**
     * Transmit the product.
     * 
     * @param practice
     *            Whether to transmit the product as an operational or practice
     *            product.
     * @return Product transmission status.
     * @throws VizException
     *             If the transmission request threw an exception during
     *             server-side processing.
     * @throws SimulatedTimeProhibitedOpException
     *             If transmit is prohibited because GFE is running in DRT mode.
     */
    public ProductStateEnum transmitProduct(boolean practice)
            throws VizException, SimulatedTimeProhibitedOpException {
        if (SimulatedTimeOperations.isTransmitAllowed()) {
            throw SimulatedTimeOperations
                    .constructProhibitedOpException("Transmit GFE text products.");
        }

        IServerRequest req = (!practice) ? constructOperationalRequest()
                : constructPracticeRequest();
        Object response = ThriftClient.sendRequest(req);
        /*
         * Currently processing of the practice product happens async, so as
         * long as the request didn't throw an exception, we can only assume it
         * succeeded.
         */
        ProductStateEnum status = (response instanceof OUPResponse) ? processOupResponse((OUPResponse) response)
                : ProductStateEnum.Transmitted;
        return status;
    }

    private SendPracticeProductRequest constructPracticeRequest() {
        SendPracticeProductRequest practiceReq = new SendPracticeProductRequest();
        practiceReq.setNotifyGFE(true);
        practiceReq.setProductText(productText);
        practiceReq.setDrtString(getDRTString());
        return practiceReq;
    }

    private OUPRequest constructOperationalRequest() {
        OfficialUserProduct oup = new OfficialUserProduct();
        oup.setProductText(productText);

        /*
         * ensure the awipsWanPil is exactly 10 characters space-padded long
         */
        String awipsWanPil = String.format("%-10s", pil.trim());
        oup.setAwipsWanPil(awipsWanPil);

        String tempName = awipsWanPil + "-" + SEQ_NUMBER.incrementAndGet()
                + "-"
                + (System.currentTimeMillis() / TimeUtil.MILLIS_PER_SECOND);
        oup.setFilename(tempName);

        if (!wmoType.equals("rou") && !wmoType.equals("res")) {
            oup.setWmoType(wmoType);
        }
        oup.setNeedsWmoHeader(false);
        oup.setSource("GFE");

        OUPRequest oupReq = new OUPRequest();
        oupReq.setProduct(oup);
        oupReq.setUser(UserController.getUserObject());

        return oupReq;
    }

    private ProductStateEnum processOupResponse(OUPResponse response) {
        ProductStateEnum retVal = null;

        if (response instanceof OUPResponse) {
            OUPResponse resp = response;
            Priority p = null;
            if (!resp.hasFailure()) {
                p = Priority.EVENTA;
                retVal = ConfigData.ProductStateEnum.Transmitted;
            } else {
                // determine the failure type and priority
                if (resp.isSendLocalSuccess()) {
                    retVal = ConfigData.ProductStateEnum.Transmitted;
                } else {
                    retVal = ConfigData.ProductStateEnum.Failed;
                }
                p = Priority.EVENTA;
                if (!resp.isAttempted()) {
                    // if was never attempted to send or store even locally
                    p = Priority.CRITICAL;
                } else if (!resp.isSendLocalSuccess()) {
                    // if send/store locally failed
                    p = Priority.CRITICAL;
                } else if (!resp.isSendWANSuccess()) {
                    // if send to WAN failed
                    if (resp.getNeedAcknowledgment()) {
                        // if ack was needed, if it never sent then no ack
                        // was recieved
                        p = Priority.CRITICAL;
                    } else {
                        // if no ack was needed
                        p = Priority.EVENTA;
                    }
                } else if (resp.getNeedAcknowledgment()
                        && !resp.isAcknowledged()) {
                    // if sent but not acknowledged when acknowledgment is
                    // needed
                    p = Priority.CRITICAL;
                }
            }
            statusHandler.handle(p, resp.getMessage());
        }

        return retVal;
    }

    private static String getDRTString() {
        String time = null;
        if (!SimulatedTime.getSystemTime().isRealTime()) {
            DateFormat gmtFormatter = new SimpleDateFormat("yyyyMMdd_HHmm");
            gmtFormatter.setTimeZone(TimeZone.getTimeZone("GMT"));
            time = gmtFormatter.format(SimulatedTime.getSystemTime().getTime());
        }
        return time;
    }
}
