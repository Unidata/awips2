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
package com.raytheon.edex.plugin.radar.util;

import java.text.SimpleDateFormat;
import java.util.Calendar;

import com.raytheon.uf.common.dataplugin.text.db.OperationalStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.PracticeStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.wmo.AFOSProductId;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2012            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class RadarEdexTextProductUtil {

    private static final transient IUFStatusHandler theHandler = UFStatus
            .getHandler(RadarEdexTextProductUtil.class);

    private static final String TEXT_ENDPOINT = "textDirectDecodedIngestRoute";

    /**
     * A private constructor so that Java does not attempt to create one for us.
     * As this class should not be instantiated, do not attempt to ever call
     * this constructor; it will simply throw an AssertionError.
     * 
     */
    private RadarEdexTextProductUtil() {
        throw new AssertionError();
    }

    public static void storeTextProduct(AFOSProductId afosId, WMOHeader wmoId,
            String text, boolean operationalMode, Calendar cal) {
        SimpleDateFormat sdf = new SimpleDateFormat("MMM dd yyyy HH:mm:ss");
        String temp = "";
        if (wmoId != null && wmoId.isValid()) {
            temp = sdf.format(wmoId.getHeaderDate().getTime());
        } else {
            temp = sdf.format(cal.getTime());
        }
        if (!text.contains("Message Date:")) {
            text = "Message Date: " + temp + "\n\n" + text;
        }
        text = text.replaceAll("Page \\d+", "");
        boolean isValid = wmoId.isValid();
        try {
            if (afosId != null && text != null && !afosId.toString().isEmpty()
                    && !text.isEmpty()) {
                StdTextProduct textProd = (operationalMode == true ? new OperationalStdTextProduct()
                        : new PracticeStdTextProduct());
                if (!isValid) {
                    textProd.setWmoid(" ");
                    textProd.setSite(" ");
                    textProd.setBbbid(" ");
                    textProd.setHdrtime(" ");
                } else {
                    textProd.setWmoid(wmoId.getTtaaii());
                    textProd.setSite(wmoId.getCccc());
                    textProd.setHdrtime(wmoId.getYYGGgg());
                    textProd.setBbbid(wmoId.getBBBIndicator());
                }
                textProd.setRefTime(cal.getTimeInMillis());
                textProd.setCccid(afosId.getCcc());
                textProd.setNnnid(afosId.getNnn());
                textProd.setXxxid(afosId.getXxx());
                textProd.setProduct(text);
                EDEXUtil.getMessageProducer()
                        .sendAsync(TEXT_ENDPOINT, textProd);
            }
        } catch (Exception e) {
            theHandler.handle(Priority.ERROR,
                    "Unable to store product to text database", e);
        }
    }
}
