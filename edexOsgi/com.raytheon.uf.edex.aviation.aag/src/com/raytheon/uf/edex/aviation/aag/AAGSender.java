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
package com.raytheon.uf.edex.aviation.aag;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.text.db.AfosToAwips;
import com.raytheon.uf.common.dissemination.OUPRequest;
import com.raytheon.uf.common.dissemination.OfficialUserProduct;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.plugin.text.AfosToAwipsLookup;

/**
 * Sends Alaska Aviation Guidance text products
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 14, 2017 6110       tgurney     Initial creation
 * May 18, 2017 6110       tgurney     Add second line to header
 *
 * </pre>
 *
 * @author tgurney
 */

public class AAGSender {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private static final String productTag = "AAG";

    private final String thisSite;

    public AAGSender() {
        thisSite = SiteMap.getInstance()
                .getSite4LetterId(EDEXUtil.getEdexSite());
    }

    private String getAwipsWanPil(String stationId) {
        return thisSite + productTag + stationId.substring(1);
    }

    /**
     * @param stationId
     * @return the ttaaii if exactly one was found for the provided station ID.
     *         Otherwise null
     */
    private String getWmoTtaaii(String stationId) {
        String awipsWanPil = getAwipsWanPil(stationId);
        String cccc = awipsWanPil.substring(0, 4);
        String nnn = awipsWanPil.substring(4, 7);
        String xxx = awipsWanPil.substring(7);
        if (awipsWanPil.length() >= 10) {
            xxx = awipsWanPil.substring(7, 10);
        }
        List<AfosToAwips> list = AfosToAwipsLookup.lookupAfosId(cccc, nnn, xxx)
                .getIdList();
        String rval = null;
        if (list.size() == 1) {
            rval = list.get(0).getWmottaaii();
        } else if (list.size() > 1) {
            String fmt = "Got %d different afos2awips entries for AAG product %s. "
                    + "Will not send";
            logger.warn(String.format(fmt, list.size(), awipsWanPil));
        } else {
            logger.warn("Failed to look up afos2awips entry for AAG product "
                    + awipsWanPil);
        }
        return rval;
    }

    private String getHeader(String stationId) {
        String rval = null;
        String ttaaii = getWmoTtaaii(stationId);
        String xxxid = stationId.substring(1);
        if (ttaaii != null) {
            String fmt = "%s %s %s\n%s%s";
            String ts = DateTimeFormatter.ofPattern("ddHHmm")
                    .format(LocalDateTime.now());
            rval = String.format(fmt, ttaaii, thisSite, ts, productTag, xxxid);
        }
        return rval;
    }

    /**
     * @param productText
     *            Product text without header
     * @param stationId
     *            four letter ID (e.g. "PANC")
     * @return true if product was sent successfully
     */
    private boolean sendAAG(String productText, String stationId) {
        OfficialUserProduct oup = new OfficialUserProduct();
        String header = getHeader(stationId);
        boolean success = false;
        if (header != null) {
            oup.setProductText(header + "\n\n" + productText);
            oup.setFilename(
                    header.split("\n")[0].replace(' ', '_') + "_" + stationId
                            + "_" + SimulatedTime.getSystemTime().getMillis());
            oup.setAwipsWanPil(getAwipsWanPil(stationId));
            oup.setNeedsWmoHeader(false);
            oup.setSource("EDEX");
            OUPRequest oupRequest = new OUPRequest();
            oupRequest.setProduct(oup);
            try {
                RequestRouter.route(oupRequest);
                success = true;
            } catch (Exception e) {
                logger.warn("Failed to send AAG product for " + stationId, e);
            }
        }
        return success;
    }

    /**
     * @param forecastTexts
     *            Map of station id to product text without header
     */
    public void sendAAGs(Map<String, String> forecastTexts) {
        if (!forecastTexts.isEmpty()) {
            int sentCount = 0;
            for (Entry<String, String> forecastEntry : forecastTexts
                    .entrySet()) {
                String stationId = forecastEntry.getKey();
                String text = forecastEntry.getValue();
                if (sendAAG(text, stationId)) {
                    sentCount += 1;
                }
            }
            logger.info(String.format("Sent AAG product for %d/%d stations",
                    sentCount, forecastTexts.size()));
        } else {
            logger.info("No products to send");
        }
    }
}
