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
package com.raytheon.edex.plugin.gfe.watch;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.activetable.VTECPartners;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;

/**
 * Watches ingested warnings for WOU products from the SPC (Storm Prediction
 * Center). If the warning is a WOU, then it looks to see if the site is in the
 * ATTN...WFO... line, and if so, sends a user message to GFE to alert users.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 03, 2008            njensen      Initial creation
 * Jul 10, 2009  #2590     njensen      Added multiple site support
 * May 12, 2014  #3157     dgilling     Re-factor based on AbstractWatchNotifierSrv.
 * Jun 10, 2014  #3268     dgilling     Re-factor based on AbstractWatchNotifierSrv.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public final class SPCWatchSrv extends AbstractWatchNotifierSrv {

    private static final String SPC_WATCH_TYPE = "SPC";

    private static final String SPC_SUPPORTED_PIL = "WOU";

    private static final String DEFAULT_SPC_SITE = "KNHC";

    private static final String TEST_TEXT_MSG = " This is a TEST watch. Please restart the GFE in TEST mode before issuing WCN. ";

    private static final String ALERT_MSG = "Alert: %s %s has arrived. "
            + "Check for 'red' locks (owned by others) on your Hazard grid and resolve them. "
            + "If hazards are separated into temporary grids, please run MergeHazards. "
            + "Next...save Hazards grid.  Finally, select PlotSPCWatches from the Hazards menu.";

    private static final Map<String, String> phenTextMap;

    static {
        Map<String, String> phenTextMapTemp = new HashMap<String, String>(2, 1f);
        phenTextMapTemp.put("TO", "Tornado Watch");
        phenTextMapTemp.put("SV", "Severe Thunderstorm Watch");
        phenTextMap = Collections.unmodifiableMap(phenTextMapTemp);
    }

    public SPCWatchSrv() {
        super(SPC_WATCH_TYPE, SPC_SUPPORTED_PIL);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.gfe.warning.AbstractWarningNotifierSrv#
     * buildNotification(java.util.List, java.lang.String,
     * com.raytheon.uf.common.activetable.VTECPartners)
     */
    @Override
    protected String buildNotification(List<AbstractWarningRecord> decodedVTEC,
            VTECPartners partnersConfig) {
        Collection<String> spcSites = partnersConfig
                .getSpcSites(DEFAULT_SPC_SITE);

        // find the first record from our configured list of issuing sites.
        // Also this product must be a NEW SV.A or TO.A
        AbstractWarningRecord matchRecord = null;
        for (AbstractWarningRecord e : decodedVTEC) {
            if (spcSites.contains(e.getOfficeid())
                    && e.getSig().equals("A")
                    && ((e.getPhen().equals("TO")) || (e.getPhen().equals("SV")))
                    && e.getAct().equals("NEW")) {
                matchRecord = e;
                break;
            }
        }

        if (matchRecord == null) {
            statusHandler.debug("SPC notification:  "
                    + "no SV.A, TO.A vtec lines, or not NEW action code");
            return null;
        }

        // create the message
        String eventType = phenTextMap.get(matchRecord.getPhen());
        StringBuilder msg = new StringBuilder(String.format(ALERT_MSG,
                eventType, matchRecord.getEtn()));
        if (matchRecord.getVtecstr().charAt(1) == 'T') {
            msg.append(TEST_TEXT_MSG);
        }
        return msg.toString();
    }
}
