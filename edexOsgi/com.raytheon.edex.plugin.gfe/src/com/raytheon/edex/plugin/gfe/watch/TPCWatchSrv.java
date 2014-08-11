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
import java.util.Set;
import java.util.TreeSet;

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
 * Jun 10, 2014  #3268     dgilling     Re-factor based on AbstractWatchNotifierSrv.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public final class TPCWatchSrv extends AbstractWatchNotifierSrv {

    private static final String TPC_WATCH_TYPE = "TPC";

    private static final String TPC_SUPPORTED_PIL = "TCV";

    private static final String TPC_SITE_ATTRIBUTE = "VTEC_TPC_SITE";

    private static final String DEFAULT_TPC_SITE = "KNHC";

    private static final String ALERT_TXT = "Alert: %s has arrived from TPC. "
            + "Check for 'red' locks (owned by others) on your Hazard grid and resolve them. "
            + "If hazards are separated into temporary grids, please run Mergehazards. "
            + "Next...save Hazards grid. Finally, select PlotTPCEvents from Hazards menu.";

    private static final Map<String, String> phensigMap;

    private static final Map<String, String> actMap;

    static {
        Map<String, String> phensigMapTemp = new HashMap<String, String>(5, 1f);
        phensigMapTemp.put("HU.A", "Hurricane Watch");
        phensigMapTemp.put("HU.S", "Hurricane Local Statement");
        phensigMapTemp.put("HU.W", "Hurricane Warning");
        phensigMapTemp.put("TR.A", "Tropical Storm Watch");
        phensigMapTemp.put("TR.W", "Tropical Storm Warning");
        phensigMap = Collections.unmodifiableMap(phensigMapTemp);

        Map<String, String> actMapTemp = new HashMap<String, String>(3, 1f);
        actMapTemp.put("CON", "Continued");
        actMapTemp.put("CAN", "Cancelled");
        actMapTemp.put("NEW", "New");
        actMap = Collections.unmodifiableMap(actMapTemp);
    }

    public TPCWatchSrv() {
        super(TPC_WATCH_TYPE, TPC_SUPPORTED_PIL);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.gfe.warning.AbstractWarningNotifierSrv#
     * buildNotification(java.util.List,
     * com.raytheon.uf.common.activetable.VTECPartners)
     */
    @Override
    protected String buildNotification(List<AbstractWarningRecord> decodedVTEC,
            VTECPartners partnersConfig) {
        String tpcSite = partnersConfig.getattr(TPC_SITE_ATTRIBUTE,
                DEFAULT_TPC_SITE).toString();

        // get all VTEC records, assemble unique list of phen/sig and storm#
        Map<String, Set<String>> phensigStormAct = new HashMap<String, Set<String>>();
        for (AbstractWarningRecord e : decodedVTEC) {
            if (tpcSite.equals(e.getOfficeid())) {
                String phensig = e.getPhensig();
                String storm = e.getEtn();
                String act = e.getAct();
                Set<String> psActs = phensigStormAct.get(phensig + ":" + storm);
                if (psActs == null) {
                    psActs = new TreeSet<String>();
                    phensigStormAct.put(phensig + ":" + storm, psActs);
                }
                psActs.add(act);
            }
        }

        if (phensigStormAct.isEmpty()) {
            statusHandler
                    .debug("TPC Notification: no HU/TR vtec lines, or not NEW action code");
            return null;
        }

        // create the message
        StringBuilder msg = new StringBuilder(String.format(ALERT_TXT,
                supportedPIL));
        for (String phensigStorm : phensigStormAct.keySet()) {
            Collection<String> acts = phensigStormAct.get(phensigStorm);
            String[] splitKey = phensigStorm.split(":");
            String phensig = splitKey[0];
            String storm = splitKey[1];

            String t1 = phensigMap.get(phensig);
            if (t1 == null) {
                t1 = phensig;
            }
            msg.append(t1 + ": #" + storm + "(");
            String sep = "";
            for (String a : acts) {
                String a1 = actMap.get(a);
                if (a1 == null) {
                    a1 = a;
                }
                msg.append(sep).append(a1);
                sep = ",";
            }
            msg.append("). ");
        }

        return msg.toString();
    }
}
