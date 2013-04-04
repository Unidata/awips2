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
package com.raytheon.edex.plugin.gfe.tpc;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.gfe.config.GFESiteActivation;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.activetable.VTECPartners;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.UserMessageNotification;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.core.props.EnvProperties;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

/**
 * Watches ingested warnings for WOU products from the SPC (Storm Prediction
 * Center). If the warning is a WOU, then it looks to see if the site is in the
 * ATTN...WFO... line, and if so, sends a user message to GFE to alert users.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2008            njensen     Initial creation
 * Jul 10, 2009  #2590 njensen     Added multiple site support
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TPCWatchSrv {

    private static final Pattern ATTN_WFO = Pattern
            .compile("ATTN\\.\\.\\.WFO\\.\\.\\.([A-Z]{3}\\.\\.\\.)+");

    private static final Map<String, String> phensigMap;

    private static final Map<String, String> actMap;

    static {
        Map<String, String> phensigMapTemp = new HashMap<String, String>();
        phensigMapTemp.put("HU.A", "Hurricane Watch");
        phensigMapTemp.put("HU.S", "Hurricane Local Statement");
        phensigMapTemp.put("HU.W", "Hurricane Warning");
        phensigMap = Collections.unmodifiableMap(phensigMapTemp);

        Map<String, String> actMapTemp = new HashMap<String, String>();
        actMapTemp.put("CON", "Continued");
        actMapTemp.put("CAN", "Cancelled");
        actMapTemp.put("NEW", "New");
        actMap = Collections.unmodifiableMap(actMapTemp);
    }

    private static final String alertTxt = "Alert: {0} has arrived from TPC. "
            + "Check for 'red' locks (owned by others) on your Hazard grid and resolve them. "
            + "If hazards are separated into temporary grids, please run Mergehazards. "
            + "Next...save Hazards grid. Finally, select PlotTPCEvents from Hazards menu.";

    protected transient Log logger = LogFactory.getLog(getClass());

    public void handleTpcWatch(PluginDataObject[] pdos) throws EdexException {

        EnvProperties env = PropertiesFactory.getInstance().getEnvProperties();
        String primarySite = env.getEnvValue("SITENAME");
        String tpcSite = (String) VTECPartners.getInstance(primarySite)
                .getattr("VTEC_TPC_SITE", "KNHC");
        Set<String> activeSites = GFESiteActivation.getInstance()
                .getActiveSites();

        AbstractWarningRecord ourWarn = null;
        String ourSite = null;
        // create the appropriate TPC notification, returns null if not
        // needed.
        Map<String, Set<String>> phensigStormAct = new HashMap<String, Set<String>>();
        for (PluginDataObject pdo : pdos) {
            AbstractWarningRecord warn = (AbstractWarningRecord) pdo;
            if (!warn.getPil().startsWith("TCV")) {
                logger.debug("TPC notification:  not TCV product");
                return;
            }

            // The warning is a TPC, but for us?
            List<String> wfos = getAttnWfos(warn.getRawmessage());
            wfos.retainAll(activeSites);
            if (wfos.size() == 0) {
                logger.debug("TPC notification:  my site not in ATTN list");
                continue;
            }

            if (ourWarn == null) {
                ourWarn = warn;
                ourSite = wfos.get(0);
            }

            // Collect action codes by phensig and storm #
            if (tpcSite.equals(warn.getOfficeid())) {
                String phensig = warn.getPhen() + "." + warn.getSig();
                String storm = warn.getEtn();
                String act = warn.getAct();
                Set<String> psActs = phensigStormAct.get(phensig + ":" + storm);
                if (psActs == null) {
                    psActs = new TreeSet<String>();
                    phensigStormAct.put(phensig + ":" + storm, psActs);
                }
                psActs.add(act);
            }

        }

        if (phensigStormAct.size() == 0) {
            logger.debug("TPC Notification: no HU/TR vtec lines, or not NEW action code");
            return;
        }

        // Build the notification message
        StringBuilder msg = new StringBuilder();
        msg.append(MessageFormat.format(alertTxt, ourWarn.getPil()));

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

        UserMessageNotification notification = new UserMessageNotification(
                msg.toString(), Priority.CRITICAL, "GFE", ourSite);

        SendNotifications.send(notification);

    }

    private static List<String> getAttnWfos(String rawMessage) {
        List<String> list = new ArrayList<String>();

        // decode the ATTN line, which tells us which WFOs are affected
        // only used for WCL and WOU products
        Matcher m = ATTN_WFO.matcher(rawMessage);
        if (m.find()) {
            String found = m.group();
            // eliminate ATTN...WFO...
            found = found.substring(13);
            if (found != null) {
                String[] wfos = found.split("\\.\\.\\.");
                for (String s : wfos) {
                    list.add(s);
                }
            }
        }

        return list;
    }

}
