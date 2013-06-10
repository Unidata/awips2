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
package com.raytheon.edex.plugin.gfe.spc;

import java.util.ArrayList;
import java.util.List;
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

public class SPCWatchSrv {

    private static final Pattern ATTN_WFO = Pattern
            .compile("ATTN\\.\\.\\.WFO\\.\\.\\.([A-Z]{3}\\.\\.\\.)+");

    protected transient Log logger = LogFactory.getLog(getClass());

    public void handleSpcWatch(PluginDataObject[] pdos) throws EdexException {
        // create the appropriate SPC notification, returns null if not
        // needed.
        EnvProperties env = PropertiesFactory.getInstance().getEnvProperties();
        String primarySite = env.getEnvValue("SITENAME");
        String spcSite = (String) VTECPartners.getInstance(primarySite)
                .getattr("VTEC_SPC_SITE", "KWNS");
        for (PluginDataObject pdo : pdos) {
            AbstractWarningRecord warn = (AbstractWarningRecord) pdo;
            if (!warn.getPil().equals("WOU")) {
                logger.debug("SPC notification:  not WOU product");
                return;
            }

            // find the first record from KWNS, SV.A, TO.A in this product
            // action code must be "NEW"
            if (warn.getOfficeid().equals(spcSite)
                    && warn.getSig().equals("A")
                    && (warn.getPhen().equals("TO") || warn.getPhen().equals(
                            "SV")) && warn.getAct().equals("NEW")) {
                // decode the ATTN line, which tells us which WFOs are affected
                List<String> wfos = getAttnWfos(warn.getRawmessage());
                for (String siteid : GFESiteActivation.getInstance()
                        .getActiveSites()) {
                    if (!wfos.contains(siteid)) {
                        logger.debug("SPC notification:  my site not in ATTN list");
                        continue; // not my WFO
                    }

                    // create the message
                    String txt = "";
                    if (warn.getPhen().equals("TO")) {
                        txt = "Tornado Watch";
                    } else if (warn.getPhen().equals("SV")) {
                        txt = "Severe Thunderstorm Watch";
                    }

                    String testText = "";
                    if (warn.getVtecstr().charAt(1) == 'T') {
                        testText = " This is a TEST watch. Please restart the GFE "
                                + "in TEST mode before issuing WCN. ";
                    }
                    String msg = "Alert: "
                            + txt
                            + " "
                            + warn.getEtn()
                            + " has arrived. "
                            + "Check for 'red' locks (owned by others) on your Hazard grid and resolve them. "
                            + "If hazards are separated into temporary grids, please run MergeHazards. "
                            + "Next...save Hazards grid.  Finally, select PlotSPCWatches from the Hazards menu.";
                    msg = msg + testText;

                    UserMessageNotification notification = new UserMessageNotification(
                            msg, Priority.CRITICAL, "GFE", siteid);
                    SendNotifications.send(notification);
                }
            } else {
                logger.debug("SPC notification:  "
                        + "no SV.A, TO.A vtec lines, or not NEW action code");
            }
        }

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
