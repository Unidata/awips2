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
package com.raytheon.edex.plugin.gfe.server.notify;

import java.util.List;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.VTECChange;
import com.raytheon.uf.common.activetable.VTECTableChangeNotification;
import com.raytheon.uf.common.dataplugin.gfe.textproduct.DraftProduct;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Listener to handle VTEC Table Change notifications
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 5, 2012            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class VTECTableChangeListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VTECTableChangeListener.class);

    public void handleNotification(VTECTableChangeNotification notif) {
        for (VTECChange change : notif.getChanges()) {
            checkDrafts(notif.getMode(), change);
        }

    }

    private void checkDrafts(ActiveTableMode tableName, VTECChange change) {
        String siteid = change.getSite();
        String pil = change.getPil();

        statusHandler.handle(Priority.EVENTA, "checkDrafts: " + tableName + ":"
                + siteid + ":" + pil);
        String mode = "Standard";
        if (tableName.equals(ActiveTableMode.PRACTICE)) {
            mode = "PRACTICE";
        }
        String awipspil = siteid + pil; // only the KKKKCCC

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext siteContext = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        String path = FileUtil.join("gfe", "drafts");
        LocalizationFile[] inv = pathMgr.listFiles(siteContext, path, null,
                false, true);

        for (LocalizationFile lf : inv) {
            String[] tokens = lf.getFile().getName().split("-");
            if (tokens.length != 2) {
                continue;
            } // BAD name. All should have a '-'
            String fpil = tokens[0]; // file pil
            String fmode = tokens[1]; // file mode (e.g., Standard)

            // wrong VTEC table
            if (!fmode.equals(mode)) {
                continue;
            }

            boolean markit = false;

            // attempt a match for the pil in the DisableTable of related pils
            List<String> pils = VTECTableChangeNotification.DisableTable
                    .get(pil);
            if (pils != null) {
                markit = pils.contains(fpil.substring(4, 7));
            } else if (awipspil.equals(fpil.substring(0, 7))) {
                markit = true;
            } else if (siteid.equals("*ALL*")) {
                markit = true;
            }

            if (markit) {
                markDraft(lf);
            }
        }
    }

    /**
     * Marks a saved draft as "BAD". This will cause the editor to disallow
     * transmission.
     * 
     * @param lf
     */
    private void markDraft(LocalizationFile lf) {
        statusHandler.handle(Priority.PROBLEM, "Marking draft: "
                + lf.getFile().getName() + " as 'invalid'");

        try {
            DraftProduct draft = DraftProduct.load(lf);
            draft.getProductDefinition().put("brain", 1);
            draft.save(lf);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error marking draft", e);
        }
    }
}
