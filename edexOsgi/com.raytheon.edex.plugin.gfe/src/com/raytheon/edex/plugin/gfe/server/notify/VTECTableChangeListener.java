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
import java.util.Set;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.VTECChange;
import com.raytheon.uf.common.activetable.VTECTableChangeNotification;
import com.raytheon.uf.common.dataplugin.gfe.textproduct.DraftProduct;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.FileUpdatedMessage.FileChangeType;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Listener to handle VTEC Table Change notifications
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun  5, 2012            randerso    Initial creation
 * Mar 25, 2014 #2884      randerso    Added xxxid to check for disabling drafts
 *                                     Fixed to work with sites other than the EDEX site
 *                                     Added work around to Localization not sending
 *                                     FileUpdatedMessages on EDEX
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
        String officeId = change.getSite();
        String pil = change.getPil();
        String xxxid = change.getXxxid();
        String awipspil = officeId + pil + xxxid; // the KKKKCCCXXX

        statusHandler.handle(Priority.EVENTA, "checkDrafts: " + tableName + ":"
                + awipspil);
        String mode = "Standard";
        if (tableName.equals(ActiveTableMode.PRACTICE)) {
            mode = "PRACTICE";
        }

        Set<String> siteList = SiteMap.getInstance()
                .getSite3LetterIds(officeId);

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext[] contexts = new LocalizationContext[siteList
                .size()];
        int i = 0;
        for (String siteId : siteList) {
            contexts[i++] = pathMgr.getContextForSite(
                    LocalizationType.CAVE_STATIC, siteId);
        }
        String path = FileUtil.join("gfe", "drafts");
        LocalizationFile[] inv = pathMgr.listFiles(contexts, path, null, false,
                true);

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

            // attempt a match for the pil in the DisableTable of related
            // pils
            List<String> pils = VTECTableChangeNotification.DisableTable
                    .get(pil);
            if (pils != null) {
                markit = pils.contains(fpil.substring(4, 7))
                        && xxxid.equals(fpil.substring(7, fpil.length()));
            } else if (awipspil.equals(fpil)) {
                markit = true;
            } else if (officeId.equals("*ALL*")) {
                // This is for the clear hazards GUI.
                markit = true;
            }

            if (markit) {
                markDraft(lf);

                // TODO: remove sending of FileUpdateMessage after DR #2768 is
                // fixed
                try {
                    EDEXUtil.getMessageProducer().sendAsync(
                            "utilityNotify",
                            new FileUpdatedMessage(lf.getContext(), lf
                                    .getName(), FileChangeType.UPDATED, lf
                                    .getTimeStamp().getTime()));
                } catch (EdexException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
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
