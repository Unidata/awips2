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
package com.raytheon.viz.gfe.dialogs.sbu.jobs;

import com.raytheon.uf.common.dataplugin.gfe.request.ExportDataToFailedSiteRequest;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.GFEServerException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class SvcbuExportDigitalDataToFailedSiteJob extends ServiceBackupJob {

    private String failedSite;

    /**
     * @param name
     */
    public SvcbuExportDigitalDataToFailedSiteJob(String primarySite, String failedSite) {
        super("Export Grids to: " + failedSite, primarySite);
        this.failedSite = failedSite;
    }

    @Override
    public void run() {
        ExportDataToFailedSiteRequest request = new ExportDataToFailedSiteRequest(
                primarySite, failedSite);
        try {
            makeRequest(request);
        } catch (GFEServerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "SERVICE BACKUP problem: Unable to export grids to "
                            + failedSite, e);
        }

    }
}
