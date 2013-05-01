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

import com.raytheon.uf.common.dataplugin.gfe.request.ExportGridsRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.ExportGridsRequest.ExportGridsMode;
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
 * Aug 05, 2011            bphillip     Initial creation
 * Apr 30, 2013  #1761     dgilling     Support changes made to 
 *                                      ExportGridsRequest.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class SvcbuExportDigitalDataJob extends ServiceBackupJob {

    private String site;

    /**
     * @param name
     */
    public SvcbuExportDigitalDataJob(String site) {
        super("Export Digital Data: " + site, site);
        this.site = site;
    }

    @Override
    public void run() {
        ExportGridsRequest request = new ExportGridsRequest(site,
                ExportGridsMode.MANUAL);
        try {
            makeRequest(request);
        } catch (GFEServerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "SERVICE BACKUP problem: Unable to export " + site
                            + "'s digital data to the central server!", e);
        }

    }
}
