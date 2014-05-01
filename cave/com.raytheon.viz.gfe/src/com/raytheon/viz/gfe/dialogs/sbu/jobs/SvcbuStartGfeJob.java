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

import java.io.IOException;

import com.raytheon.uf.common.dataplugin.gfe.request.GetGfeStartCmdRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.RunProcess;
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
 * Aug 9, 2011            bphillip     Initial creation
 * Sep 19, 2011 10955      rferrel     Use RunProcess
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class SvcbuStartGfeJob extends ServiceBackupJob {

    private String site;

    /**
     * @param name
     */
    public SvcbuStartGfeJob(String name, String primarySite) {
        super("Starting GFE: " + name, primarySite);
        this.site = name;

    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Runnable#run()
     */
    @Override
    public void run() {
        GetGfeStartCmdRequest request = new GetGfeStartCmdRequest(site);
        try {
            ServerResponse<String> sr = makeRequest(request);
            String launchScript = sr.getPayload();
            // DR#10955
            RunProcess.getRunProcess().exec(launchScript.split(" "));
        } catch (GFEServerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "SERVICE BACKUP problem: Unable to get GFE launch script "
                            + site + e.getLocalizedMessage(), e);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "SERVICE BACKUP problem: Error starting new GFE instance! "
                            + site + e.getLocalizedMessage(), e);
        }

    }
}
