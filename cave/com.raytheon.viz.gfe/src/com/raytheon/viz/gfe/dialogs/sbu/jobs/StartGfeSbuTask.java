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

import com.raytheon.uf.common.dataplugin.gfe.request.AbstractGfeRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetGfeStartCmdRequest;
import com.raytheon.uf.common.dataplugin.gfe.svcbu.JobProgress;
import com.raytheon.uf.common.util.RunProcess;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.GFEServerException;

/**
 * Service backup task to start a GFE session
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 18, 2015  #4300     randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class StartGfeSbuTask extends ServerRequestSbuTask {

    private String site;

    /**
     * @param site
     * @param statusFileName
     * @param guiDescription
     */
    public StartGfeSbuTask(String site, String statusFileName,
            String guiDescription) {
        super(statusFileName, guiDescription, new GetGfeStartCmdRequest(site));
        this.site = site;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.dialogs.sbu.jobs.ServerRequestSbuTask#runTask()
     */
    @Override
    public JobProgress runTask() throws VizException {
        JobProgress rval = JobProgress.FAILED;

        if (request instanceof AbstractGfeRequest) {
            ((AbstractGfeRequest) request).setWorkstationID(VizApp.getWsId());
        }

        Object obj = ThriftClient.sendRequest(request);
        if (obj instanceof String) {
            String cmd = (String) obj;

            if (cmd != null) {
                statusHandler
                        .info("Attempting to start GFE using the following command: "
                                + cmd);
                // start GFE session
                try {
                    RunProcess.getRunProcess().exec(cmd.split(" "));
                    rval = JobProgress.SUCCESS;
                } catch (IOException e) {
                    throw new GFEException("Error starting GFE session for "
                            + this.site, e);
                }
            }
        } else {
            throw new GFEServerException(
                    "Received invalid response object from GFE Server.  Received ["
                            + obj.getClass().getName() + "] excepted ["
                            + String.class.getName());
        }

        return rval;
    }

}
