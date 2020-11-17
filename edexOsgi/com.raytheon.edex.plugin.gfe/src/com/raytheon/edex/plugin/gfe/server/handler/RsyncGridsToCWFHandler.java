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
package com.raytheon.edex.plugin.gfe.server.handler;

import java.io.IOException;

import com.raytheon.uf.common.dataplugin.gfe.request.RsyncGridsToCWFRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.RunProcess;

/**
 * rsync GFE grids to CWF handler
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 15, 2015  #4013     randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class RsyncGridsToCWFHandler implements
        IRequestHandler<RsyncGridsToCWFRequest> {
    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RsyncGridsToCWFHandler.class);

    @Override
    public Object handleRequest(RsyncGridsToCWFRequest request)
            throws Exception {

        String command = "/awips2/GFESuite/bin/rsyncGridsToCWF.sh "
                + request.getSiteID();
        statusHandler.info("Running: \"" + command + "\"");
        RunProcess proc;
        try {
            proc = RunProcess.getRunProcess().exec(command);
        } catch (IOException e) {
            statusHandler.error("Error executing " + command, e);
            return null;
        }

        int exitCode = proc.waitFor();
        if (exitCode != 0) {
            statusHandler.error(command
                    + " terminated abnormally with exit code: " + exitCode);
        }

        return null;
    }

}
