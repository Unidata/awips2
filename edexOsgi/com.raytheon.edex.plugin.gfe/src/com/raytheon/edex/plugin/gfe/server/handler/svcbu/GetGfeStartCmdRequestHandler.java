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
package com.raytheon.edex.plugin.gfe.server.handler.svcbu;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import com.raytheon.edex.plugin.gfe.svcbackup.SvcBackupUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.GetGfeStartCmdRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

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
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class GetGfeStartCmdRequestHandler implements
        IRequestHandler<GetGfeStartCmdRequest> {

    @Override
    public Object handleRequest(GetGfeStartCmdRequest request) throws Exception {
        ServerResponse<String> sr = new ServerResponse<String>();
        BufferedReader in = null;
        try {
            SvcBackupUtil.execute("createGFEStartScript", request.getSite());

            String scriptFile = "/awips2/GFESuite/ServiceBackup/.launch_cave.sh";
            in = new BufferedReader(new FileReader(scriptFile));
            String str = null;
            while ((str = in.readLine()) != null) {
                sr.setPayload(str);
            }
            in.close();
        } catch (Exception e) {
            sr.addMessage("Error generating GFE launch script! "
                    + e.getLocalizedMessage());
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    sr.addMessage("Error closing input stream!");
                }
            }
        }
        return sr;
    }
}
