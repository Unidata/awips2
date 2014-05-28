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
package com.raytheon.uf.edex.remote.script;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.remote.script.RemoteScriptListRequest;
import com.raytheon.uf.common.remote.script.RemoteScriptListResponse;
import com.raytheon.uf.common.remote.script.RemoteScriptRequest;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Handler to get the remote script list.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 14, 2014 2742       rferrel     Initial creation
 *                                      Exclude files with md5 checksum extension.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class RemoteScriptListHandler extends AbstractRemoteScriptHandler {

    /** Extension for check sum files to remove from listing. */
    private final String MD5_EXT = ".md5";

    /**
     * Constructor.
     */
    public RemoteScriptListHandler() {
        // The role id in the common remoteScriptAdminRoles.xml
        super("remote.script.list");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.remote.script.RemoteScriptHandler#performRequest
     * (com.raytheon.uf.common.remote.script.RemoteScriptRequest)
     */
    public Object performRequest(RemoteScriptRequest request) {
        IPathManager pm = PathManagerFactory.getPathManager();
        RemoteScriptListRequest req = (RemoteScriptListRequest) request;

        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            statusHandler.handle(Priority.DEBUG,
                    String.format("Request: %s", req));
        }

        LocalizationContext[] ctxs = req.getContexts();

        RemoteScriptListResponse result = new RemoteScriptListResponse();

        for (LocalizationContext ctx : ctxs) {
            LocalizationFile[] lFiles = pm.listFiles(ctx, scriptsDirectory,
                    null, false, true);
            if ((lFiles != null) && (lFiles.length > 0)) {
                for (LocalizationFile lFile : lFiles) {
                    if (!lFile.getName().trim().endsWith(MD5_EXT)) {
                        result.add(lFile);
                        System.out.println(lFile.getFile().getAbsolutePath());
                    }
                }
            }
        }

        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            statusHandler.handle(Priority.DEBUG,
                    String.format("Results: %s", result));
        }

        return result;
    }

}
