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

import com.raytheon.uf.common.auth.util.PermissionUtils;
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
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 14, 2014  2742     rferrel   Initial creation Exclude files with md5
 *                                  checksum extension.
 * Feb 15, 2017  6111     njensen   Overrode getRequestType()
 * Jul 19, 2017  6288     randerso  Changes for new roles/permissions framework
 *
 * </pre>
 *
 * @author rferrel
 */

public class RemoteScriptListHandler extends AbstractRemoteScriptHandler {

    /** Extension for check sum files to remove from listing. */
    private static final String MD5_EXT = ".md5";

    /**
     * Constructor.
     */
    public RemoteScriptListHandler() {
        // The permission id in the common remoteScript.ini
        super(PermissionUtils.buildPermissionString("remoteScript", "list"));
    }

    @Override
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
                    if (!lFile.getPath().trim().endsWith(MD5_EXT)) {
                        result.add(lFile);
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

    @Override
    public Class<?> getRequestType() {
        return RemoteScriptListRequest.class;
    }

}
