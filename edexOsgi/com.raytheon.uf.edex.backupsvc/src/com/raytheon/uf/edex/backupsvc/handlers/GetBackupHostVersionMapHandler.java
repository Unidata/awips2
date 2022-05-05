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
package com.raytheon.uf.edex.backupsvc.handlers;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.backupsvc.BackupHost;
import com.raytheon.uf.common.backupsvc.request.GetBackupHostVersionMapRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.backupsvc.service.BackupServiceConfigManager;

/**
 * Handler for {@link GetBackupHostVersionMapRequest}
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 20, 2017 6352       tgurney     Initial creation
 *
 * </pre>
 *
 * @author tgurney
 */

public class GetBackupHostVersionMapHandler
        implements IRequestHandler<GetBackupHostVersionMapRequest> {

    public GetBackupHostVersionMapHandler() {
    }

    @Override
    public Object handleRequest(GetBackupHostVersionMapRequest request)
            throws Exception {
        BackupServiceConfigManager configMgr = BackupServiceConfigManager
                .getInstance();
        configMgr.reload();
        List<BackupHost> hosts = BackupServiceConfigManager.getInstance()
                .getBackupHosts();
        Map<String, String> hostMap = new HashMap<>();
        for (BackupHost host : hosts) {
            String version = host.getEDEXVersion();
            if (version != null) {
                hostMap.put(host.getName(), version);
            }
        }
        return hostMap;
    }
}
