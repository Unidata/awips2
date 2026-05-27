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

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.backupsvc.BackupHost;
import com.raytheon.uf.common.backupsvc.BackupServiceFile;
import com.raytheon.uf.common.backupsvc.request.GetBackupConfigFileRequest;
import com.raytheon.uf.common.backupsvc.response.BackupConfigFileResponse;
import com.raytheon.uf.common.dataplugin.backupsvc.constants.BackupServicesConstants;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.backupsvc.service.BackupServiceConfigManager;

/**
 * Handles GetBackupConfigFileRequest made in the Backup Services Settings
 * Dialog
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/15/2021   84638     Amanuel.Challa    Initial creation
 * Jul 27, 2021 84654     Robert.Blum       Added domains to response object.
 * Aug 30, 2021 93179     Amanuel Challa    Added edex cutoff version to response object.
 * Oct 08, 2021 97253     Robert.Blum       Add purge config value to response object.
 *
 * </pre>
 *
 * @author Amanuel.Challa
 * @version 1.0
 */
public class BackupConfigFileHandler
        implements IRequestHandler<GetBackupConfigFileRequest> {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BackupConfigFileHandler.class);

    public BackupConfigFileHandler() {
        statusHandler.info("Initializing BackupConfigFileHandler.");
    }

    @Override
    public Object handleRequest(GetBackupConfigFileRequest request)
            throws Exception {
        BackupConfigFileResponse response = new BackupConfigFileResponse();
        try {
            response = loadFile();
            return response;
        } catch (Exception e) {
            statusHandler.error(
                    "Unable to process GetBackupConfigFileRequest Request");
            response.setExceptions(e);
        }
        return response;
    }

    /**
     * Reload the latest backupSvc.xml file
     *
     * @return BackupConfigFileResponse
     */
    private BackupConfigFileResponse loadFile() {
        BackupConfigFileResponse response = new BackupConfigFileResponse();
        int pollIntervalSeconds;
        int rateLimitKBps;
        int bigJobSize;
        int purgeTimePeriodDays;
        String edexVersion;
        List<String> hostInfoList = new ArrayList<>();
        List<String> includeFileList = new ArrayList<>();
        List<String> excludeFileList = new ArrayList<>();
        List<String> localDomains = new ArrayList<>();
        try {
            BackupServiceConfigManager configMgr = BackupServiceConfigManager
                    .getInstance();
            configMgr.reload();
            pollIntervalSeconds = configMgr.getPollIntervalSeconds();
            rateLimitKBps = configMgr.getRateLimitKBps();
            bigJobSize = configMgr.getBigJobSize();
            purgeTimePeriodDays = configMgr.getPurgeTimePeriodDays();
            edexVersion = configMgr.getEdexCutoffVersion();
            localDomains = configMgr.getLocalDomains();
            // Extract Host information and add it to a String List
            for (BackupHost host : configMgr.getBackupHosts()) {
                StringBuilder sb = new StringBuilder();
                sb.append(host.getSite());
                sb.append(BackupServicesConstants.COLON_SEPARATOR);

                List<String> domains = host.getFilterDomains();
                if (domains != null && !domains.isEmpty()) {
                    sb.append(String.join(", ", domains));
                }
                sb.append(BackupServicesConstants.COLON_SEPARATOR);
                sb.append(host.getHostName());
                sb.append(BackupServicesConstants.COLON_SEPARATOR);
                sb.append(host.getPort());
                hostInfoList.add(sb.toString());
            }
            /*
             * Extract Included files from BackupServiceFile info in to a List
             * String
             */
            for (BackupServiceFile file : configMgr.getIncludeList()) {
                StringBuilder sb = new StringBuilder();
                sb.append(file.getLevel()
                        + BackupServicesConstants.COLON_SEPARATOR
                        + file.getFilePath()
                        + BackupServicesConstants.SPACE_SEPARATOR);

                includeFileList.add(sb.toString());
            }
            /*
             * Extract Excluded files from BackupServiceFile info in to a List
             * String
             */
            for (BackupServiceFile file : configMgr.getExcludeList()) {
                StringBuilder sb = new StringBuilder();
                sb.append(file.getLevel()
                        + BackupServicesConstants.COLON_SEPARATOR
                        + file.getFilePath()
                        + BackupServicesConstants.SPACE_SEPARATOR);

                excludeFileList.add(sb.toString());
            }
            response.setHostInfoList(hostInfoList);
            response.setIncludeFileList(includeFileList);
            response.setExcludeFileList(excludeFileList);
            response.setPollIntervalSeconds(pollIntervalSeconds);
            response.setRateLimitKBps(rateLimitKBps);
            response.setBigJobSize(bigJobSize);
            response.setPurgeTimePeriodDays(purgeTimePeriodDays);
            response.setEdexCutoffVersion(edexVersion);
            response.setLocalDomains(localDomains);
        } catch (Exception e) {
            statusHandler.error(
                    "Unable to call BackupServiceConfigManager to load backupSvc.xml file");
            response.setExceptions(e);
        }
        return response;
    }
}