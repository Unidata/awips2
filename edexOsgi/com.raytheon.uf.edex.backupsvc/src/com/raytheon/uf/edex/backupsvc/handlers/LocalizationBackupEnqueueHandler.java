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
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.backupsvc.BackupHost;
import com.raytheon.uf.common.backupsvc.BackupServiceException;
import com.raytheon.uf.common.backupsvc.notification.BackupServiceNotification;
import com.raytheon.uf.common.backupsvc.request.LocalizationBackupEnqueueRequest;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.backup.request.LocalizationFileDeleteRequest;
import com.raytheon.uf.common.localization.backup.request.LocalizationFileSaveRequest;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.serialization.comm.response.GenericResponse;
import com.raytheon.uf.edex.backupsvc.category.LocalizationCategoryManager;
import com.raytheon.uf.edex.backupsvc.database.BackupSvcOutgoingDao;
import com.raytheon.uf.edex.backupsvc.notification.BackupServiceNotificationPublisher;
import com.raytheon.uf.edex.backupsvc.service.BackupServiceConfigManager;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Handler for LocalizationBackupEnqueueRequest as of 21.4.1. This Enqueue
 * handler is for the new features of localization backup for Backup Services.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ------------ --------------------------
 * Nov 10, 2016 5937       tgurney         Initial creation
 * Dec  9, 2016 5937       tgurney         Better config handling
 * Jul 20, 2017 6352       tgurney         Add versionRequired parameters to enqueue
 * Oct  8, 2019 7929       tgurney         Store the request uncompressed
 * May 18, 2021 91044      Robert.Blum     Update BackupEnqueue to new table/Dao.
 * May 25, 2021 91044      Robert.Blum     Fix hostname issues and clean up parameters.
 * Jun 28, 2021 84643      Gang Chen       Add the module to publish the notification.
 * Aug 09, 2021 84654      Robert.Blum     Filter hosts based on context name and domains.
 * Jul 13, 2021 92922      Robert.Blum     Updates for new DAO method signatures.
 * Aug 13, 2021 93179      Amanuel Challa  Moved LocalizationFileSaveRequest/DeleteRequest to
 *                                         com.raytheon.uf.common.localization.backup.request
 *                                         and created local variable for Component DB field
 * Sep 23, 2021 96321      Robert.Blum     Populating Component column on jobs.
 * Oct 11, 2021 97253      Robert.Blum     Rename to LocalizationBackupEnqueueHandler.
 *
 * </pre>
 *
 * @author tgurney
 */

public class LocalizationBackupEnqueueHandler
        implements IRequestHandler<LocalizationBackupEnqueueRequest> {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private LocalizationCategoryManager categoryManager;

    private BackupSvcOutgoingDao outgoingDao;

    private BackupServiceConfigManager configMgr;

    public LocalizationBackupEnqueueHandler() {
        outgoingDao = new BackupSvcOutgoingDao();
        configMgr = BackupServiceConfigManager.getInstance();
        categoryManager = new LocalizationCategoryManager();
    }

    /**
     * Enqueue a request to send to backup hosts. A new job with the same name
     * as a job that is already in queue will supersede that already existing
     * job. If you don't want this behavior then you need to use a unique job
     * name every time you enqueue a job. Note that this method will not create
     * a job (i.e. is a no-op) if no backup hosts are configured.
     *
     * @param request
     *            Request to send
     * @param jobName
     *            Job name
     * @param hostnames
     *            List of hosts to send the request to. If empty, send to all
     *            hosts configured with BackupService
     * @throws SerializationException
     * @throws BackupServiceException
     *             If the enqueue failed. This is either a database-related
     *             error, or a failure to serialize the provided request, or a
     *             problem with BackupService configuration
     */
    private void enqueue(IServerRequest request, String jobName,
            List<String> hostnames)
            throws BackupServiceException, SerializationException {
        LocalizationContext context = null;
        if (request instanceof LocalizationFileSaveRequest) {
            context = ((LocalizationFileSaveRequest) request).getContext();
        } else if (request instanceof LocalizationFileDeleteRequest) {
            context = ((LocalizationFileDeleteRequest) request).getContext();
        }

        List<BackupHost> filteredHosts = new ArrayList<>();
        List<BackupHost> configuredHosts = configMgr.getBackupHosts();
        if (configuredHosts.isEmpty()) {
            throw new BackupServiceException("No backup hosts are configured.");
        }
        if (hostnames != null) {
            for (String hostname : hostnames) {
                for (BackupHost host : configuredHosts) {
                    if (host.getHostName().equals(hostname)) {
                        filteredHosts.add(host);
                    } else {
                        // Host specified that is not configured
                        throw new BackupServiceException("Host " + hostname
                                + " is not a configured backup host.");
                    }
                }
            }
        } else {
            /*
             * Filter out hosts with domains configured that don't include this
             * file.
             */
            for (BackupHost host : configuredHosts) {
                if (host.getFilterDomains() != null
                        && !host.getFilterDomains().isEmpty()) {
                    if ((context.getLocalizationLevel()
                            .equals(LocalizationLevel.SITE)
                            || context.getLocalizationLevel()
                                    .equals(LocalizationLevel.CONFIGURED))
                            && !host.getFilterDomains()
                                    .contains(context.getContextName())) {
                        continue;
                    }
                }
                filteredHosts.add(host);
            }
        }
        if (!filteredHosts.isEmpty()) {
            try {
                byte[] blob = DynamicSerializationManager
                        .getManager(SerializationType.Thrift)
                        .serialize(request);
                String jobFileLocation;
                String jobFilePath;
                List<String> categories = new ArrayList<>();
                IPathManager pathMgr = PathManagerFactory.getPathManager();
                if (request instanceof LocalizationFileSaveRequest) {
                    jobFilePath = ((LocalizationFileSaveRequest) request)
                            .getPath();
                    LocalizationFile lf = pathMgr.getLocalizationFile(context,
                            ((LocalizationFileSaveRequest) request).getPath());
                    jobFileLocation = lf.getFile().getPath();
                    categories = categoryManager
                            .getCategoryForLocalizationFile(lf);

                } else if (request instanceof LocalizationFileDeleteRequest) {
                    jobFilePath = ((LocalizationFileDeleteRequest) request)
                            .getPath();
                    LocalizationFile lf = pathMgr.getLocalizationFile(context,
                            ((LocalizationFileDeleteRequest) request)
                                    .getPath());
                    jobFileLocation = lf.getFile().getPath();
                    categories = categoryManager
                            .getCategoryForLocalizationFile(lf);
                } else {
                    this.logger.error(
                            "Unhandled IServerRequestType received in LocalizationBackupEnqueueHandler.");
                    return;
                }
                jobFileLocation = jobFileLocation.replace(jobFilePath, "");
                String categoryText = categories.stream().map(i -> i.toString())
                        .collect(Collectors.joining(","));
                outgoingDao.createOutBSJ(blob, jobFileLocation, jobFilePath,
                        categoryText, EDEXUtil.getEdexSite(), filteredHosts);
            } catch (Exception e) {
                throw new BackupServiceException(e);
            }
        }
    }

    @Override
    public Object handleRequest(LocalizationBackupEnqueueRequest request)
            throws Exception {
        GenericResponse response = new GenericResponse();
        response.setSuccess(false);

        BackupServiceNotificationPublisher backupSvcNotificationPublisher = new BackupServiceNotificationPublisher();
        try {
            enqueue(request.getRequest(), request.getJobName(),
                    request.getHosts());
            response.setSuccess(true);

            logger.info("Send the OUTGOING notification - "
                    + BackupServiceNotification.TOPIC + " : "
                    + BackupServiceNotification.NotificationType.OUTGOING);
            backupSvcNotificationPublisher.notify(
                    BackupServiceNotification.NotificationType.OUTGOING, true);

        } catch (Exception e) {
            logger.error("Failed to enqueue backup job " + request.getJobName()
                    + ": ", e);
            response.setMessage(e.getMessage());
        }
        return response;
    }

}
