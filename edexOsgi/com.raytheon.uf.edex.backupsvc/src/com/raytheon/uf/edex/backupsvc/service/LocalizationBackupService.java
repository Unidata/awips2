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
package com.raytheon.uf.edex.backupsvc.service;

import java.util.HashSet;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.backupsvc.request.LocalizationBackupEnqueueRequest;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.backup.request.LocalizationFileDeleteRequest;
import com.raytheon.uf.common.localization.backup.request.LocalizationFileSaveRequest;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.IContextStateProcessor;

/**
 * Service that listens for changes to localization files and pushes changed
 * files to backup hosts.
 *
 * This is a clustered singleton service. It must only be instantiated from
 * Spring and only in one place.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov  9, 2016  5937       tgurney     Initial creation
 * Dec 20, 2016  5937       tgurney     Fix reloadConfig() NPE
 * Jul 20, 2017  6352       tgurney     Specify required EDEX version on enqueue
 *                                      request
 * Oct  3, 2019  7929       tgurney     Set required version to be the sender's
 *                                      version at the time the job would be
 *                                      processed (not at the time the job
 *                                      is created).
 * Apr 15 2021  84655      Lisa.Singh   Combined localizationBackupList.txt
 *                                      and backupSvc.xml into one file.
 * Aug 09, 2021 84654      Robert.Blum  Service now sends files for configured
 *                                      domains as well.
 * Sep 20, 2021 96176      Lisa.Singh   Clarified logging comments.
 * Oct 11, 2021 97253      Robert.Blum  Update to use LocalizationBackupEnqueueRequest.
 * </pre>
 *
 * @author tgurney
 */

public class LocalizationBackupService implements IContextStateProcessor {

    private final Logger logger = LoggerFactory.getLogger(this.getClass());

    private LocalizationBackupFileFilter filter;

    private ILocalizationPathObserver pathObserver;

    private IPathManager pathMgr = PathManagerFactory.getPathManager();

    private boolean accept(ILocalizationFile file) {
        LocalizationContext context = file.getContext();
        if (context.getLocalizationLevel().equals(LocalizationLevel.BASE)) {
            return false;
        }
        // Only accept SITE and CONFIGURED files if they are for our own site or
        // local domains
        BackupServiceConfigManager configManager = BackupServiceConfigManager
                .getInstance();
        Set<String> sitesToSendFilesFor = new HashSet<>();
        if (configManager.getLocalDomains() != null) {
            sitesToSendFilesFor.addAll(configManager.getLocalDomains());
        }
        sitesToSendFilesFor.add(EDEXUtil.getEdexSite());
        if ((context.getLocalizationLevel().equals(LocalizationLevel.SITE)
                || context.getLocalizationLevel()
                        .equals(LocalizationLevel.CONFIGURED))
                && !sitesToSendFilesFor.contains(context.getContextName())) {
            return false;
        }
        if (!filter.accept(file)) {
            logger.debug("Changes have been detected in " + file.getPath()
                    + " but file is excluded from backups.");
            return false;
        }
        logger.info("Changes accepted for file " + file.getPath());
        return true;
    }

    private void process(ILocalizationFile file) {
        logger.info(
                "Processing file changes for backup file:" + file.getPath());
        if (accept(file)) {
            try {
                if (file.exists()) {
                    enqueue(new LocalizationFileSaveRequest(file), file);
                } else {
                    enqueue(new LocalizationFileDeleteRequest(file), file);
                }
            } catch (Exception e) {
                logger.warn("Failed to create backup service job for " + file,
                        e);
            }
        }
    }

    private void enqueue(IServerRequest request, ILocalizationFile file)
            throws Exception {
        LocalizationBackupEnqueueRequest enqueueRequest = new LocalizationBackupEnqueueRequest();
        enqueueRequest.setJobName("LocalizationBackupService:" + file);
        enqueueRequest.setRequest(request);
        RequestRouter.route(enqueueRequest);
    }

    /** Called from spring to reload at fixed interval */
    public void reloadConfig() {
        if (filter != null) {
            filter.reload();
        }
    }

    @Override
    public void preStart() {
    }

    @Override
    public void postStart() {
        if (filter == null) {
            filter = new LocalizationBackupFileFilter();
        } else {
            reloadConfig();
        }
        if (pathObserver == null) {
            pathObserver = this::process;
        }
        pathMgr.addLocalizationPathObserver("", pathObserver);
    }

    @Override
    public void preStop() {
        if (pathObserver != null) {
            pathMgr.removeLocalizationPathObserver(pathObserver);
        }
    }

    @Override
    public void postStop() {
    }

}
