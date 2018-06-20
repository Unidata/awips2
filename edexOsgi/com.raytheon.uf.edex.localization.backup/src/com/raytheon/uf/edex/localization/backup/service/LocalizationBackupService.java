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
package com.raytheon.uf.edex.localization.backup.service;

import java.io.IOException;
import java.io.InputStream;

import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.backupsvc.request.BackupEnqueueRequest;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.backup.request.LocalizationFileDeleteRequest;
import com.raytheon.uf.common.localization.backup.request.LocalizationFileSaveRequest;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.util.app.AppInfo;
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
 *
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
        // Only accept SITE and CONFIGURED files if they are for our own site
        if ((context.getLocalizationLevel().equals(LocalizationLevel.SITE)
                || context.getLocalizationLevel()
                        .equals(LocalizationLevel.CONFIGURED))
                && !context.getContextName().equals(EDEXUtil.getEdexSite())) {
            return false;
        }
        if (!filter.accept(file)) {
            return false;
        }
        return true;
    }

    private void process(ILocalizationFile file) {
        if (accept(file)) {
            if (file.exists()) {
                submitSaveJob(file);
            } else {
                submitDeleteJob(file);
            }
        }
    }

    private void enqueue(IServerRequest request, ILocalizationFile file) {
        BackupEnqueueRequest enqueueRequest = new BackupEnqueueRequest();
        enqueueRequest.setJobName("LocalizationBackupService:" + file);
        enqueueRequest.setPriority(0);
        enqueueRequest.setRequest(request);
        String myEdexVersion = AppInfo.getInstance().getVersion();
        enqueueRequest.setMinVersionRequired(myEdexVersion);
        enqueueRequest.setMaxVersionRequired(myEdexVersion);
        try {
            RequestRouter.route(enqueueRequest);
        } catch (Exception e) {
            logger.warn("Backup job enqueue failed for " + file, e);
        }
    }

    private void submitSaveJob(ILocalizationFile file) {
        byte[] contents = null;
        try (InputStream inStream = file.openInputStream()) {
            try {
                contents = IOUtils.toByteArray(inStream);
            } catch (IOException e) {
                logger.warn("Failed to read localization file", e);
            }
        } catch (LocalizationException e) {
            logger.warn("Failed to read localization file", e);
        } catch (IOException e) {
            logger.warn("Error on localization file stream open/close", e);
        }
        if (contents != null) {
            LocalizationFileSaveRequest request = new LocalizationFileSaveRequest();
            request.setPath(file.getPath());
            request.setBytes(contents);
            request.setContext(file.getContext());
            // TODO allow specifying priority through config file?
            enqueue(request, file);
        } else {
            logger.warn("Did not enqueue backup job for " + file.getPath()
                    + " because of the above errors.");
        }
    }

    private void submitDeleteJob(ILocalizationFile file) {
        LocalizationFileDeleteRequest request = new LocalizationFileDeleteRequest();
        request.setPath(file.getPath());
        request.setContext(file.getContext());
        // TODO allow specifying priority through config file?
        enqueue(request, file);
    }

    public void reloadConfig() {
        /* Called from spring to reload at fixed interval */
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
            pathObserver = (ILocalizationFile file) -> {
                process(file);
            };
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
