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

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import com.raytheon.edex.plugin.gfe.svcbackup.SvcBackupUtil;
import com.raytheon.uf.common.site.notify.ClusterActivationNotification;
import com.raytheon.uf.common.site.notify.SiteActivationNotification;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class ServiceBackupLockManager {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ServiceBackupLockManager.class);

    private static ServiceBackupLockManager instance;

    private ServiceBackupLockManager() {
    }

    public static ServiceBackupLockManager getInstance() {
        if (instance == null) {
            instance = new ServiceBackupLockManager();
        }
        return instance;
    }

    public void handleSiteActivationNotification(
            SiteActivationNotification notify) {
        String site = notify.getModifiedSite();
        File waitLockFile = new File(SvcBackupUtil.getLockDir() + "/"
                + site.toLowerCase() + "waitMode");

        if ((notify instanceof ClusterActivationNotification)) {
            if (notify.isActivation() && waitLockFile.exists()) {
                waitLockFile.delete();
            }
        } else if (notify.isActivation() && notify.isBegin()) {
            try {
                waitLockFile.createNewFile();
            } catch (IOException e) {
                statusHandler.error(
                        "Error creating service backup wait lock file!", e);
            }
        }
    }

    public List<String> getLockFiles() {

        File lockDir = new File(SvcBackupUtil.getLockDir());
        if (lockDir.exists()) {
            return Arrays.asList(lockDir.list());
        } else {
            return Collections.emptyList();
        }
    }
}
