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
import java.util.Timer;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.filter.LocalizationFileFilter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * File filter for localization backup service. Uses one whitelist and one
 * blacklist with optional site override. Automatically reloads filter lists
 * when changed, and also at a fixed interval (CONFIG_RELOAD_INTERVAL_MS)
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 8, 2016  5937       tgurney     Initial creation
 *
 * </pre>
 *
 * @author tgurney
 */

public class LocalizationBackupFileFilter {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(LocalizationBackupFileFilter.class);

    private static final String ACCEPT_LIST_PATH = "localizationBackup"
            + IPathManager.SEPARATOR + "localizationBackupList.txt";

    private static final String REJECT_LIST_PATH = "localizationBackup"
            + IPathManager.SEPARATOR + "localizationBackupBlacklist.txt";

    private static final LocalizationLevel[] SEARCH_LEVELS = new LocalizationLevel[] {
            LocalizationLevel.BASE, LocalizationLevel.SITE };

    private volatile LocalizationFileFilter filter;

    private final Timer reloadTimer = new Timer();

    private static final long CONFIG_RELOAD_INTERVAL_MS = 5
            * TimeUtil.MILLIS_PER_MINUTE;

    /**
     * Constructor. Immediately loads all filter lists
     */
    public LocalizationBackupFileFilter() {
        filter = new LocalizationFileFilter();
        reload();
        PathManagerFactory.getPathManager()
                .addLocalizationPathObserver(ACCEPT_LIST_PATH, (file) -> {
                    reload();
                });
        PathManagerFactory.getPathManager()
                .addLocalizationPathObserver(REJECT_LIST_PATH, (file) -> {
                    reload();
                });
    }

    private ILocalizationFile getFile(LocalizationLevel level, String path) {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pathManager
                .getContext(LocalizationType.COMMON_STATIC, level);
        ILocalizationFile file = pathManager.getLocalizationFile(ctx, path);
        return file;
    }

    /**
     * Reload all filter lists. If any lists fail to load, fall back to an empty
     * whitelist and an empty blacklist.
     */
    public synchronized void reload() {
        LocalizationFileFilter newFilter = new LocalizationFileFilter();
        try {
            for (LocalizationLevel level : SEARCH_LEVELS) {
                ILocalizationFile acceptList = getFile(level, ACCEPT_LIST_PATH);
                if (acceptList != null && acceptList.exists()) {
                    try (InputStream is = acceptList.openInputStream()) {
                        newFilter.addAcceptList(is);
                    }
                }
                ILocalizationFile rejectList = getFile(level, REJECT_LIST_PATH);
                if (rejectList != null && rejectList.exists()) {
                    try (InputStream is = rejectList.openInputStream()) {
                        newFilter.addRejectList(is);
                    }
                }
            }
        } catch (IOException | LocalizationException e) {
            statusHandler.handle(Priority.PROBLEM, "Failed to load filter list",
                    e);
            statusHandler.warn("Falling back to empty filter lists");
            newFilter = new LocalizationFileFilter();
        }
        filter = newFilter;
    }

    public boolean accept(ILocalizationFile file) {
        return filter.accept(file);
    }

}
