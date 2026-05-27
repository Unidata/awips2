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
 */
package com.raytheon.uf.viz.spellchecker.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.regex.Pattern;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Utility class for accessing the blacklisted/inappropriate words configured in
 * localization.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket# Engineer    Description
 * ------------ ------- ----------- --------------------------
 * May 10, 2019 7747    mapeters     Initial creation (copied from Hazard Services)
 *
 * </pre>
 *
 * @author mapeters
 */
public class BlacklistedWordsUtil {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BlacklistedWordsUtil.class);

    private static final String BLACKLIST_PATH = "spellchecker"
            + IPathManager.SEPARATOR + "inappropriateWords.txt";

    private static final Pattern COMMENT_PATTERN = Pattern.compile("^#");

    private static final Object LOCK = new Object();

    private static volatile Collection<String> blacklist;

    private static BlacklistFileObserver observer;

    /**
     * Private constructor to prevent instantiation as everything is static.
     */
    private BlacklistedWordsUtil() {
    }

    /**
     * Determine whether or not the given word is blacklisted/inappropriate.
     *
     * @param word
     *            the word to check
     * @return true if blacklisted, false otherwise
     */
    public static boolean isBlacklisted(String word) {
        return getBlacklist().contains(word.toUpperCase());
    }

    /**
     * @return all blacklisted/inapproriate words
     */
    public static Collection<String> getBlacklist() {
        Collection<String> localBlacklist = blacklist;
        if (localBlacklist == null) {
            synchronized (LOCK) {
                localBlacklist = blacklist;
                if (localBlacklist == null) {
                    IPathManager pathMgr = PathManagerFactory.getPathManager();
                    if (observer == null) {
                        // Add an observer the first time this is called
                        observer = new BlacklistFileObserver();
                        pathMgr.addLocalizationPathObserver(BLACKLIST_PATH,
                                observer);
                    }

                    ILocalizationFile blacklistFile = pathMgr
                            .getStaticLocalizationFile(
                                    LocalizationType.CAVE_STATIC,
                                    BLACKLIST_PATH);
                    try {
                        localBlacklist = readBlacklistFile(blacklistFile);
                    } catch (IOException | LocalizationException e) {
                        if (blacklistFile.getContext()
                                .getLocalizationLevel() != LocalizationLevel.BASE) {
                            statusHandler.warn(
                                    "Unable to read spell checker blacklist override file: "
                                            + blacklistFile
                                            + ". Falling back to BASE file.",
                                    e);
                        } else {
                            statusHandler
                                    .error("Unable to read spell checker blacklist file: "
                                            + blacklistFile + ".", e);
                            localBlacklist = Collections.emptySet();
                        }
                    }

                    if (localBlacklist == null) {
                        blacklistFile = pathMgr.getLocalizationFile(
                                pathMgr.getContext(LocalizationType.CAVE_STATIC,
                                        LocalizationLevel.BASE),
                                BLACKLIST_PATH);
                        try {
                            localBlacklist = readBlacklistFile(blacklistFile);
                        } catch (IOException | LocalizationException e) {
                            statusHandler
                                    .error("Unable to read spell checker blacklist file: "
                                            + blacklistFile + ".", e);
                            localBlacklist = Collections.emptySet();
                        }
                    }

                    localBlacklist = Collections
                            .unmodifiableCollection(localBlacklist);
                    blacklist = localBlacklist;
                }
            }
        }
        return localBlacklist;
    }

    /**
     * Read the blacklisted words from the given localization file.
     *
     * @param blacklistFile
     *            the localization file to read the words from
     * @return the blacklisted words
     * @throws IOException
     * @throws LocalizationException
     */
    private static Collection<String> readBlacklistFile(
            ILocalizationFile blacklistFile)
            throws IOException, LocalizationException {
        Collection<String> retVal = new HashSet<>();
        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(blacklistFile.openInputStream()))) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (!COMMENT_PATTERN.matcher(line).find() && !line.isEmpty()) {
                    retVal.add(line);
                }
            }
        }

        return retVal;
    }

    private static class BlacklistFileObserver
            implements ILocalizationPathObserver {

        @Override
        public void fileChanged(ILocalizationFile file) {
            synchronized (LOCK) {
                // Force it to be re-initialized next time it's needed
                blacklist = null;
            }
        }
    }
}
