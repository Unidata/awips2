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

package com.raytheon.uf.edex.plugin.text;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.common.dataplugin.text.db.AfosToAwips;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Facilities for handling afos2awips records
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------  ----------  -----------  --------------------------
 * Aug 10, 2016  5801        tgurney      Initial creation
 * Jan 20, 2017  6090        tgurney      Add blacklists
 * Jan 30, 2017  6090        tgurney      Allow comments (starting with '#')
 *
 * </pre>
 *
 * @author tgurney
 */

public class AfosToAwipsUtil {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AfosToAwipsUtil.class);

    public static final int AFOSID_LENGTH = 9;

    public static final String AFOS2AWIPS_FILE = "afos2awips"
            + IPathManager.SEPARATOR + "afos2awips.txt";

    public static final String AFOS2AWIPS_BLACKLIST_FILE = "afos2awips"
            + IPathManager.SEPARATOR + "afos2awips.blacklist.txt";

    private AfosToAwipsUtil() {
        // static interface only
    }

    /**
     * Parse single line from afos2awips.txt
     *
     * @param line
     * @return AfosToAwips record. Null if failed to parse or if line is a
     *         comment, starting with '#'
     */
    public static AfosToAwips parseLine(String line) {
        AfosToAwips rval = null;
        if (line.trim().startsWith("#")) {
            // is a comment
            return null;
        }
        String[] parts = line.trim().split("\\s+");
        if (parts.length == 3) {
            rval = new AfosToAwips();
            rval.setAfosid(cleanAfosId(parts[0]));
            rval.setWmottaaii(parts[1].toUpperCase());
            rval.setWmocccc(parts[2].toUpperCase());
        }
        return rval;
    }

    public static String cleanAfosId(String afosid) {
        return StringUtils.rightPad(afosid.trim().toUpperCase(), AFOSID_LENGTH);
    }

    /**
     * @return afos2awips records retrieved in this order: base, configured,
     *         site. Includes blacklists
     */
    public static Set<AfosToAwips> readAllLocalizationFiles() {
        Set<AfosToAwips> rval = new HashSet<>();
        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext baseCtx = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        LocalizationContext configuredCtx = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        LocalizationContext siteCtx = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        Set<AfosToAwips> baseList = readLocalizationFile(baseCtx,
                AFOS2AWIPS_FILE);
        Set<AfosToAwips> configuredList = readLocalizationFile(configuredCtx,
                AFOS2AWIPS_FILE);
        Set<AfosToAwips> configuredBlacklist = readLocalizationFile(
                configuredCtx, AFOS2AWIPS_BLACKLIST_FILE);
        Set<AfosToAwips> siteList = readLocalizationFile(siteCtx,
                AFOS2AWIPS_FILE);
        Set<AfosToAwips> siteBlacklist = readLocalizationFile(siteCtx,
                AFOS2AWIPS_BLACKLIST_FILE);
        rval.addAll(baseList);
        rval.addAll(configuredList);
        rval.removeAll(configuredBlacklist);
        rval.addAll(siteList);
        rval.removeAll(siteBlacklist);
        Set<AfosToAwips> siteConflicts = new HashSet<>(siteList);
        siteConflicts.retainAll(siteBlacklist);
        if (!siteConflicts.isEmpty()) {
            statusHandler.warn("Overlap detected between SITE afos2awips.txt "
                    + " and afos2awips.blacklist.txt. "
                    + "The blacklist takes precedence");
        }
        return rval;
    }

    public static Set<AfosToAwips> readLocalizationFile(LocalizationContext ctx,
            String path) {
        Set<AfosToAwips> rval = new HashSet<>();
        ILocalizationFile file = PathManagerFactory.getPathManager()
                .getLocalizationFile(ctx, path);
        if (file.exists()) {
            try {
                rval = readA2aFile(file.openInputStream());
            } catch (LocalizationException | IOException e) {
                statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(),
                        e);
            }
        }
        return rval;
    }

    public static Set<AfosToAwips> readA2aFile(InputStream is)
            throws IOException {
        Set<AfosToAwips> rval = new HashSet<>();
        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(is))) {
            String line;
            while ((line = reader.readLine()) != null) {
                AfosToAwips a = AfosToAwipsUtil.parseLine(line);
                if (a != null) {
                    rval.add(a);
                }
            }
        }
        return rval;
    }
}
