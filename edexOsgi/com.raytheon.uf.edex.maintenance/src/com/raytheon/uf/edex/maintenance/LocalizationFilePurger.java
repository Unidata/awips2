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
package com.raytheon.uf.edex.maintenance;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.regex.Pattern;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * This class should be triggered via a camel cron route to periodically delete
 * {@link LocalizationFile} instances older than the specified purge threshold
 * from the specified localization path. Optionally, a file name pattern may be
 * specified which will be used to filter which files will be purged.
 * <p>
 * This class has some limitations when trying to purge from specific
 * {@link LocalizationLevel}s. With CONFIGURED and SITE levels, this class can
 * only purge from the primary site's (that is, AW_SITE_IDENTIFIER) localization
 * directories. This class cannot purge from the USER level because the EDEX
 * localization code does not know how to list all available user directories.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2016  #5458     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class LocalizationFilePurger {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(LocalizationFilePurger.class);

    private static final String DELETE_ERROR_MSG_FORMAT = "Could not delete localization file [%s]";

    private final String rootPath;

    private final long purgeAge;

    private final Pattern matcher;

    private final LocalizationContext[] contexts;

    public LocalizationFilePurger(String rootDirectory, long purgeAge,
            LocalizationType localizationType, LocalizationLevel[] levels) {
        this(rootDirectory, purgeAge, ".*", localizationType, levels);
    }

    public LocalizationFilePurger(String rootDirectory, long purgeAge,
            String filenamePattern, LocalizationType localizationType,
            LocalizationLevel[] levels) {
        this.rootPath = rootDirectory;
        this.purgeAge = purgeAge;
        this.matcher = Pattern.compile(filenamePattern);

        /*
         * TODO: rewrite this code if EDEXLocalizationAdapter ever properly
         * supports getContextList. It would allow support of purging USER level
         * files and purging for sites other than AW_SITE_IDENTIFIER.
         */
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        List<LocalizationContext> contextList = new ArrayList<>(levels.length);
        for (LocalizationLevel level : levels) {
            contextList.add(pathMgr.getContext(localizationType, level));
        }
        this.contexts = contextList.toArray(new LocalizationContext[0]);
    }

    public void purge() {
        statusHandler.info("Purging files older than "
                + TimeUtil.prettyDuration(purgeAge) + " from " + rootPath);
        Date purgeTime = new Date(TimeUtil.newDate().getTime() - purgeAge);

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        ILocalizationFile[] files = pathMgr.listFiles(contexts, rootPath, null,
                false, true);
        for (ILocalizationFile file : files) {
            String fileName = LocalizationUtil.extractName(file.getPath());
            boolean fileNameMatches = matcher.matcher(fileName).matches();
            boolean timeStampMatches = file.getTimeStamp().before(purgeTime);
            if ((fileNameMatches) && (timeStampMatches)) {
                try {
                    file.delete();
                } catch (LocalizationException e) {
                    statusHandler.handle(Priority.WARN,
                            String.format(DELETE_ERROR_MSG_FORMAT, file), e);
                }
            }
        }
    }
}
