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
package com.raytheon.uf.edex.plugin.text.dbsrv.ingest;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;

/**
 * Site Map NDM subscriber.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 * Jan 06, 2011           bfarmer      Initial creation
 * Mar 06, 2014  2876     mpduff       New NDM plugin.
 * May 20, 2014  2536     bclement     moved from edex.textdb to
 *                                     edex.plugin.text
 * Jan 18, 2016  4562     tjensen      Moved from edex.plugin.text to
 *                                     edex.plugin.text.dbsrv
 * Apr 06, 2017  19619    MPorricelli  Have all edex servers made aware of ndm
 *                                     textdb file change
 * Jan 26, 2018  6863     dgilling     Write received files to CONFIGURED level,
 *                                     remove use of route for cluster
 *                                     synchronization.
 * Feb 07, 2019  7730     randerso     Moved files to textdb/config when adding
 *                                     to localization perspective.
 *
 * </pre>
 *
 * @author bfarmer
 */

public class SiteMapNationalDatasetSubscriber
        implements INationalDatasetSubscriber {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SiteMapNationalDatasetSubscriber.class);

    @Override
    public void notify(String fileName, File file) {
        if (SiteMap.AFOS_LOOKUP_FILENAME.endsWith(fileName)) {
            saveAfosLookupTable(file);
        } else if (SiteMap.NATIONAL_CATEGORY_TABLE_FILENAME
                .endsWith(fileName)) {
            saveNationalCategoryTable(file);
        }
    }

    private void saveNationalCategoryTable(File file) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        ILocalizationFile outFile = pathMgr.getLocalizationFile(lc,
                SiteMap.NATIONAL_CATEGORY_TABLE_FILENAME);
        saveFile(file, outFile);
    }

    private void saveFile(File file, ILocalizationFile outFile) {
        if ((file != null) && file.exists()) {
            try (SaveableOutputStream fos = outFile.openOutputStream()) {
                Files.copy(file.toPath(), fos);
                fos.save();
            } catch (IOException e) {
                statusHandler.error("Error reading from " + file
                        + " or writing to file " + outFile, e);
            } catch (LocalizationException e) {
                statusHandler.error("Error writing to " + outFile, e);
            }
        }
    }

    private void saveAfosLookupTable(File file) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        ILocalizationFile outFile = pathMgr.getLocalizationFile(lc,
                SiteMap.AFOS_LOOKUP_FILENAME);
        saveFile(file, outFile);
    }
}
