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
package com.raytheon.uf.edex.backupsvc.category;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.filter.LocalizationFileFilter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * File filter for localization categories. Files are listed in
 * localizationCategories.xml. localizationCategories.xml contains a list of
 * files/paths to include in the defined category.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2021 96321      Robert.Blum Initial creation
 *
 * </pre>
 *
 * * @author Robert.Blum
 *
 */

public class LocalizationCategoryFileFilter {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(LocalizationCategoryFileFilter.class);

    private String categoryName;

    private volatile LocalizationFileFilter filter;

    /**
     * Constructor.
     */
    public LocalizationCategoryFileFilter(String categoryName) {
        this.categoryName = categoryName;
        filter = new LocalizationFileFilter();
        reload();
    }

    /**
     * Reload all filter lists. If any lists fail to load, fall back to an empty
     * list
     */
    public synchronized void reload() {
        LocalizationFileFilter newFilter = new LocalizationFileFilter();
        LocalizationCategoryConfigManager configManager = LocalizationCategoryConfigManager
                .getInstance();

        List<String> includeList = configManager
                .getIncludeListForCategory(categoryName);

        if (includeList == null) {
            includeList = new ArrayList<>();
        }

        try (InputStream is = getInputStream(includeList)) {
            newFilter.addAcceptList(is);
        } catch (IOException | LocalizationException e) {
            statusHandler.error(
                    "Failed to add include file list to Localization category file filter.",
                    e);
        }
        filter = newFilter;
    }

    /**
     * Convert BackupServiceFiles to a single input stream
     *
     * @param fileList
     *            list of localization files
     * @return ByteArrayInputStream of files
     */
    private InputStream getInputStream(List<String> fileList) {
        StringBuilder fileListString = new StringBuilder();

        for (String path : fileList) {
            if (path == null || path.isEmpty()) {
                statusHandler.error(
                        "Could not parse Localization category file list. Either file path is null or empty. ");
                return null;
            }

            // Categories apply to all levels
            for (LocalizationLevel level : LocalizationLevel.values()) {
                // use the format denoted in ILocalizationFileFilter
                fileListString.append(level.toString() + ":" + path);
                fileListString.append(System.lineSeparator());
            }
        }

        return new ByteArrayInputStream(fileListString.toString().getBytes());
    }

    public boolean accept(ILocalizationFile file) {
        return filter.accept(file);
    }

}
