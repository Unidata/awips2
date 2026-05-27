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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.localization.LocalizationFile;

/**
 * Configuration XML file for localization categories.
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
 * @author Robert.Blum
 */

public class LocalizationCategoryManager {

    private final Logger logger = LoggerFactory.getLogger(this.getClass());

    private LocalizationCategoryConfigManager configManager;

    public LocalizationCategoryManager() {
        configManager = LocalizationCategoryConfigManager.getInstance();
    }

    /**
     * Gets all the Categories from the xml configuration.
     *
     * @return
     */
    public List<Category> getAllCategories() {
        return configManager.getCategories();
    }

    public List<String> getCategoryForLocalizationFile(LocalizationFile lf) {
        Map<String, LocalizationCategoryFileFilter> filterMap = createCategoryFilters();
        List<String> categoriesForFile = new ArrayList<>();
        for (Entry<String, LocalizationCategoryFileFilter> filterEntry : filterMap
                .entrySet()) {
            if (filterEntry.getValue().accept(lf)) {
                categoriesForFile.add(filterEntry.getKey());
            }
        }
        return categoriesForFile;
    }

    private Map<String, LocalizationCategoryFileFilter> createCategoryFilters() {
        Map<String, LocalizationCategoryFileFilter> filterMap = new HashMap<>();
        for (Category cat : getAllCategories()) {
            LocalizationCategoryFileFilter catFilter = new LocalizationCategoryFileFilter(
                    cat.getCategoryName());
            filterMap.put(cat.getCategoryName(), catFilter);
        }
        return filterMap;
    }
}
