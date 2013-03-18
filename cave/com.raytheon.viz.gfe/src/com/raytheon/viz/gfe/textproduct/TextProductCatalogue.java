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
package com.raytheon.viz.gfe.textproduct;

import java.util.HashMap;
import java.util.Vector;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.gfe.core.msgs.ITextProductListChangedListener;

/**
 * Keeps track of the available Text Products and Utilities
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#    Engineer    Description
 * ------------  ---------- ----------- --------------------------
 * Sep 19, 2008  1562       askripsky   Initial creation
 * Mar 06  2013  15717      jzeng       Change CAVE_STATIC to COMMON_STATIC
 *                                      for GFE localization files
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class TextProductCatalogue {

    private static TextProductCatalogue instance;

    private static Vector<ITextProductListChangedListener> listListeners;

    // Catalogue to keep track of the products and utilities
    private HashMap<String, HashMap<String, LocalizationFile>> catalogue;

    private TextProductCatalogue() {
        catalogue = new HashMap<String, HashMap<String, LocalizationFile>>();
        catalogue.put(TextProductUtils.PRODUCT,
                new HashMap<String, LocalizationFile>());
        catalogue.put(TextProductUtils.UTILITIES,
                new HashMap<String, LocalizationFile>());

        listListeners = new Vector<ITextProductListChangedListener>();

        updateCatalogue();
    }

    /**
     * Adds a listener to be used when the catalog changes
     * 
     * @param newListener
     *            A listener to be triggered when the catalogue changes
     */
    public void addListener(ITextProductListChangedListener newListener) {
        listListeners.add(newListener);
    }

    /**
     * Refreshes the catalogue with the current products and utilities
     */
    private void updateCatalogue() {
        updateUtilities();
        updateProducts();

        notifyChangeListeners();
    }

    /**
     * Updates the Utilities in the catalogue. It will first search the Base
     * context and then the Site context. If a script is found in both
     * locations, the Site instance will take precedence.
     * 
     */
    private void updateUtilities() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();

        // Retrieve the current set of Utilities from the Base
        LocalizationContext baseContext = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);

        LocalizationFile[] baseUtilitiesFiles = pathMgr.listFiles(baseContext,
                TextProductUtils.UTILITIES_PATH,
                new String[] { TextProductUtils.EXTENSION }, false, true);

        // Retrieve the current set of Utilities from the Site
        LocalizationContext siteContext = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);

        LocalizationFile[] siteUtilitiesFiles = pathMgr.listFiles(siteContext,
                TextProductUtils.UTILITIES_PATH,
                new String[] { TextProductUtils.EXTENSION }, false, true);

        // temp array for parsing out the file name
        String[] filePath;

        // Add the files from the base to the catalogue
        for (LocalizationFile currFile : baseUtilitiesFiles) {
            filePath = currFile.toString().split(FileUtil.fileSeparatorRegex);
            addEntry(filePath[filePath.length - 1], currFile,
                    TextProductUtils.UTILITIES);
        }

        // Add the files from the site to the catalogue
        for (LocalizationFile currFile : siteUtilitiesFiles) {
            filePath = currFile.toString().split(FileUtil.fileSeparatorRegex);
            addEntry(filePath[filePath.length - 1], currFile,
                    TextProductUtils.UTILITIES);
        }

    }

    /**
     * Scans the appropriate directories and
     */
    private void updateProducts() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();

        LocalizationContext siteContext = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);

        // Gets the the current products
        LocalizationFile[] productsFiles = pathMgr.listFiles(siteContext,
                TextProductUtils.PRODUCTS_PATH,
                new String[] { TextProductUtils.EXTENSION }, false, true);

        String[] filePath;

        // Add the current products to the catalogue
        for (LocalizationFile currFile : productsFiles) {
            filePath = currFile.toString().split(FileUtil.fileSeparatorRegex);
            addEntry(filePath[filePath.length - 1], currFile,
                    TextProductUtils.PRODUCT);
        }
    }

    /**
     * Returns the instance of catalogue
     * 
     * @return The instance of the catalogue
     */
    public static TextProductCatalogue getInstance() {
        if (instance == null) {
            instance = new TextProductCatalogue();
        }

        return instance;
    }

    public void addEntry(String entryName, LocalizationFile entryFile,
            String entryType) {
        if (entryType.compareTo(TextProductUtils.PRODUCT) == 0
                || entryType.compareTo(TextProductUtils.UTILITIES) == 0) {
            (catalogue.get(entryType)).put(entryName, entryFile);

            notifyChangeListeners();
        }
    }

    /**
     * Notifies the listeners that the catalogue has changed
     */
    private void notifyChangeListeners() {
        for (ITextProductListChangedListener listener : listListeners) {
            listener.textProductListChanged();
        }
    }

    /**
     * Removes a product or utility from the catalogue
     * 
     * @param entryName
     *            Name of the catalogue entry to remove
     * @param entryType
     *            Whether the entry is either a product or utility
     */
    public void removeEntry(String entryName, String entryType) {
        if (!entryName.endsWith(TextProductUtils.EXTENSION)) {
            entryName += TextProductUtils.EXTENSION;
        }

        catalogue.get(entryType).remove(entryName);

        // reload catalog in case a Base instance should replace a recently
        // removed Site instance
        updateCatalogue();
    }

    /**
     * Retrieve the localization file for a catalogue entry
     * 
     * @param entryName
     *            Name of the catalogue entry to retrieve
     * @param entryType
     *            Whether the entry is either a product or utility
     * @return The locatization file of the entry
     */
    public LocalizationFile getEntry(String entryName, String entryType) {
        LocalizationFile rval = null;

        if (!entryName.endsWith(TextProductUtils.EXTENSION)) {
            entryName += TextProductUtils.EXTENSION;
        }

        if (entryType.compareTo(TextProductUtils.PRODUCT) == 0
                || entryType.compareTo(TextProductUtils.UTILITIES) == 0) {
            rval = (catalogue.get(entryType)).get(entryName);
        }

        return rval;
    }

    /**
     * Retrieves the names of the text products or utilities in the catalogue
     * 
     * @param entryType
     *            Type of names to retrieve, either product or utilities
     * @return The names of the selected type currently in the catalogue
     */
    public String[] getNames(String entryType) {
        String[] rval = { "" };
        int i = 0;

        if (entryType.compareTo(TextProductUtils.PRODUCT) == 0
                || entryType.compareTo(TextProductUtils.UTILITIES) == 0) {
            HashMap<String, LocalizationFile> tempHash = catalogue
                    .get(entryType);

            rval = new String[tempHash.size()];

            for (String currKey : tempHash.keySet()) {
                rval[i++] = currKey;
            }
        }

        return rval;
    }

    /**
     * Removes the specified listener
     * 
     * @param listListener
     *            The listener to remove
     */
    public void removeListener(ITextProductListChangedListener listListener) {
        listListeners.remove(listListener);
    }
}
