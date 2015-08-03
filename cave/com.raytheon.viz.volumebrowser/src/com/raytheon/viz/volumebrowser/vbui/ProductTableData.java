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
package com.raytheon.viz.volumebrowser.vbui;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.viz.volumebrowser.datacatalog.DataCatalogManager;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;

/**
 * 
 * This class contains data for a product in the products table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 09, 2009  2161     lvenable  Initial creation
 * Aug 03, 2015  3861     bsteffen  Move resource creation to ProductCreators
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ProductTableData {

    /**
     * Data Catalog Entry
     */
    private final IDataCatalogEntry catalogEntry;

    /**
     * Display types the product should be loaded as.
     */
    private final Set<DisplayType> displayTypeSet;

    private ProductInventory productInventory;

    /**
     * Constructor.
     * 
     * @param productParms
     *            Product parameters.
     * @param selectedData
     *            Data that contains Sources, Fields, and Planes information.
     * @param time
     *            Time string to be displayed.
     */
    public ProductTableData(IDataCatalogEntry catalogEntry) {
        this.catalogEntry = catalogEntry;
        this.displayTypeSet = new HashSet<DisplayType>();

        DisplayType displayType = DisplayType.CONTOUR;
        try {

            displayType = catalogEntry.getSelectedData().getDisplayTypes()
                    .get(0);

        } catch (Exception e) {
        } // the default has already been set

        // make sure the product at least displays a contour
        this.displayTypeSet.add(displayType);
    }

    /**
     * Copy Constructor
     * 
     * @param productData
     */
    public ProductTableData(final ProductTableData productData) {
        this.displayTypeSet = new HashSet<DisplayType>();
        this.displayTypeSet.addAll(productData.displayTypeSet);
        try {
            // instantiate the appropriate copy constructor
            this.catalogEntry = productData.catalogEntry.getClass()
                    .getConstructor(productData.catalogEntry.getClass())
                    .newInstance(productData.catalogEntry);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Get the product parameters.
     * 
     * @return The product parameters.
     */
    public HashMap<String, RequestConstraint> getProductParms() {
        return getDataCatalog().getProductParameters(catalogEntry);
    }

    /**
     * Get the selected data (data that contains Sources, Fields, and Planes
     * information).
     * 
     * @return The selected data.
     */
    public SelectedData getSelectedData() {
        return catalogEntry.getSelectedData();
    }

    /**
     * Get the time display string.
     * 
     * @return The time display string.
     */
    public String getTime() {
        return getProductInventory().getLatestForecastTime();
    }

    public ProductInventory getProductInventory() {
        if (productInventory == null) {
            productInventory = new ProductInventory(getProductParms());
        }
        return productInventory;
    }

    /**
     * Checks if there is a unique key.
     * 
     * @param key
     *            Unique key.
     * @return True if a unique key is present.
     */
    public boolean hasUniqueKey(String key) {
        if (key.compareTo(getSelectedData().getUniqueKey()) == 0) {
            return true;
        }

        return false;
    }

    /**
     * Get the display type.
     * 
     * @return The display type.
     */
    public Set<DisplayType> getDisplayTypeSet() {
        return this.displayTypeSet;
    }

    public IDataCatalog getDataCatalog() {
        return DataCatalogManager.getDataCatalogManager().getDataCatalog(
                catalogEntry.getSelectedData());
    }

    /**
     * Get the name of the product. Returns either the simple name or the image
     * name depending on if the displayTypeSet contains the IMAGE DisplayType.
     * 
     * @return the name of the product
     */
    public String getName() {
        return getDataCatalog().getName(catalogEntry,
                displayTypeSet.iterator().next());
    }

    /**
     * @param displayType
     * @return
     */
    public String getName(DisplayType displayType) {
        return getDataCatalog().getName(catalogEntry, displayType);
    }

    public IDataCatalogEntry getCatalogEntry() {
        return catalogEntry;
    }

}
