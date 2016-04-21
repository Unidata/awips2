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
package com.raytheon.viz.volumebrowser.datacatalog;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;

/**
 * 
 * The DataCatalog provides product availability information and can be used for
 * requesting products from the database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 27, 2009 #2161      lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public interface IDataCatalog {

    /**
     * Get the product parameters
     * 
     * @return The product parameters.
     */
    HashMap<String, RequestConstraint> getProductParameters(
            IDataCatalogEntry catalogEntry);

    /**
     * 
     * @param selectedData
     *            the selected data to use to lookup a catalog entry
     * @return a DataCatalogEntry if an entry is found, null otherwise
     */
    IDataCatalogEntry getCatalogEntry(SelectedData selectedData);

    /**
     * 
     * @param catalogEntry
     *            entry for which to obtain the name
     * @param displayType
     *            indicate which product name to obtain. Some products have
     *            different names depending on what the displayType is.
     * @return the name of the entry as it should appear in the volume browser
     */
    String getName(IDataCatalogEntry catalogEntry, DisplayType displayType);

    /**
     * Given the selected sources, fields, and planes a data catalog will add
     * any available items to the appropriate queue. Data Catalogs should
     * respond to a Thread.interrupt() to ensure quick user interaction
     * 
     * @param selectedSources
     * @param selectedFields
     * @param selectedPlanes
     * @param sourceQueue
     * @param fieldQueue
     * @param planeQueue
     * @throws InterruptedException
     */
    void getAvailableData(AvailableDataRequest request);

    /**
     * Get the Request object for a single catalog Entry
     * 
     * @param catalogEntry
     *            the catalog entry for which to obtain a request
     * @param resourceType
     *            the type of request
     * @param displayType
     *            display an image, or contour, etc
     * @return a request that can be used along with the editor returned from
     *         the getEditor() method to request a catalogEntry to be fetched,
     *         loaded, and displayed on the display.
     */
    Collection<ResourcePair> getResourcesToLoad(IDataCatalogEntry catalogEntry,
            ResourceType resourceType, DisplayType displayType);

    /**
     * Get all possible sources for which this catalog can provide data
     * 
     * @return
     */
    public List<String> getSupportedSources();

}
