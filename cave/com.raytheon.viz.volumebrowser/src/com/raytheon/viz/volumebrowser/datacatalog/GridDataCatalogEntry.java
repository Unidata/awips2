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

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;

/**
 * 
 * A DataCatologEntry which stores some extra grid specific fields.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 25, 2012            bsteffen     Initial javadoc
 * 
 * </pre>
 * 
 * @version 1.0
 */
public class GridDataCatalogEntry extends DataCatalogEntry {

    public GridDataCatalogEntry(SelectedData selData) {
        super(selData);
        this.nameMap = new HashMap<DisplayType, String>();
    }

    public GridDataCatalogEntry(GridDataCatalogEntry entry) {
        super(entry);
        this.nameMap = entry.nameMap;
        this.modelName = entry.modelName;
        this.paramAbbreviation = entry.paramAbbreviation;
        this.selectedPlanesKey = entry.selectedPlanesKey;
    }

    /**
     * The name for each corresponding displayType
     */
    public Map<DisplayType, String> nameMap;

    /**
     * Model Name
     */
    public String modelName;

    /**
     * Grib parameter abbreviation.
     */
    public String paramAbbreviation;

    /**
     * Flag indicating data is available.
     */

    public String selectedPlanesKey;

}
