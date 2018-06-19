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
package com.raytheon.viz.gfe.textformatter;

import java.util.Collection;
import java.util.Map;

/**
 * Container object to pass text product configuration data between python and
 * Java.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 29, 2015  #4263     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class TextProductConfigData {

    private final Map<String, String> defaultVtecModes;

    private final Collection<TextProductMetadata> productInventory;

    /**
     * @param defaultVtecModes
     *            The default VTEC mode for each hazard product PIL.
     * @param productInventory
     *            Text product inventory and the associated metadata for each
     *            product.
     */
    public TextProductConfigData(Map<String, String> defaultVtecModes,
            Collection<TextProductMetadata> productInventory) {
        this.defaultVtecModes = defaultVtecModes;
        this.productInventory = productInventory;
    }

    public Map<String, String> getDefaultVtecModes() {
        return defaultVtecModes;
    }

    public Collection<TextProductMetadata> getProductInventory() {
        return productInventory;
    }
}
