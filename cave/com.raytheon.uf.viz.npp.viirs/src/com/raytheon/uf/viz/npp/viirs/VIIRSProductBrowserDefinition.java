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
package com.raytheon.uf.viz.npp.viirs;

import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.npp.viirs.rsc.VIIRSResourceData;
import com.raytheon.uf.viz.productbrowser.AbstractRequestableProductBrowserDataDefinition;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 6, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSProductBrowserDefinition extends
        AbstractRequestableProductBrowserDataDefinition<VIIRSResourceData> {

    public VIIRSProductBrowserDefinition() {
        productName = "viirs";
        displayName = "VIIRS";
        order = new String[] { "region", "channelType", "wavelength" };
        order = getOrder();
        loadProperties = new LoadProperties();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.productbrowser.
     * AbstractRequestableProductBrowserDataDefinition#getResourceData()
     */
    @Override
    public VIIRSResourceData getResourceData() {
        VIIRSResourceData resourceData = new VIIRSResourceData();
        resourceData.setGroupTimeRangeMinutes(30);
        return resourceData;
    }

}
