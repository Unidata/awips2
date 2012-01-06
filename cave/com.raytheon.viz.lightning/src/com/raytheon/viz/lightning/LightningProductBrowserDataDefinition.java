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
package com.raytheon.viz.lightning;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.productbrowser.AbstractRequestableProductBrowserDataDefinition;
import com.raytheon.uf.viz.productbrowser.ProductBrowserLabel;
import com.raytheon.uf.viz.productbrowser.ProductBrowserPreference;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 12, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class LightningProductBrowserDataDefinition extends
        AbstractRequestableProductBrowserDataDefinition<LightningResourceData> {

    private static final int[] offsets = new int[] { 3600, 900, 300, 60 };

    private static final String[] types = new String[] { "Positive",
            "Negative", "Positive/Negative" };

    private static int positiveOffset = 0;

    private static int negativeOffset = 0;

    private static String lightningType = "";

    public LightningProductBrowserDataDefinition() {
        productName = "binlightning";
        displayName = "Lightning";
        order = new String[] { "pluginName", "startTime", "type" };
        order = getOrder();
        loadProperties = new LoadProperties();
        loadProperties.setResourceType(getResourceType());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.productbrowser.AbstractProductBrowserDataDefinition
     * #populateData(java.lang.String[])
     */
    @Override
    public List<ProductBrowserLabel> populateData(String[] selection) {
        if (selection.length == 1) {
            String[] strings = new String[offsets.length];
            for (int i = 0; i < offsets.length; i++) {
                strings[i] = String.valueOf(offsets[i]);
            }
            return formatData("pluginName", strings);
        } else if (selection.length == 2) {
            return formatData("startTime", types);
        } else {
            return formatData("type", types);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.productbrowser.
     * AbstractRequestableProductBrowserDataDefinition
     * #formatData(java.lang.String, java.lang.String[])
     */
    @Override
    public List<ProductBrowserLabel> formatData(String param,
            String[] parameters) {
        if ("pluginName".equals(param)) {
            List<ProductBrowserLabel> labels = new ArrayList<ProductBrowserLabel>();
            for (int i = 0; i < offsets.length; i++) {
                labels.add(new ProductBrowserLabel((offsets[i] / 60) + " min",
                        "" + offsets[i]));
                labels.get(i).setProduct(false);
            }
            return labels;
        } else if ("startTime".equals(param)) {
            List<ProductBrowserLabel> labels = new ArrayList<ProductBrowserLabel>();
            for (int i = 0; i < types.length; i++) {
                labels.add(new ProductBrowserLabel(types[i], types[i]));
                labels.get(i).setProduct(true);
            }
            return labels;
        } else {
            return null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.productbrowser.AbstractProductBrowserDataDefinition
     * #getResourceData()
     */
    @Override
    public LightningResourceData getResourceData() {
        resourceData = new LightningResourceData();
        resourceData.setHandlingNegativeStrikes(("Negative"
                .equals(lightningType) ? true : false)
                || ("Positive/Negative".equals(lightningType) ? true : false));
        resourceData.setHandlingPositiveStrikes(("Positive"
                .equals(lightningType) ? true : false)
                || ("Positive/Negative".equals(lightningType) ? true : false));
        (resourceData).setBinOffset(new BinOffset(positiveOffset,
                negativeOffset));
        return resourceData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.productbrowser.AbstractProductBrowserDataDefinition
     * #constructResource(java.lang.String[],
     * com.raytheon.uf.viz.core.rsc.ResourceType)
     */
    @Override
    public void constructResource(String[] selection, ResourceType type) {
        String[] sel = new String[] { selection[0] };
        negativeOffset = Integer.parseInt(selection[1]);
        lightningType = selection[selection.length - 1];
        super.constructResource(sel, type);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.productbrowser.xml.IProductBrowserPreferences#
     * configurePreferences()
     */
    @Override
    public List<ProductBrowserPreference> configurePreferences() {
        return super.configurePreferences();
    }
}
