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
        order = new String[] { "startTime", "type" };
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
        List<ProductBrowserLabel> labels = null;
        if (order[selection.length - 1].equals("startTime")) {
            String[] strings = new String[offsets.length];
            for (int i = 0; i < offsets.length; i++) {
                strings[i] = String.valueOf(offsets[i]);
            }

            labels = formatData("startTime", strings);
        } else if (order[selection.length - 1].equals("type")) {
            labels = formatData("type", types);
        }

        for (ProductBrowserLabel label : labels) {
            if (selection.length == order.length) {
                label.setProduct(true);
            } else {
                label.setProduct(false);
            }
        }
        return labels;
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
        if ("startTime".equals(param)) {
            List<ProductBrowserLabel> labels = new ArrayList<ProductBrowserLabel>();
            for (int i = 0; i < offsets.length; i++) {
                labels.add(new ProductBrowserLabel((offsets[i] / 60) + " min",
                        "" + offsets[i]));
            }
            return labels;
        } else if ("type".equals(param)) {
            List<ProductBrowserLabel> labels = new ArrayList<ProductBrowserLabel>();
            for (int i = 0; i < types.length; i++) {
                labels.add(new ProductBrowserLabel(types[i], types[i]));
            }
            return labels;
        }
        return null;
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
        int timeOffset = 0;
        int posNegOffset = 0;
        for (int i = 0; i < order.length; i++) {
            if ("startTime".equals(order[i])) {
                timeOffset = i;
            } else if ("type".equals(order[i])) {
                posNegOffset = i;
            }
        }
        negativeOffset = Integer.parseInt(selection[timeOffset + 1]);
        lightningType = selection[posNegOffset + 1];
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
