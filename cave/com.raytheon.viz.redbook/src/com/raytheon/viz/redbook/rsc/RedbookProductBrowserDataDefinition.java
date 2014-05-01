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
package com.raytheon.viz.redbook.rsc;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.productbrowser.AbstractRequestableProductBrowserDataDefinition;
import com.raytheon.uf.viz.productbrowser.ProductBrowserLabel;
import com.raytheon.uf.viz.productbrowser.ProductBrowserPreference;
import com.raytheon.uf.viz.productbrowser.ProductBrowserPreference.PreferenceType;
import com.raytheon.viz.redbook.RedbookWMOMap;
import com.raytheon.viz.redbookua.rsc.RedbookUpperAirResourceData;

/**
 * Product browser implementation for redbook
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 17, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RedbookProductBrowserDataDefinition extends
        AbstractRequestableProductBrowserDataDefinition<RedbookResourceData> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RedbookProductBrowserDataDefinition.class);

    private String prodType = "";

    private static RedbookWMOMap mapping = null;

    private static final String INCLUDE_UNNAMED_PRODUCTS = "Include Undefined Products";

    public RedbookProductBrowserDataDefinition() {
        productName = "redbook";
        displayName = "Redbook";
        order = new String[] { "wmoTTAAii" };
        order = getOrder();
        try {
            mapping = null;
            if (mapping == null) {
                mapping = RedbookWMOMap.load();
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        }
        loadProperties = new LoadProperties();
        loadProperties.setResourceType(getResourceType());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.productbrowser.AbstractProductBrowserDataDefinition#
     * formatData(java.lang.String, java.lang.String[])
     */
    @Override
    public List<ProductBrowserLabel> formatData(String param,
            String[] parameters) {
        List<ProductBrowserLabel> labels = new ArrayList<ProductBrowserLabel>();
        if ("wmoTTAAii".equals(param)) {
            for (int i = 0; i < parameters.length; i++) {
                RedbookWMOMap.Info info = mapping.mapping.get(parameters[i]);
                if (info != null) {
                    labels.add(new ProductBrowserLabel(info.name + " ("
                            + parameters[i] + ")", parameters[i]));
                } else {
                    if ((Boolean) getPreference(INCLUDE_UNNAMED_PRODUCTS)
                            .getValue()) {
                        labels.add(new ProductBrowserLabel(parameters[i], null));
                    }
                }
            }
            Collections.sort(labels);
        }
        return labels;
    }

    @Override
    public void constructResource(String[] selection, ResourceType type) {
        int count = 0;
        for (String temp : order) {
            if (temp.equals("wmoTTAAii")) {
                break;
            }
            count++;
        }
        if (selection[count].contains("PYMA")) {
            prodType = "UA";
        } else {
            prodType = "";
        }
        super.constructResource(selection, type);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.productbrowser.AbstractProductBrowserDataDefinition
     * #getResourceData()
     */
    @Override
    public RedbookResourceData getResourceData() {
        if (prodType.equals("UA")) {
            return new RedbookUpperAirResourceData();
        } else {
            return new RedbookResourceData();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.productbrowser.xml.IProductBrowserPreferences#
     * configurePreferences()
     */
    @Override
    public List<ProductBrowserPreference> configurePreferences() {
        List<ProductBrowserPreference> widgets = super.configurePreferences();
        ProductBrowserPreference includeUnnamed = new ProductBrowserPreference();
        includeUnnamed.setPreferenceType(PreferenceType.BOOLEAN);
        includeUnnamed.setLabel(INCLUDE_UNNAMED_PRODUCTS);
        includeUnnamed
                .setTooltip("Will remove preferences that have not had names defined in redbookMapping.xml");
        includeUnnamed.setValue(false);
        widgets.add(includeUnnamed);
        return widgets;
    }
}
