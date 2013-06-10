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
package com.raytheon.uf.viz.d2d.gfe.browser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.dataaccess.GFEDataAccessUtil;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.d2d.gfe.rsc.GFEGridResourceData;
import com.raytheon.uf.viz.productbrowser.AbstractRequestableProductBrowserDataDefinition;
import com.raytheon.viz.grid.rsc.GridLoadProperties;

/**
 * 
 * Request GFE data from the product browser
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 9, 2011            bsteffen     Initial creation
 * May 02, 2013 1949       bsteffen    Update GFE data access in Product
 *                                     Browser, Volume Browser, and Data Access
 *                                     Framework.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GFEDataDefinition extends
        AbstractRequestableProductBrowserDataDefinition<GFEGridResourceData> {

    public GFEDataDefinition() {
        productName = "gfe";
        displayName = "GFE";
        order = new String[] { GFEDataAccessUtil.SITE_ID, GFEDataAccessUtil.MODEL_NAME,
                GFEDataAccessUtil.PARM_NAME, GFEDataAccessUtil.PARM_LEVEL };
        order = getOrder();
        loadProperties = new GridLoadProperties();
        loadProperties.setResourceType(getResourceType());
    }

    @Override
    public GFEGridResourceData getResourceData() {
        resourceData = new GFEGridResourceData();
        loadProperties = new GridLoadProperties(loadProperties
                .getCapabilities()
                .getCapability(resourceData, DisplayTypeCapability.class)
                .getDisplayType());
        loadProperties.setResourceType(getResourceType());
        return resourceData;
    }

    @Override
    public Map<ResourceType, List<DisplayType>> getDisplayTypes() {
        Map<ResourceType, List<DisplayType>> type = new HashMap<ResourceType, List<DisplayType>>();
        List<DisplayType> types = new ArrayList<DisplayType>();
        types.add(DisplayType.CONTOUR);
        types.add(DisplayType.IMAGE);
        type.put(ResourceType.PLAN_VIEW, types);
        return type;
    }

}
