/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2013 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 2120 South 72nd Street
 * Omaha Tower, Suite 900
 * Omaha, NE 68124 USA
 * 402.291.0100
 *
 */
package com.raytheon.uf.viz.dataplugin.nswrc.productbrowser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.nswrc.NSWRCConstants;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.dataplugin.nswrc.NSWRCVizResourceData;
import com.raytheon.uf.viz.productbrowser.AbstractRequestableProductBrowserDataDefinition;

/**
 * Product browser NSWRC request able implementation
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2013            ekladstrup     Initial creation
 * Apr 22, 2014  3048      mweeks      Updates for peer review and 13.5.4 baseline.
 *
 * </pre>
 *
 * @author ekladstrup
 * @version 1.0
 */

public class NSWRCRadialProductBrowserDataDefinition extends
        AbstractRequestableProductBrowserDataDefinition<NSWRCVizResourceData> {

    public NSWRCRadialProductBrowserDataDefinition() {
        productName = NSWRCConstants.NSWRC_RADIAL;
        displayName = "NSWRC Radial";
        order = new String[] { "locationName", "productName", "angleIdentifier" };
        order = getOrder();
        loadProperties = new LoadProperties();
        loadProperties.getCapabilities().addCapability(ImagingCapability.class);
        loadProperties.setResourceType(getResourceType());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.productbrowser.
     * AbstractRequestableProductBrowserDataDefinition#getResourceData()
     */
    @Override
    public NSWRCVizResourceData getResourceData() {
        return new NSWRCVizResourceData();
    }

    @Override
    public Map<ResourceType, List<DisplayType>> getDisplayTypes() {
        Map<ResourceType, List<DisplayType>> type = new HashMap<ResourceType, List<DisplayType>>();
        List<DisplayType> types = new ArrayList<DisplayType>();
        types.add(DisplayType.IMAGE);
        type.put(ResourceType.PLAN_VIEW, types);
        return type;
    }

}
