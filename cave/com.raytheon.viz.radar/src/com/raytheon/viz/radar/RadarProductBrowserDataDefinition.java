package com.raytheon.viz.radar;

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

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.radar.util.RadarInfoDict;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.productbrowser.datalisting.DataListingProductBrowserDefinition;
import com.raytheon.viz.radar.rsc.RadarResourceData;
import com.raytheon.viz.radar.ui.xy.RadarGraphDisplay;
import com.raytheon.viz.radar.ui.xy.RadarXYDisplay;

/**
 * Product browser implementation for radar
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * May 03, 2010           mnash     Initial creation
 * Jun 30, 2010           mnash     Used ProductBrowserLabel instead of String[]
 * Jun 09, 2015  4153     bsteffen  Switch to use a datalisting.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarProductBrowserDataDefinition extends DataListingProductBrowserDefinition {

    private final RadarInfoDict infoDict;

    public RadarProductBrowserDataDefinition() {
        super(new RadarDataListing(Arrays.asList("icao", "productCode", "primaryElevationAngle")));
        File radarInfo = PathManagerFactory.getPathManager().getStaticFile("radarInfo.txt");
        if (radarInfo != null) {
            infoDict = RadarInfoDict.getInstance(radarInfo.getParent());
        } else {
            infoDict = null;
        }
    }

    @Override
    protected RadarResourceData createResourceData(Map<String, String> keyVals) {
        RadarResourceData resourceData = new RadarResourceData();
        Map<String, RequestConstraint> constraints = listing.getRequestConstraints(keyVals);
        resourceData.setMetadataMap(new HashMap<>(constraints));
        return resourceData;
    }

    @Override
    protected AbstractRenderableDisplay createRenderableDisplay(ResourcePair resourcePair) throws VizException {
        AbstractResourceData resourceData = resourcePair.getResourceData();
        if (resourceData instanceof RadarResourceData) {
            int prodCode = Integer.parseInt(((RadarResourceData) resourceData).getMetadataMap().get("productCode")
                    .getConstraintValue());
            if (infoDict != null) {
                String format = infoDict.getInfo(prodCode).getFormat();
                if ("XY".equals(format)) {
                    RadarXYDisplay display = new RadarXYDisplay();
                    display.getDescriptor().getResourceList().add(resourcePair);
                    return display;
                } else if ("Graph".equals(format)) {
                    RadarGraphDisplay display = new RadarGraphDisplay();
                    display.getDescriptor().getResourceList().add(resourcePair);
                    return display;
                }
            }
        }
        return super.createRenderableDisplay(resourcePair);
    }

}
