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

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.dataaccess.GFEDataAccessUtil;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.grid.rsc.GridLoadProperties;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.d2d.gfe.rsc.GFEGridResourceData;
import com.raytheon.uf.viz.productbrowser.ProductBrowserDataDefinition;
import com.raytheon.uf.viz.productbrowser.datalisting.DataListingProductBrowserDefinition;

/**
 * {@link ProductBrowserDataDefinition} for GFE data
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 24, 2018 6609       njensen     Initial creation
 *
 * </pre>
 *
 * @author njensen
 */

public class GFEProductBrowserDataDefinition
        extends DataListingProductBrowserDefinition {

    public GFEProductBrowserDataDefinition() {
        super("GFE", new GFEProductBrowserDataListing("gfe", Arrays.asList(
                GFEDataAccessUtil.SITE_ID, GFEDataAccessUtil.MODEL_NAME,
                GFEDataAccessUtil.PARM_NAME, GFEDataAccessUtil.PARM_LEVEL)));
    }

    @Override
    protected AbstractResourceData createResourceData(
            Map<String, String> keyVals) {
        // Copy so we can safely modify
        Map<String, String> keyValsCopy = new HashMap<>(keyVals);

        // add dbType into metadata map if model name is Fcst
        ((GFEProductBrowserDataListing) listing).checkForFcstDb(keyValsCopy);

        GFEGridResourceData rscData = new GFEGridResourceData();
        rscData.setMetadataMap(
                new HashMap<>(listing.getRequestConstraints(keyValsCopy)));
        return rscData;
    }

    @Override
    public Collection<DisplayType> getValidDisplayTypes(String[] selection) {
        Map<String, String> keyValMap = createKeyValMap(selection);
        return ((GFEProductBrowserDataListing) listing).getValidDisplayTypes(keyValMap);
    }

    @Override
    public void loadResource(String[] selection, DisplayType displayType) {
        /*
         * Code borrowed from GridProductBrowserDataDefinition. There's not a
         * good way to share code without introducing extraneous dependencies.
         */
        if (displayType == null) {
            Collection<DisplayType> types = getValidDisplayTypes(selection);
            if (types != null && !types.isEmpty()) {
                displayType = types.iterator().next();
            }
        }
        super.loadResource(selection, displayType);
    }

    @Override
    protected ResourcePair createResourcePair(AbstractResourceData resourceData,
            DisplayType displayType) {
        /*
         * Code borrowed from GridProductBrowserDataDefinition. There's not a
         * good way to share code without introducing extraneous dependencies.
         */
        ResourcePair pair = new ResourcePair();
        pair.setResourceData(resourceData);
        GridLoadProperties loadProperties = new GridLoadProperties();
        if (displayType != null) {
            loadProperties.setDisplayType(displayType);
        }
        pair.setLoadProperties(loadProperties);
        pair.setProperties(new ResourceProperties());
        return pair;
    }

}
