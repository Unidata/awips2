package com.raytheon.viz.satellite;

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

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.datalisting.impl.DefaultDataListing;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.productbrowser.datalisting.DataListingProductBrowserDefinition;
import com.raytheon.viz.satellite.rsc.SatResourceData;

/**
 * Product browser implementation for satellite
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * May 03, 2010           mnash     Initial creation
 * Jun 04, 2015  4153     bsteffen  Switch to use a datalisting.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
public class SatelliteProductBrowserDataDefinition extends DataListingProductBrowserDefinition {

    public SatelliteProductBrowserDataDefinition() {
        super(new DefaultDataListing("satellite", Arrays.asList("creatingEntity", "sectorID", "physicalElement")));
    }

    @Override
    protected AbstractResourceData createResourceData(Map<String, String> keyVals) {
        SatResourceData resourceData = new SatResourceData();
        Map<String, RequestConstraint> constraints = listing.getRequestConstraints(keyVals);
        resourceData.setMetadataMap(new HashMap<>(constraints));
        return resourceData;
    }

}
