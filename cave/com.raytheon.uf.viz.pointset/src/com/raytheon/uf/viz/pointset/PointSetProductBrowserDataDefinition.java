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
package com.raytheon.uf.viz.pointset;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.pointset.PointSetConstants;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.pointset.rsc.PointSetResourceData;
import com.raytheon.uf.viz.productbrowser.datalisting.DataListingProductBrowserDefinition;

/**
 * 
 * Uses the {@link PointSetDataListing} to get metadata for a
 * {@link PointSetResourceData}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 28, 2015  4709     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class PointSetProductBrowserDataDefinition extends DataListingProductBrowserDefinition {

    public PointSetProductBrowserDataDefinition() {
        super(new PointSetDataListing(
                Arrays.asList(PointSetConstants.DATASET_ID,
                        PointSetConstants.PARAMETER_ABBREVIATION,
                        PointSetConstants.MASTER_LEVEL_NAME,
                        PointSetConstants.LEVEL_ID)));
    }

    @Override
    protected AbstractResourceData createResourceData(Map<String, String> keyVals) {
        PointSetResourceData resourceData = new PointSetResourceData();
        Map<String, RequestConstraint> constraints = listing.getRequestConstraints(keyVals);
        resourceData.setMetadataMap(new HashMap<>(constraints));
        return resourceData;
    }

}
