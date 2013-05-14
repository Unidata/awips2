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
package com.raytheon.viz.pointdata.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.pointdata.PointDataConstants;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.inv.AvailabilityContainer;
import com.raytheon.uf.viz.derivparam.inv.MetadataContainer;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode;

/**
 * A MetadataContainer that is optimized for point data. This container ensures
 * that the point data api is used properly to bulk request all the base
 * parameters at once.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2012            bsteffen    Initial creation
 * May 14, 2013 1869       bsteffen    Get plots working without dataURI
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class PointMetadataContainer extends MetadataContainer {

    /**
     * Parameters which should be requested for all levels.
     */
    private static final List<String> constantParameters = Arrays.asList("id",
            "latitude", "longitude", "dataURI",
            PointDataConstants.DATASET_STATIONID,
            PointDataConstants.DATASET_REFTIME,
            PointDataConstants.DATASET_FORECASTHR);

    private final PointDataCubeAdapter pdca;

    private final List<String> requestedParameters;

    private PointDataContainer pdc;

    public PointMetadataContainer(
            Map<String, RequestConstraint> originalConstraints,
            List<String> requestedParameters, PointDataCubeAdapter pdca) {
        super(originalConstraints, new AvailabilityContainer(
                originalConstraints));
        this.requestedParameters = requestedParameters;
        this.pdca = pdca;
    }

    public PointDataContainer getContainer() {
        return pdc;
    }

    /**
     * For point data need the full data record instead of just a simple db
     * request, this method handles requesting all the parameters using the
     * point data API at one time.
     */
    @Override
    protected void processRequests() throws VizException {
        List<PointDataLevelNode> nodes = new ArrayList<PointDataLevelNode>();
        List<String> baseParams = new ArrayList<String>();
        for (AbstractRequestableNode node : availCache.keySet()) {
            if (dataCache.containsKey(node)) {
                continue;
            }
            if (node instanceof PointDataLevelNode) {
                PointDataLevelNode dataNode = (PointDataLevelNode) node;
                nodes.add(dataNode);
                baseParams.add(dataNode.getParameter());
            }
        }
        if (baseParams.isEmpty()) {
            return;
        }
        for (String parameter : requestedParameters) {
            if (constantParameters.contains(parameter)
                    && !baseParams.contains(parameter)) {
                baseParams.add(parameter);
            }
        }

        pdc = pdca.getBaseRecords(baseParams, originalConstraints);
        for (PointDataLevelNode node : nodes) {
            Set<AbstractRequestableData> cacheSet = new HashSet<AbstractRequestableData>();
            if (pdc != null) {
                IDataRecord rec = pdc.getParameterRecord(node.getParameter());
                cacheSet.add(new PointRequestableData(rec, pdc.getDescription(
                        node.getParameter()).getUnitObject()));
                if (!constantParameters
                        .contains(rec.getName())) {
                    pdc.remove(rec.getName());
                }
            }
            dataCache.put(node, cacheSet);
        }
    }

}
