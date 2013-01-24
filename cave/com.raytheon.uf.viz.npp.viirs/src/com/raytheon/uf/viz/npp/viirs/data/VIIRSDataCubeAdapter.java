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
package com.raytheon.uf.viz.npp.viirs.data;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSDataRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.datastructure.CubeUtil;
import com.raytheon.uf.viz.core.datastructure.VizDataCubeException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractDataCubeAdapter;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode.Dependency;
import com.raytheon.uf.viz.npp.viirs.Activator;
import com.raytheon.uf.viz.npp.viirs.data.VIIRSRequestableData.VIIRSRequest;

/**
 * DataCubeAdapter for viirs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSDataCubeAdapter extends AbstractDataCubeAdapter {

    private VIIRSDataInventory inventory;

    public VIIRSDataCubeAdapter() {
        super(new String[] { VIIRSDataInventory.PLUGIN_NAME });
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.DefaultDataCubeAdapter#initInventory
     * ()
     */
    @Override
    public void initInventory() {
        if (inventory == null) {
            try {
                VIIRSDataInventory inventory = new VIIRSDataInventory();
                inventory.initTree(new HashMap<String, DerivParamDesc>());
                this.inventory = inventory;
            } catch (Exception e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.DefaultDataCubeAdapter#getInventory
     * ()
     */
    @Override
    public Object getInventory() {
        return inventory;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.data.AbstractDataCubeAdapter#getRecord
     * (com.raytheon.uf.common.dataplugin.PluginDataObject,
     * com.raytheon.uf.common.datastorage.Request, java.lang.String)
     */
    @Override
    public IDataRecord[] getRecord(PluginDataObject pdo, Request req,
            String dataset) throws VizDataCubeException {
        if (dataset == null) {
            dataset = VIIRSDataRecord.getDataSet(0);
        }
        try {
            IDataRecord[] dataRecords = null;
            if (pdo instanceof VIIRSRequestableDataRecord) {
                VIIRSRequestableDataRecord vrdr = (VIIRSRequestableDataRecord) pdo;
                // Put VIIRSSpatialCoverage from pdo in VIIRSRequest and make
                // sure data records out of getData are in same coverage
                VIIRSRequest request = new VIIRSRequest(req, dataset,
                        vrdr.getCoverage());
                AbstractRequestableData requestable = vrdr.getRequestableData();
                if (requestable instanceof VIIRSRequestableData) {
                    dataRecords = ((VIIRSRequestableData) requestable)
                            .getRawDataValue(request);
                } else {
                    dataRecords = (IDataRecord[]) requestable
                            .getDataValue(request);
                }
            } else if (pdo instanceof VIIRSDataRecord) {
                VIIRSDataRecord vdr = (VIIRSDataRecord) pdo;
                VIIRSRequestableData requester = new VIIRSRequestableData(vdr,
                        inventory.getParameterLevel(vdr.getParameter()));
                dataRecords = getRecord(new VIIRSRequestableDataRecord(
                        requester, Arrays.asList(vdr)), req, dataset);
            } else {
                dataRecords = new IDataRecord[] { CubeUtil.retrieveData(pdo,
                        pdo.getPluginName(), req, dataset) };
            }
            return dataRecords;
        } catch (VizException e) {
            throw new VizDataCubeException("Error requesting viirs data: "
                    + e.getLocalizedMessage(), e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getRecords(java
     * .util.List, com.raytheon.uf.common.datastorage.Request, java.lang.String)
     */
    @Override
    public void getRecords(List<PluginDataObject> objs, Request req,
            String dataset) throws VizDataCubeException {
        // TODO: Need more advanced synchronizing for derived parameters
        for (PluginDataObject pdo : objs) {
            pdo.setMessageData(getRecord(pdo, req, dataset));
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.derivparam.data.AbstractDataCubeAdapter#
     * evaluateRequestConstraints(java.util.Map)
     */
    @Override
    protected List<AbstractRequestableNode> evaluateRequestConstraints(
            Map<String, RequestConstraint> constraints) {
        return inventory.evaluateRequestConstraints(constraints);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.data.AbstractDataCubeAdapter#timeAgnosticQuery
     * (java.util.Map)
     */
    @Override
    protected List<DataTime> timeAgnosticQuery(
            Map<String, RequestConstraint> queryTerms) throws VizException {
        return inventory.timeAgnosticQuery(queryTerms);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.data.AbstractDataCubeAdapter#getData(com
     * .raytheon.uf.viz.core.catalog.LayerProperty, java.util.List)
     */
    @Override
    protected List<Object> getData(LayerProperty property,
            List<AbstractRequestableData> requesters) throws VizException {
        List<Object> results = new ArrayList<Object>(requesters.size());
        for (AbstractRequestableData requester : requesters) {
            List<VIIRSDataRecord> baseRecords = new ArrayList<VIIRSDataRecord>();
            findBaseRecords(requester, baseRecords);
            results.add(new VIIRSRequestableDataRecord(requester, baseRecords));
        }
        return results;
    }

    /**
     * Recursively searches dependencies for VIIRSRequestableData objects and
     * adds records from objects in baseRecords list
     * 
     * @param requester
     * @param baseRecords
     */
    private void findBaseRecords(AbstractRequestableData requester,
            List<VIIRSDataRecord> baseRecords) {
        if (requester instanceof VIIRSRequestableData) {
            baseRecords.add(((VIIRSRequestableData) requester).getDataRecord());
        }
        for (AbstractRequestableData ard : requester.getDependencies()) {
            findBaseRecords(ard, baseRecords);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.derivparam.data.AbstractDataCubeAdapter#
     * getBaseUpdateConstraints(java.util.Map)
     */
    @Override
    public List<Map<String, RequestConstraint>> getBaseUpdateConstraints(
            Map<String, RequestConstraint> constraints) {
        List<AbstractRequestableNode> nodes = evaluateRequestConstraints(constraints);
        List<Map<String, RequestConstraint>> baseConstraints = new ArrayList<Map<String, RequestConstraint>>();
        for (AbstractRequestableNode node : nodes) {
            getBaseUpdateConstraints(baseConstraints, node);
        }
        return baseConstraints;
    }

    private void getBaseUpdateConstraints(
            List<Map<String, RequestConstraint>> baseConstraints,
            AbstractRequestableNode node) {

        if (node instanceof VIIRSRequestableLevelNode) {
            VIIRSRequestableLevelNode viirsNode = (VIIRSRequestableLevelNode) node;
            baseConstraints.add(viirsNode.getRequestConstraintMap());
        } else if (node instanceof AbstractDerivedDataNode) {
            AbstractDerivedDataNode derivedNode = (AbstractDerivedDataNode) node;
            for (Dependency d : derivedNode.getDependencies()) {
                getBaseUpdateConstraints(baseConstraints, d.node);
            }
        } else if (!node.isConstant()) {
            // If everything is working correctly than this is dead code, but it is here just in case I missed something.
            Activator.statusHandler.handle(Priority.WARN, this.getClass()
                    .getSimpleName()
                    + " cannot determine base constraints for "
                    + node.getClass().getSimpleName());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getPoints(java
     * .lang.String, java.lang.String[], java.util.Map)
     */
    @Override
    public PointDataContainer getPoints(String plugin, String[] parameters,
            Map<String, RequestConstraint> queryParams) throws VizException {
        throw new UnsupportedOperationException(
                "getPoints is not supported by viirs");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getPoints(java
     * .lang.String, java.lang.String[], java.lang.String, java.util.Map)
     */
    @Override
    public PointDataContainer getPoints(String plugin, String[] parameters,
            String levelKey, Map<String, RequestConstraint> queryParams)
            throws VizException {
        throw new UnsupportedOperationException(
                "getPoints is not supported by viirs");
    }

}
