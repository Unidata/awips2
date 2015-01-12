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
package com.raytheon.viz.satellite.inventory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.derivparam.library.DerivedParameterGenerator;
import com.raytheon.uf.common.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode.Dependency;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.datacube.AbstractDataCubeAdapter;

/**
 * DataCubeAdapter for satellite
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 09, 2014  2947     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class SatelliteDataCubeAdapter extends AbstractDataCubeAdapter {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SatelliteDataCubeAdapter.class);

    /**
     * @deprecated This used to be a required key in derived request constraints
     *             but is no longer necessary
     */
    @Deprecated
    public static final String DERIVED = "DERIVED";

    private SatelliteInventory inventory;

    public SatelliteDataCubeAdapter() {
        super(new String[] { SatelliteInventory.SATELLITE });
    }

    @Override
    public void initInventory() {
        if (inventory == null) {
            try {
                SatelliteInventory inventory = new SatelliteInventory();
                inventory
                        .initTree(DerivedParameterGenerator.getDerParLibrary());
                this.inventory = inventory;
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
        }
    }

    @Override
    public Object getInventory() {
        return inventory;
    }

    @Override
    protected List<AbstractRequestableNode> evaluateRequestConstraints(
            Map<String, RequestConstraint> constraints) {
        return inventory.evaluateRequestConstraints(constraints);
    }

    @Override
    protected List<DataTime> timeAgnosticQuery(
            Map<String, RequestConstraint> queryTerms) throws DataCubeException {
        return inventory.timeAgnosticQuery(queryTerms);
    }

    @Override
    protected List<PluginDataObject> getData(
            Map<String, RequestConstraint> constraints,
            DataTime[] selectedTimes, List<AbstractRequestableData> requesters)
            throws DataCubeException {
        List<PluginDataObject> results = new ArrayList<PluginDataObject>(
                requesters.size());
        for (AbstractRequestableData requester : requesters) {
            if(requester instanceof SatelliteRequestableData){
                results.add(((SatelliteRequestableData) requester).getRecord());
            } else {
                results.add(new DerivedSatelliteRecord(requester));
            }
        }
        return results;
    }

    @Override
    public void getRecords(List<PluginDataObject> objs, Request req,
            String dataset) throws DataCubeException {
        List<PluginDataObject> simpleRecords = new ArrayList<PluginDataObject>(
                objs.size());
        for (PluginDataObject obj : objs) {
            if (obj instanceof DerivedSatelliteRecord) {
                ((DerivedSatelliteRecord) obj).deriveMessageData(req, dataset);
            } else if (obj instanceof SatelliteRecord) {
                simpleRecords.add(obj);
            } else if (obj != null) {
                throw new DataCubeException(this.getClass().getSimpleName()
                        + " cannot get records for a "
                        + obj.getClass().getSimpleName());
            }
        }
        if (!simpleRecords.isEmpty()) {
            super.getRecords(simpleRecords, req, dataset);
        }
    }

    @Override
    public List<Map<String, RequestConstraint>> getBaseUpdateConstraints(
            Map<String, RequestConstraint> constraints) {
        List<AbstractRequestableNode> nodes = evaluateRequestConstraints(constraints);
        List<Map<String, RequestConstraint>> baseConstraints = new ArrayList<Map<String, RequestConstraint>>(
                nodes.size() * 2);
        for (AbstractRequestableNode node : nodes) {
            baseConstraints.addAll(getBaseUpdateConstraints(node));
        }
        return baseConstraints;
    }

    protected List<Map<String, RequestConstraint>> getBaseUpdateConstraints(
            AbstractRequestableNode node) {
        if (node instanceof SatelliteRequestableLevelNode) {
            SatelliteRequestableLevelNode satNode = (SatelliteRequestableLevelNode) node;
            return Collections.singletonList(satNode.getRequestConstraintMap());
        } else if (node instanceof AbstractDerivedDataNode) {
            AbstractDerivedDataNode derivedNode = (AbstractDerivedDataNode) node;
            List<Map<String, RequestConstraint>> baseConstraints = new ArrayList<Map<String, RequestConstraint>>();
            for (Dependency d : derivedNode.getDependencies()) {
                baseConstraints.addAll(getBaseUpdateConstraints(d.node));
            }
            return baseConstraints;
        }
        return Collections.emptyList();
    }



}
