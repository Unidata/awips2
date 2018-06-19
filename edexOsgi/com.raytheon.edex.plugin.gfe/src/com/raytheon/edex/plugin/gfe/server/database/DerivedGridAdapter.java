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
package com.raytheon.edex.plugin.gfe.server.database;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.derivparam.CommonGridInventory;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.derivparam.inv.AvailabilityContainer;
import com.raytheon.uf.common.derivparam.inv.MetadataContainer;
import com.raytheon.uf.common.derivparam.library.DerivedParameterGenerator;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * 
 * Provides methods for the {@link D2DGridDatabase} to interact with the
 * {@link CommonGridInventory} to retrieve derived products.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Mar 21, 2016  5439     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class DerivedGridAdapter {

    private static CommonGridInventory inventory;

    public DerivedGridAdapter() {

    }

    private Map<String, RequestConstraint> buildRequestConstraints(
            String d2dModelName, Date refTime, String d2dParmName,
            Level d2dLevel) {
        Map<String, RequestConstraint> query = new HashMap<>();
        query.put(GridConstants.DATASET_ID, new RequestConstraint(d2dModelName));
        query.put(GridConstants.PARAMETER_ABBREVIATION, new RequestConstraint(
                d2dParmName));
        query.put(GridConstants.MASTER_LEVEL_NAME, new RequestConstraint(
                d2dLevel.getMasterLevel().getName()));
        query.put(GridConstants.LEVEL_ONE,
                new RequestConstraint(d2dLevel.getLevelOneValueAsString()));
        query.put(GridConstants.LEVEL_TWO,
                new RequestConstraint(d2dLevel.getLevelTwoValueAsString()));
        query.put(PluginDataObject.REFTIME_ID,
                new RequestConstraint(TimeUtil.formatToSqlTimestamp(refTime)));
        return query;
    }

    public List<DataTime> queryDataTimeByParmId(String d2dModelName,
            Date refTime, String d2dParmName, Level d2dLevel)
            throws DataCubeException, InterruptedException {
        List<DataTime> dbInv = new ArrayList<>();
        Map<String, RequestConstraint> query = buildRequestConstraints(
                d2dModelName, refTime, d2dParmName, d2dLevel);

        List<AbstractRequestableNode> nodes = getInventory()
                .evaluateRequestConstraints(query);
        AvailabilityContainer container = new AvailabilityContainer(query);
        for (AbstractRequestableNode node : nodes) {
            container.prepareRequests(node);
        }
        for (AbstractRequestableNode requestNode : nodes) {
            Set<TimeAndSpace> availability = container
                    .getAvailability(requestNode);
            for (TimeAndSpace ast : availability) {
                if (!ast.isTimeAgnostic()) {
                    dbInv.add(ast.getTime());
                }
            }
        }
        return dbInv;

    }

    public GridRecord getGrid(String d2dModelName, Date refTime,
            String d2dParmName, Level d2dLevel, Integer forecastTime)
            throws InterruptedException, DataCubeException {
        Map<String, RequestConstraint> query = buildRequestConstraints(
                d2dModelName, refTime, d2dParmName, d2dLevel);
        /*
         * This list is expected to only have a single item since the
         * constraints should be specific eno0ugh to limit it to one node.
         */
        List<AbstractRequestableNode> nodes = getInventory()
                .evaluateRequestConstraints(query);
        MetadataContainer container = new MetadataContainer(query,
                new AvailabilityContainer(query));
        List<AbstractRequestableData> requesters = new ArrayList<>();
        Set<TimeAndSpace> availability = new HashSet<>();
        availability.add(new TimeAndSpace(new DataTime(refTime, forecastTime)));
        for (AbstractRequestableNode node : nodes) {
            container.prepareRequests(node, availability);
        }
        for (AbstractRequestableNode node : nodes) {
            requesters.addAll(container.getData(node, availability));
        }
        /*
         * There is only expected to be one requester, since there should be one
         * node and one time. The only exception would be if a model has
         * multiple coverages, however GFE cannot handle that case so return
         * data only for the first requester.
         */
        for (AbstractRequestableData requester : requesters) {
            GridRecord d2dRecord = new GridRecord();
            d2dRecord.setDatasetId(requester.getSource());
            d2dRecord.setDataTime(requester.getDataTime());
            d2dRecord.setLevel(requester.getLevel());
            Parameter parameter = new Parameter();
            parameter.setAbbreviation(requester.getParameter());
            parameter.setName(requester.getParameterName());
            parameter.setUnit(requester.getUnit());
            d2dRecord.setParameter(parameter);
            d2dRecord.setLocation((GridCoverage) requester.getSpace());
            d2dRecord.setMessageData(requester);
            return d2dRecord;
        }
        return null;
    }

    public IDataRecord retrieve(GridRecord d2dRecord, Request request)
            throws DataCubeException {
        Object messageData = d2dRecord.getMessageData();
        if (messageData instanceof AbstractRequestableData) {
            AbstractRequestableData requester = (AbstractRequestableData) messageData;
            Object val = requester.getDataValue(request);
            if (val instanceof IDataRecord) {
                return (IDataRecord) val;
            } else if (val instanceof IDataRecord[]) {
                return ((IDataRecord[]) val)[0];
            }
        }
        return null;

    }

    private static synchronized CommonGridInventory getInventory()
            throws DataCubeException {
        if (inventory == null) {
            CommonGridInventory inventory = new CommonGridInventory();
            inventory.initTree(DerivedParameterGenerator.getDerParLibrary());
            DerivedGridAdapter.inventory = inventory;
        }
        return inventory;
    }
}
