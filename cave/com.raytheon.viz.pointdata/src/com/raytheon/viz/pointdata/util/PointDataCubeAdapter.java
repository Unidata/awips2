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
import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequestSet;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Loader;
import com.raytheon.uf.viz.core.datastructure.CubeUtil;
import com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter;
import com.raytheon.uf.viz.core.datastructure.VizDataCubeException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.level.LevelMappingFactory;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.inv.AvailabilityContainer;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterGenerator;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode;
import com.raytheon.viz.pointdata.PointDataRequest;

/**
 * This adapter allows a user to request derived parameters from point data
 * sets. It is important to note that derived parameters for point data is much
 * different than grid. The primary difference is that while grid is combining
 * multiple records that represent different parameters, point data is combining
 * multiple paramters within a single record. As a result point data does not
 * use the time and space matching functions of derived parameters since they
 * are guaranteed to match for all parameters within a record.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 1, 2009             brockwoo    Initial creation
 * Nov 21, 2009 #3576      rjpeter     Refactored use of DerivParamDesc.
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */

public class PointDataCubeAdapter implements IDataCubeAdapter {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PointDataCubeAdapter.class);

    public static String PLUGIN_NAME = PointDataInventory.PLUGIN_NAME;

    private static String[] supportedPlugins = { "obs", "modelsounding",
            "bufrssmi", "bufrquikscat", "lsr", "sfcobs", "goessounding",
            "bufrascat", "poessounding", "profiler", "bufrua", "ldadmesonet",
            "ldadhydro", "qc", "fssobs", "bufrmosAVN", "bufrmosETA",
            "bufrmosGFS", "bufrmosHPC", "bufrmosLAMP", "bufrmosMRF",
            "bufrmosNGM" };

    protected AbstractPointDataInventory inventory;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getData(com.raytheon
     * .uf.viz.core.catalog.LayerProperty, java.util.Map)
     */
    @Override
    public List<Object> getData(LayerProperty property, int timeOut)
            throws VizException {
        String scriptToExecute = ScriptCreator.createScript(property);
        return Loader.loadScripts(new String[] { scriptToExecute }, timeOut);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getInventory()
     */
    @Override
    public Object getInventory() {
        if (inventory == null) {
            initInventory();
        }
        return this.inventory;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getPoints(java
     * .lang.String[], java.util.Map)
     */
    @Override
    public PointDataContainer getPoints(String plugin, String[] parameters,
            Map<String, RequestConstraint> queryParams) throws VizException {
        return getPoints(plugin, parameters, "Station", queryParams);
    }

    @Override
    public PointDataContainer getPoints(String plugin, String[] parameters,
            String levelKey, Map<String, RequestConstraint> queryParams)
            throws VizException {
        queryParams.put(PLUGIN_NAME, new RequestConstraint(plugin));
        String type = getType(queryParams);
        String source = plugin;
        if (!type.equals(plugin)) {
            source += type;
        }

        List<Level> levels = LevelMappingFactory.getInstance()
                .getLevelMappingForKey(levelKey).getLevels();
        List<AbstractRequestableNode> nodes = inventory.getNodes(source,
                Arrays.asList(parameters), levels);
        PointMetadataContainer pmc = new PointMetadataContainer(queryParams,
                Arrays.asList(parameters), this);
        for (AbstractRequestableNode node : nodes) {
            pmc.prepareRequests(node, AvailabilityContainer.AGNOSTIC_SET);
        }
        List<AbstractRequestableData> requests = new ArrayList<AbstractRequestableData>();

        for (AbstractRequestableNode node : nodes) {
            requests.addAll(pmc.getData(node,
                    AvailabilityContainer.AGNOSTIC_SET));
        }
        PointDataContainer pdc = pmc.getContainer();
        if (pdc == null) {
            return null;
        }
        for (AbstractRequestableData request : requests) {
            String unit = request.getUnit() == null ? null : request.getUnit()
                    .toString();
            Object obj = request.getDataValue(null);
            List<IDataRecord> recs = null;
            if (obj instanceof IDataRecord) {
                recs = Arrays.asList((IDataRecord) obj);
            } else if (obj instanceof IDataRecord[]) {
                recs = Arrays.asList((IDataRecord[]) obj);
            } else if (obj instanceof Number) {
                IDataRecord baseRec = pdc.getParameterRecord("id");
                long[] sizes = baseRec.getSizes();
                int length = 1;
                for (long size : sizes) {
                    length *= size;
                }
                float[] data = new float[length];
                Arrays.fill(data, ((Number) obj).floatValue());
                recs = Arrays.asList((IDataRecord) new FloatDataRecord(request
                        .getParameter(), null, data, baseRec.getDimension(),
                        sizes));
            } else if (obj == null) {
                throw new VizException("Invalid Object of type: null");
            } else {
                throw new VizException("Invalid Object of type: "
                        + obj.getClass().getSimpleName());
            }
            int resultCount = 0;
            for (IDataRecord rec : recs) {
                String resultPosition = (resultCount >= 1) ? "[" + resultCount
                        + "]" : "";
                rec.setName(rec.getName() + resultPosition);
                resultCount++;
                pdc.add(rec, unit);
            }
        }
        return pdc;
    }

    /**
     * @param queryParams
     * @return
     * @throws VizException
     */
    protected String getType(Map<String, RequestConstraint> queryParams)
            throws VizException {
        String plugin = queryParams.get(PLUGIN_NAME).getConstraintValue();
        String type = plugin;
        String typeKey = inventory.getTypeKey(plugin);
        if (queryParams.containsKey(typeKey)) {
            type = queryParams.get(typeKey).getConstraintValue();
        } else if (queryParams.containsKey("dataURI")) {
            String dataURI = queryParams.get("dataURI").getConstraintValue()
                    .split(",")[0];
            Map<String, Object> paramMap = RecordFactory.getInstance()
                    .loadMapFromUri(dataURI);
            type = paramMap.get(typeKey).toString();
        }
        return type;
    }

    public PointDataContainer getBaseRecords(Collection<String> baseParams,
            Map<String, RequestConstraint> queryParams) throws VizException {
        String plugin = queryParams.get(PLUGIN_NAME).getConstraintValue();
        return PointDataRequest.requestPointDataAllLevels(null, plugin,
                baseParams.toArray(new String[] {}), null, queryParams);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getRecord(com
     * .raytheon.uf.common.dataplugin.PluginDataObject)
     */
    @Override
    public IDataRecord[] getRecord(PluginDataObject obj)
            throws VizDataCubeException {
        if (obj.getMessageData() == null) {
            IDataRecord record = null;
            try {
                record = CubeUtil.retrieveData(obj, obj.getPluginName());
            } catch (VizException e) {
                throw new VizDataCubeException(
                        "Error retrieving point data record.", e);
            }

            return new IDataRecord[] { record };
        }

        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getSupportedPlugin
     * ()
     */
    @Override
    public String[] getSupportedPlugins() {
        return supportedPlugins;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#initInventory
     * (java.util.Map)
     */
    @Override
    public void initInventory() {
        if (inventory == null) {
            PointDataInventory pointInventory = new PointDataInventory(
                    Arrays.asList(supportedPlugins));
            try {
                pointInventory.initTree(DerivedParameterGenerator
                        .getDerParLibrary());
                this.inventory = pointInventory;
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    @Override
    public IDataRecord[] getRecord(PluginDataObject obj, Request req,
            String dataset) throws VizDataCubeException {
        if (obj.getMessageData() == null) {
            IDataRecord record = null;
            try {
                record = CubeUtil.retrieveData(obj, obj.getPluginName(), req,
                        dataset);
            } catch (VizException e) {
                throw new VizDataCubeException(
                        "Error retrieving point data record.", e);
            }

            return new IDataRecord[] { record };
        }

        return null;
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
        for (PluginDataObject obj : objs) {
            IDataRecord[] records = getRecord(obj, req, dataset);
            obj.setMessageData(records);
        }
    }

    @Override
    public List<Map<String, RequestConstraint>> getBaseUpdateConstraints(
            Map<String, RequestConstraint> constraints) {
        List<Map<String, RequestConstraint>> result = new ArrayList<Map<String, RequestConstraint>>(
                1);
        result.add(constraints);
        return result;
    }

    @Override
    public List<List<DataTime>> timeQuery(List<TimeQueryRequest> requests)
            throws VizException {
        TimeQueryRequestSet set = new TimeQueryRequestSet();
        set.setRequests(requests.toArray(new TimeQueryRequest[0]));

        @SuppressWarnings("unchecked")
        List<List<DataTime>> result = (List<List<DataTime>>) ThriftClient
                .sendRequest(set);
        return result;
    }
}
