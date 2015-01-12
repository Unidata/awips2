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
package com.raytheon.uf.viz.d2d.xy.adapters.timeseries;

import java.awt.Point;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.util.GridLevelTranslator;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.PointUtil;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.datacube.CubeUtil;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYDataList;
import com.raytheon.viz.core.graphing.xy.XYIconImageData;
import com.raytheon.viz.core.graphing.xy.XYWindImageData;

/**
 * Adapter providing a TimeSeries view of GridData.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 07, 2010           bsteffen    Initial creation
 * Feb 17, 2014  2661     bsteffen    Use only u,v for vectors.
 * Jun 18, 2014  3242     njensen     Overrode getEnsembleId()
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GridTimeSeriesAdapter extends
        AbstractTimeSeriesAdapter<GridRecord> {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridTimeSeriesAdapter.class);

    private Map<GridRecord, IDataRecord[]> cache = new WeakHashMap<GridRecord, IDataRecord[]>();

    protected Map<DataTime, Set<GridRecord>> recordsByTime = new HashMap<DataTime, Set<GridRecord>>();

    /** the level I would prefer to use **/
    protected Level preferredLevel = null;

    /** the unit to convert to, matches preferredLevel unit **/
    protected Unit<?> preferredUnit = Unit.ONE;

    /** a map of levels to units for quick lookup **/
    protected Map<Level, Unit<?>> levelUnitMap = new HashMap<Level, Unit<?>>();

    /** first received parameter name **/
    protected String parameterName = "";

    protected String parameterAbbreviation = "";

    protected String ensembleId;

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter#
     * getDataUnit()
     */
    @Override
    public Unit<?> getDataUnit() {
        return preferredUnit;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter#getLevel
     * ()
     */
    @Override
    public SingleLevel getLevel() {
        String name = preferredLevel.getMasterLevel().getName();
        SingleLevel level = GridLevelTranslator.construct(name);
        level.setValue(preferredLevel.getLevelonevalue());
        return level;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter#loadData
     * (com.raytheon.uf.viz.core.rsc.DisplayType)
     */
    @Override
    public XYDataList loadData() throws VizException {
        ArrayList<GridRecord> gribs = null;
        synchronized (recordsByTime) {
            // get available times
            Set<DataTime> times = recordsByTime.keySet();
            // set initial size of gribs
            gribs = new ArrayList<GridRecord>(times.size());
            // get the best record from each time ( use only one record for a
            // point in time )
            for (DataTime key : times) {
                Set<GridRecord> records = recordsByTime.get(key);
                GridRecord[] allRecords = records
                        .toArray(new GridRecord[records.size()]);
                GridRecord bestRecord = null;
                // if only one record, use it
                if (allRecords.length == 1) {
                    bestRecord = allRecords[0];
                } else if (records.size() > 1) {
                    // get the first record, or the one that matches the
                    // preferredLevel
                    for (GridRecord rec : allRecords) {
                        if (bestRecord == null) {
                            bestRecord = rec;
                        } else if (rec.getLevel().equals(preferredLevel)) {
                            bestRecord = rec;
                            break;
                        }
                    }
                }
                // add the best record to the arrayList if not null
                if (bestRecord != null) {
                    gribs.add(bestRecord);
                }
            }
        }
        HashMap<GridCoverage, List<GridRecord>> gridsByLocation = new HashMap<GridCoverage, List<GridRecord>>();
        for (GridRecord grid : gribs) {
            List<GridRecord> list = gridsByLocation.get(grid.getLocation());
            if (list == null) {
                list = new ArrayList<GridRecord>();
                gridsByLocation.put(grid.getLocation(), list);
            }
            list.add(grid);
        }
        XYDataList result = null;
        for (List<GridRecord> list : gridsByLocation.values()) {
            XYDataList newResult = loadInternal(list
                    .toArray(new GridRecord[list.size()]));
            if (result == null) {
                result = newResult;
            } else {
                result.getData().addAll(newResult.getData());
            }
        }
        return result;
    }

    @Override
    public String getParameterName() {
        return parameterName;
    }

    /**
     * load just one record instead of all the data
     */
    @Override
    public XYDataList loadRecord(PluginDataObject pdo) throws VizException {
        GridRecord[] gribs = new GridRecord[1];
        gribs[0] = (GridRecord) pdo;
        return loadInternal(gribs);
    }

    @Override
    public void addRecord(PluginDataObject pdo) {
        // store off records by level
        if (pdo instanceof GridRecord) {
            synchronized (recordsByTime) {
                GridRecord record = (GridRecord) pdo;

                // set preferredLevel to first level
                if (preferredLevel == null) {
                    preferredLevel = record.getLevel();
                    preferredUnit = record.getParameter().getUnit();
                    parameterName = record.getParameter().getName();
                    parameterAbbreviation = record.getParameter()
                            .getAbbreviation();
                    if (parameterName == null || parameterName.isEmpty()) {
                        if (parameterAbbreviation == null) {
                            parameterAbbreviation = "";
                        }
                        parameterName = parameterAbbreviation;
                    }
                    ensembleId = record.getEnsembleId();
                }

                // add Unit to levelUnitMap if needed ( quick look-ups )
                Level lvl = record.getLevel();
                Unit<?> unit = levelUnitMap.get(lvl);
                if (unit == null) {
                    unit = record.getParameter().getUnit();
                    levelUnitMap.put(lvl, unit);
                }

                // add record to records by time
                Set<GridRecord> recordsAtTime = recordsByTime.get(record
                        .getDataTime());
                if (recordsAtTime == null) {
                    recordsAtTime = new HashSet<GridRecord>();
                    recordsByTime.put(record.getDataTime(), recordsAtTime);
                }
                recordsAtTime.add(record);
            }
        } else {
            // this shouldn't happen, code expects all pdo's for
            // GribTimeSeriesAdapter to be GridRecords
            String message = "Unexpected PluginDataObject type; got "
                    + pdo.getClass().getName() + " expected GridRecord";
            statusHandler.handle(Priority.PROBLEM, message, new Exception(
                    message));
        }
    }

    @Override
    public boolean hasRecord(PluginDataObject pdo) {
        synchronized (recordsByTime) {
            Set<GridRecord> possibleRecords = recordsByTime.get(pdo
                    .getDataTime());
            if (possibleRecords != null && possibleRecords.contains(pdo)) {
                return true;
            }
            return false;
        }
    }

    private XYDataList loadInternal(GridRecord[] gribs) throws VizException {
        ArrayList<XYData> data = new ArrayList<XYData>();

        ISpatialObject area = gribs[0].getSpatialObject();

        Point index = null;
        try {
            index = PointUtil.determineIndex(resourceData.getCoordinate(),
                    area.getCrs(), MapUtil.getGridGeometry(area));
        } catch (Exception e) {
            throw new VizException("Error initializing data for time series.",
                    e);
        }
        if (index.x < 0 || index.y < 0 || index.x >= area.getNx()
                || index.y >= area.getNy()) {
            throw new VizException(
                    "Invalid point position.  Check that the point is within the current Map.");
        }
        Request request = Request.buildPointRequest(index);

        boolean isIcon = displayType == DisplayType.ICON;

        for (GridRecord rec : gribs) {
            IDataRecord[] records = cache.get(rec);
            if (records == null) {
                try {
                    records = DataCubeContainer.getDataRecord(rec, request,
                            null);
                } catch (DataCubeException e) {
                    throw new VizException(e);
                }
                cache.put(rec, records);
            }

            DataTime time = rec.getDataTime();
            XYData dataPoint = null;

            if (records.length == 2) {
                double u = getValue(records[0]);
                double v = getValue(records[1]);
                double speed = Math.hypot(u, v);
                double dir = Math.toDegrees(Math.atan2(-u, -v));

                if (!Double.isNaN(speed)) {
                    dataPoint = new XYWindImageData(time, speed, speed, dir);
                }
            } else {
                double value = getValue(records[0]);
                if (Double.isNaN(value)) {
                    continue;
                } else if (isIcon) {
                    dataPoint = new XYIconImageData(time, value, (int) value);
                } else {
                    dataPoint = new XYData(time, value);
                }
            }
            if (dataPoint != null){
                data.add(dataPoint);
            }
        }

        XYDataList list = new XYDataList();

        list.setData(data);
        return list;
    }

    private double getValue(IDataRecord record) {
        FloatDataRecord floatRecord = (FloatDataRecord) record;
        float value = floatRecord.getFloatData()[0];
        if (value < CubeUtil.MISSING) {
            return Double.NaN;
        } else {
            return value;
        }
    }

    @Override
    public void remove(DataTime time) {
        synchronized (recordsByTime) {
            recordsByTime.remove(time);
        }
    }

    @Override
    public String getEnsembleId() {
        return ensembleId;
    }
}
