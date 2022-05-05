/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
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

import java.awt.Point;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMapping;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.derivparam.library.DerivParamField;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.PointUtil;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import org.locationtech.jts.geom.Coordinate;

/**
 * A requestable data object for importing grid data into point data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Aug 15, 2018  7019     bsteffen  Initial creation
 * 
 * </pre>
 *
 * @author bsteffen
 */
public class GridImportRequestableData extends AbstractRequestableData {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridImportRequestableData.class);

    private final DerivParamField gridField;

    private final AbstractRequestableData latRequest;

    private final AbstractRequestableData lonRequest;

    private final AbstractRequestableData timeRequest;

    /*
     * A cache of loaded data since the same data may be needed for multiple
     * locations or times.
     */
    private final Map<DataTime, PluginDataObject> gridRecs = new HashMap<>();

    public GridImportRequestableData(Level level, String parameter,
            DerivParamField gridField, AbstractRequestableData latRequest,
            AbstractRequestableData lonRequest,
            AbstractRequestableData timeRequest) {
        this.parameter = parameter;
        this.level = level;
        this.gridField = gridField;
        this.unit = gridField.getUnit();
        this.latRequest = latRequest;
        this.lonRequest = lonRequest;
        this.timeRequest = timeRequest;
    }

    @Override
    public Object getDataValue(Object arg) throws DataCubeException {
        FloatDataRecord latRec = (FloatDataRecord) latRequest.getDataValue(arg);
        FloatDataRecord lonRec = (FloatDataRecord) lonRequest.getDataValue(arg);
        float[] lats = latRec.getFloatData();
        float[] lons = lonRec.getFloatData();
        long[] times = ((LongDataRecord) timeRequest.getDataValue(arg))
                .getLongData();
        float[] resultArray = new float[lats.length];
        for (int i = 0; i < lats.length; i++) {
            resultArray[i] = getValue(lats[i], lons[i], times[i]);
        }
        gridRecs.clear();
        return new FloatDataRecord(this.parameter, "", resultArray);
    }

    private float getValue(float lat, float lon, long time)
            throws DataCubeException {
        PluginDataObject gridRec = getGridRec(time);
        ISpatialObject spatialObject = ((ISpatialEnabled) gridRec)
                .getSpatialObject();
        GridGeometry2D mapGeometry = MapUtil.getGridGeometry(spatialObject);

        CoordinateReferenceSystem crs = spatialObject.getCrs();
        Point position;
        try {
            position = PointUtil.determineIndex(new Coordinate(lon, lat), crs,
                    mapGeometry);
        } catch (Exception e) {
            statusHandler.debug("Failed to determine index for " + lon + ","
                    + lat + " in " + gridRec, e);
            return Float.NaN;
        }
        if (position.y < 0 || position.y >= spatialObject.getNy()
                || position.x < 0 || position.x >= spatialObject.getNx()) {
            return Float.NaN;
        }
        int index = position.y * spatialObject.getNx() + position.x;

        return ((float[]) gridRec.getMessageData())[index];
    }

    private PluginDataObject getGridRec(long time) throws DataCubeException {
        if (gridRecs.isEmpty()) {
            for (DataTime datatime : DataCubeContainer
                    .performTimeQuery(getConstraints(), false)) {
                gridRecs.put(datatime, null);
            }
        }
        DataTime closest = null;
        long bestDist = Long.MAX_VALUE;
        for (DataTime datatime : gridRecs.keySet()) {
            if (datatime == null) {
                continue;
            }
            long dist = Math.abs(time - datatime.getMatchValid());
            if (dist < bestDist) {
                closest = datatime;
                bestDist = dist;
            } else if (dist == bestDist
                    && datatime.getMatchRef() > closest.getMatchRef()) {
                closest = datatime;
                bestDist = dist;
            }
        }
        PluginDataObject gridRec = gridRecs.get(closest);
        if (gridRec == null) {
            gridRec = loadDataRecord(closest);
            gridRecs.put(closest, gridRec);
        }
        return gridRec;
    }

    private PluginDataObject loadDataRecord(DataTime time)
            throws DataCubeException {
        PluginDataObject[] pdos = DataCubeContainer.getData(getConstraints(),
                time);
        if (pdos == null || pdos.length == 0) {
            return null;
        }
        PluginDataObject gridRec = pdos[0];
        IDataRecord[] dr = DataCubeContainer.getDataRecord(gridRec);
        if (dr != null) {
            float[] data = (float[]) dr[0].getDataObject();
            gridRec.setMessageData(data);
        } else {
            gridRec = null;
        }
        return gridRec;
    }

    private Map<String, RequestConstraint> getConstraints() {
        return getConstraints(this.gridField, this.level);
    }

    public static Map<String, RequestConstraint> getConstraints(
            DerivParamField gridField, Level level) {
        Map<String, RequestConstraint> constraints = new HashMap<>();
        constraints.put(GridConstants.PLUGIN_NAME,
                new RequestConstraint(GridConstants.GRID));
        constraints.put(GridConstants.PARAMETER_ABBREVIATION,
                new RequestConstraint(gridField.getParam()));
        constraints.put(GridConstants.DATASET_ID,
                new RequestConstraint(gridField.getValidSource()));
        LevelMapping fieldMapping = gridField.getLevelMapping();
        if (fieldMapping != null) {
            List<Level> fieldLevels = fieldMapping.getLevels();
            if (fieldLevels != null && fieldLevels.size() == 1) {
                level = fieldLevels.get(0);
            }
        }
        constraints.put(GridConstants.MASTER_LEVEL_NAME,
                new RequestConstraint(level.getMasterLevel().getName()));
        constraints.put(GridConstants.LEVEL_ONE,
                new RequestConstraint(level.getLevelOneValueAsString()));
        constraints.put(GridConstants.LEVEL_TWO,
                new RequestConstraint(level.getLevelTwoValueAsString()));
        return constraints;
    }
}
