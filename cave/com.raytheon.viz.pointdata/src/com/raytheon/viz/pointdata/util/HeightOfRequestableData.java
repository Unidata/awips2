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

import java.awt.Point;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.unit.SI;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.edex.meteoLib.Controller;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.PointUtil;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Determines the height of a point using gridded data if it is available.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 14, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class HeightOfRequestableData extends AbstractRequestableData {

    private AbstractRequestableData latRequest;

    private AbstractRequestableData lonRequest;

    private AbstractRequestableData timeRequest;

    private Map<DataTime, PluginDataObject> gribRecs = null;

    public HeightOfRequestableData(Level level, String parameter,
            AbstractRequestableData latRequest,
            AbstractRequestableData lonRequest,
            AbstractRequestableData timeRequest) {
        this.parameter = parameter;
        this.level = level;
        this.unit = SI.METER;
        this.latRequest = latRequest;
        this.lonRequest = lonRequest;
        this.timeRequest = timeRequest;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.data.AbstractRequestableData#getDataValue
     * ()
     */
    @Override
    public Object getDataValue(Object arg) throws VizException {
        FloatDataRecord latRec = (FloatDataRecord) latRequest.getDataValue(arg);
        FloatDataRecord lonRec = (FloatDataRecord) lonRequest.getDataValue(arg);
        float[] lats = latRec.getFloatData();
        float[] lons = lonRec.getFloatData();
        long[] times = null;
        if (timeRequest != null) {
            times = ((LongDataRecord) timeRequest.getDataValue(arg))
                    .getLongData();
        }
        float[] hgt = new float[lats.length];
        for (int i = 0; i < lats.length; i++) {
            if (times == null) {
                hgt[i] = getHeight(lats[i], lons[i], null);
            } else {
                hgt[i] = getHeight(lats[i], lons[i], times[i]);
            }
        }
        return new FloatDataRecord(this.parameter, "m", hgt);
    }

    private float getHeight(float lat, float lon, Long time) {
        PluginDataObject gribRec;
        try {
            gribRec = getGribRec(time);
        } catch (VizException e1) {
            return Controller.ptozsa((float) level.getLevelonevalue());
        }
        if (gribRec == null) {
            return Controller.ptozsa((float) level.getLevelonevalue());
        } else {
            ISpatialObject spatialObject = ((ISpatialEnabled) gribRec)
                    .getSpatialObject();
            GridGeometry2D mapGeometry = MapUtil.getGridGeometry(spatialObject);

            CoordinateReferenceSystem crs = spatialObject.getCrs();
            Point position;
            try {
                position = PointUtil.determineIndex(new Coordinate(lon, lat),
                        crs, mapGeometry);
            } catch (Exception e) {
                return Controller.ptozsa((float) level.getLevelonevalue());
            }
            if (position.y < 0 || position.y >= spatialObject.getNy()
                    || position.x < 0 || position.x >= spatialObject.getNx()) {
                return Controller.ptozsa((float) level.getLevelonevalue());
            }
            int index = position.y * spatialObject.getNx() + position.x;

            Float altitude = ((float[]) gribRec.getMessageData())[index];
            return altitude.intValue();
        }
    }

    private PluginDataObject getGribRec(Long time) throws VizException {
        if (gribRecs == null) {
            gribRecs = new HashMap<DataTime, PluginDataObject>();
            if (time != null) {
                for (DataTime datatime : DataCubeContainer.performTimeQuery(
                        getConstraints(), false)) {
                    gribRecs.put(datatime, null);
                }
            }
        }
        DataTime closest = null;
        if (time != null) {
            long bestDist = Long.MAX_VALUE;
            for (DataTime datatime : gribRecs.keySet()) {
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
        }
        PluginDataObject gribRec = gribRecs.get(closest);
        if (gribRec == null) {
            gribRec = loadPressureLevel(closest);
            gribRecs.put(closest, gribRec);
        }
        return gribRec;
    }

    private Map<String, RequestConstraint> getConstraints() {
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
        constraints.put(GridConstants.PLUGIN_NAME, new RequestConstraint(
                GridConstants.GRID));
        constraints.put(GridConstants.PARAMETER_ABBREVIATION,
                new RequestConstraint("GH"));
        constraints.put(GridConstants.DATASET_ID, new RequestConstraint(
                "GFS212"));
        constraints.put(GridConstants.MASTER_LEVEL_NAME, new RequestConstraint(
                level.getMasterLevel().getName()));
        constraints.put(GridConstants.LEVEL_ONE, new RequestConstraint(level
                .getLevelOneValueAsString().toString()));
        constraints.put(GridConstants.LEVEL_TWO,
                new RequestConstraint(Level.getInvalidLevelValueAsString()));
        return constraints;
    }

    /**
     * load a grib record for the specified altitude
     * 
     * @param altitudeInHPa
     *            the altitude to load in hPa
     * @throws VizException
     */
    private PluginDataObject loadPressureLevel(DataTime time)
            throws VizException {

        LayerProperty lp = new LayerProperty();
        lp.setNumberOfImages(1);
        lp.setEntryQueryParameters(getConstraints(), false);
        if (time != null) {
            lp.setSelectedEntryTimes(new DataTime[] { time });
        }
        List<Object> resp = DataCubeContainer.getData(lp, 60000);
        if (resp.isEmpty()) {
            return null;
        }
        PluginDataObject gribRec = (PluginDataObject) resp.get(0);
        IDataRecord[] dr = DataCubeContainer.getDataRecord(gribRec);
        if (dr != null) {
            float[] data = (float[]) dr[0].getDataObject();
            gribRec.setMessageData(data);
        } else {
            gribRec = null;
        }
        return gribRec;
    }
}
