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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMapping;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMappingFactory;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.level.LevelUtilities;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.style.level.SingleLevel;
import com.raytheon.uf.viz.objectiveanalysis.rsc.OAGridTransformer;
import com.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYDataList;
import com.raytheon.viz.core.graphing.xy.XYIconImageData;
import com.raytheon.viz.core.graphing.xy.XYWindImageData;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 7, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class PointDataTimeSeriesAdapter extends
        AbstractTimeSeriesAdapter<PluginDataObject> {

    private static final int GRID_SIZE = 100;

    private static final int GRID_SPACING = 5000;

    private Unit<?> unit = Unit.ONE;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.timeseries.adapter.ITimeSeriesAdapter#loadData()
     */
    @Override
    public XYDataList loadData() throws VizException {
        if (this.resourceData.getSource().endsWith("OA")) {
            return loadDataOA();
        } else {
            PluginDataObject[] recordsToLoad = null;
            synchronized (records) {
                recordsToLoad = new PluginDataObject[records.size()];
                Iterator<PluginDataObject> iter = records.iterator();
                for (int i = 0; i < recordsToLoad.length; ++i) {
                    recordsToLoad[i] = iter.next();
                }
            }
            return loadDataInternal(recordsToLoad);
        }
    }

    private XYDataList loadDataInternal(PluginDataObject[] recordsToLoad)
            throws VizException {
        RequestConstraint uriConstraint = new RequestConstraint();
        uriConstraint.setConstraintType(RequestConstraint.ConstraintType.IN);
        // Perhaps this should just be done using the resource metadatamap
        for (PluginDataObject pdo : recordsToLoad) {
            uriConstraint.addToConstraintValueList(pdo.getDataURI());
        }

        String parameter = resourceData.getYParameter().code;

        boolean isIcon = displayType == DisplayType.ICON;
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
        constraints.put("dataURI", uriConstraint);

        PointDataContainer pdc = DataCubeContainer.getPointData(
                recordsToLoad[0].getPluginName(), new String[] { "dataURI",
                        parameter }, resourceData.getLevelKey(), constraints);
        ArrayList<XYData> data = new ArrayList<XYData>();
        for (int uriCounter = 0; uriCounter < pdc.getAllocatedSz(); uriCounter++) {
            PointDataView pdv = pdc.readRandom(uriCounter);
            String dataURI = pdv.getString("dataURI");
            DataTime x = null;
            for (PluginDataObject pdo : recordsToLoad) {
                if (dataURI.equals(pdo.getDataURI())) {
                    x = pdo.getDataTime();
                }
            }
            Number y = pdv.getNumber(parameter);

            if (x == null) {
                continue;
            }

            // the parameter is a (wind) vector
            if (pdc.getParameters().contains(parameter + "[1]")) {

                if (y.intValue() != -9999) {
                    double windSpeed = y.doubleValue();
                    double windDirection = pdv.getNumber(parameter + "[1]")
                            .doubleValue();
                    data.add(new XYWindImageData(x, y, windSpeed, windDirection));
                }
            } else if (isIcon) {
                data.add(new XYIconImageData(x, y, y.intValue()));
            } else if (y.intValue() > -9000) {
                data.add(new XYData(x, y));
            }
        }

        unit = pdc.getDescription(parameter).getUnitObject();

        if (data.size() == 0) {
            throw new VizException("Data is not available");
        }

        XYDataList list = new XYDataList();

        list.setData(data);
        return list;
    }

    private XYDataList loadDataOAInternal(PluginDataObject[] recordsToLoad)
            throws VizException {
        Set<DataTime> times = new HashSet<DataTime>();
        for (PluginDataObject pdo : recordsToLoad) {
            times.add(this.resourceData.getBinOffset().getNormalizedTime(
                    pdo.getDataTime()));
        }
        Coordinate coord = resourceData.getPointCoordinate();
        CoordinateReferenceSystem crs = MapUtil.constructStereographic(
                MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                coord.y, coord.x);
        GeneralEnvelope generalEnvelope = new GeneralEnvelope(2);
        generalEnvelope.setCoordinateReferenceSystem(crs);

        double maxExtent = GRID_SPACING * GRID_SIZE / 2;
        generalEnvelope.setRange(0, -maxExtent, maxExtent);
        generalEnvelope.setRange(1, -maxExtent, maxExtent);

        GridGeometry2D gridGeom = new GridGeometry2D(new GeneralGridEnvelope(
                new int[] { 0, 0 }, new int[] { GRID_SIZE, GRID_SIZE }, false),
                generalEnvelope);

        OAGridTransformer transformer = new OAGridTransformer(gridGeom, crs,
                GRID_SIZE, 3);
        ArrayList<XYData> data = new ArrayList<XYData>();
        HashMap<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>(
                resourceData.getMetadataMap());
        for (DataTime time : times) {
            RequestConstraint dtConstraint = new RequestConstraint();

            TimeRange tr = this.resourceData.getBinOffset().getTimeRange(time);

            DataTime start = new DataTime(tr.getStart());
            DataTime end = new DataTime(tr.getEnd());

            String[] constraintList = { start.toString(), end.toString() };
            dtConstraint.setBetweenValueList(constraintList);
            dtConstraint.setConstraintType(ConstraintType.BETWEEN);
            constraints.put("dataTime", dtConstraint);
            float[] grid = transformer.computeGrid(
                    resourceData.getYParameter().code, constraints,
                    resourceData.getLevelKey());
            if (grid != null) {
                float y = grid[grid.length / 2 + GRID_SIZE / 2];
                data.add(new XYData(time, y));
            }
        }
        unit = transformer.getParmDescription().getUnitObject();

        if (data.size() == 0) {
            throw new VizException("Data is not available");
        }

        XYDataList list = new XYDataList();

        list.setData(data);
        return list;
    }

    public XYDataList loadDataOA() throws VizException {
        PluginDataObject[] recordsToLoad = null;
        synchronized (records) {
            recordsToLoad = new PluginDataObject[records.size()];
            Iterator<PluginDataObject> iter = records.iterator();
            for (int i = 0; i < recordsToLoad.length; ++i) {
                recordsToLoad[i] = iter.next();
            }
        }
        return loadDataOAInternal(recordsToLoad);
    }

    @Override
    public SingleLevel getLevel() {
        SingleLevel level = new SingleLevel("SURFACE");
        level.setValue(0.0);
        if (resourceData.getLevelKey().equals("Surface")) {
            // The level mapping for surface has way to much junk in it.
            return level;
        }
        try {
            LevelMapping mapping = LevelMappingFactory.getInstance(
                    LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE)
                    .getLevelMappingForKey(resourceData.getLevelKey());
            for (Level l : mapping.getLevels()) {
                if (LevelUtilities.isPressureLevel(l)) {
                    try {
                        level = new SingleLevel(l.getMasterLevel().getName());
                        level.setValue(l.getLevelonevalue());
                        break;
                    } catch (IllegalArgumentException e) {
                        level = new SingleLevel("SURFACE");
                        level.setValue(0.0);
                    }
                }
            }
        } catch (Exception e) {
            // return the default
        }
        return level;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter#
     * getDataUnit()
     */
    @Override
    public Unit<?> getDataUnit() {
        return unit;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter#
     * getParamterName()
     */
    @Override
    public String getParameterName() {
        return resourceData.getYParameter().name;
    }

    @Override
    public XYDataList loadRecord(PluginDataObject pdo) throws VizException {
        PluginDataObject[] recordsToLoad = new PluginDataObject[1];
        recordsToLoad[0] = pdo;
        if (this.resourceData.getSource().endsWith("OA")) {
            return loadDataOAInternal(recordsToLoad);
        } else {
            return loadDataInternal(recordsToLoad);
        }
    }
}
