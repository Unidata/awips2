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

import javax.measure.Unit;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.locationtech.jts.geom.Coordinate;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMapping;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMappingFactory;
import com.raytheon.uf.common.dataplugin.level.util.LevelUtilities;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.pointdata.PointDataConstants;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.FLAG;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.uf.viz.objectiveanalysis.rsc.OAGridTransformer;
import com.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYDataList;
import com.raytheon.viz.core.graphing.xy.XYIconImageData;
import com.raytheon.viz.core.graphing.xy.XYWindImageData;

import tec.uom.se.AbstractUnit;

/**
 * Adapter for converting pdos that are compatible with the point data api into
 * XYDataLists that can be used for time series.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 07, 2010           bsteffen    Initial creation
 * May 09, 2013  1869     bsteffen    Modified D2D time series of point data to
 *                                    work without dataURI.
 * Feb 17, 2014  2661     bsteffen    Use only u,v for vectors.
 * Nov 09, 2016  5986     tgurney     Move getDataTime to PointDataView
 *
 * </pre>
 *
 * @author bsteffen
 */

public class PointDataTimeSeriesAdapter
        extends AbstractTimeSeriesAdapter<PluginDataObject> {

    private static final int GRID_SIZE = 100;

    private static final int GRID_SPACING = 5000;

    private Unit<?> unit = AbstractUnit.ONE;

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
        boolean refTimeOnly = true;
        RequestConstraint dataTimeConstraint = new RequestConstraint();
        RequestConstraint refTimeConstraint = new RequestConstraint();
        // Perhaps this should just be done using the resource metadatamap
        for (PluginDataObject pdo : recordsToLoad) {
            DataTime dt = pdo.getDataTime();
            dataTimeConstraint.addToConstraintValueList(dt.toString());
            refTimeOnly &= !dt.getUtilityFlags().contains(FLAG.FCST_USED);
            if (refTimeOnly) {
                refTimeConstraint.addToConstraintValueList(
                        TimeUtil.formatToSqlTimestamp(dt.getRefTime()));
            }
        }

        String parameter = resourceData.getYParameter().code;

        Map<String, RequestConstraint> constraints = new HashMap<>(
                resourceData.getMetadataMap());
        String[] parameters = null;
        if (refTimeOnly) {
            refTimeConstraint
                    .setConstraintType(RequestConstraint.ConstraintType.IN);
            constraints.put("dataTime.refTime", refTimeConstraint);
            parameters = new String[] { PointDataConstants.DATASET_REFTIME,
                    parameter };
        } else {
            dataTimeConstraint
                    .setConstraintType(RequestConstraint.ConstraintType.IN);
            constraints.put("dataTime", dataTimeConstraint);
            parameters = new String[] { PointDataConstants.DATASET_REFTIME,
                    PointDataConstants.DATASET_FORECASTHR, parameter };
        }

        PointDataContainer pdc;
        try {
            pdc = DataCubeContainer.getPointData(
                    recordsToLoad[0].getPluginName(), parameters,
                    resourceData.getLevelKey(), constraints);
        } catch (DataCubeException e) {
            throw new VizException(e);
        }

        boolean isWind = pdc.getParameters().contains(parameter + "[1]");
        boolean isIcon = displayType == DisplayType.ICON;

        ArrayList<XYData> data = new ArrayList<>();
        for (int uriCounter = 0; uriCounter < pdc
                .getAllocatedSz(); uriCounter++) {
            PointDataView pdv = pdc.readRandom(uriCounter);
            DataTime x = pdv.getDataTime(refTimeOnly);
            Number y = pdv.getNumber(parameter);

            if (x == null || y.intValue() < -9000) {
                continue;
            }

            // the parameter is a (wind) vector
            if (isWind) {
                double u = y.doubleValue();
                double v = pdv.getNumber(parameter + "[1]").doubleValue();
                double speed = Math.hypot(u, v);
                double dir = Math.toDegrees(Math.atan2(-u, -v));

                data.add(new XYWindImageData(x, speed, speed, dir));

            } else if (isIcon) {
                data.add(new XYIconImageData(x, y, y.intValue()));
            } else {
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
        Set<DataTime> times = new HashSet<>();
        for (PluginDataObject pdo : recordsToLoad) {
            times.add(this.resourceData.getBinOffset()
                    .getNormalizedTime(pdo.getDataTime()));
        }
        Coordinate coord = resourceData.getPointCoordinate();
        CoordinateReferenceSystem crs = MapUtil.constructStereographic(
                MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS, coord.y,
                coord.x);
        GeneralEnvelope generalEnvelope = new GeneralEnvelope(2);
        generalEnvelope.setCoordinateReferenceSystem(crs);

        double maxExtent = GRID_SPACING * GRID_SIZE / 2;
        generalEnvelope.setRange(0, -maxExtent, maxExtent);
        generalEnvelope.setRange(1, -maxExtent, maxExtent);

        GridGeometry2D gridGeom = new GridGeometry2D(
                new GeneralGridEnvelope(new int[] { 0, 0 },
                        new int[] { GRID_SIZE, GRID_SIZE }, false),
                generalEnvelope);

        OAGridTransformer transformer = new OAGridTransformer(gridGeom, crs,
                GRID_SIZE, 3);
        ArrayList<XYData> data = new ArrayList<>();
        HashMap<String, RequestConstraint> constraints = new HashMap<>(
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
            LevelMapping mapping = LevelMappingFactory
                    .getInstance(
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

    @Override
    public Unit<?> getDataUnit() {
        return unit;
    }

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
