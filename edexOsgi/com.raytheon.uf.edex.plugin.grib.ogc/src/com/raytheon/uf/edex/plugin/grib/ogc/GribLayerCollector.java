/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */

package com.raytheon.uf.edex.plugin.grib.ogc;

import java.util.Set;
import java.util.TreeSet;

import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.geotools.geometry.jts.JTS;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.grid.GridInfoRecord;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.ogc.common.OgcLayer;
import com.raytheon.uf.edex.ogc.common.db.DefaultLayerCollector;
import com.raytheon.uf.edex.ogc.common.db.ILayerStore;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.ogc.common.db.SimpleDimension;
import com.raytheon.uf.edex.ogc.common.level.LevelDimUtil;
import com.raytheon.uf.edex.ogc.common.spatial.AltUtil;
import com.vividsolutions.jts.geom.Envelope;

public class GribLayerCollector extends
        DefaultLayerCollector<GribDimension, GridCompositeLayer, GridRecord> {

    public GribLayerCollector(ILayerStore store) {
        super(GridCompositeLayer.class, GridRecord.class, store);
    }

    @Override
    protected void addToDims(GridCompositeLayer layer, GridRecord rec) {
        DataTime dt = rec.getDataTime();
        GridInfoRecord info = rec.getInfo();
        Level level = info.getLevel();
        Set<? extends SimpleDimension> dims = layer.getDimensions();
        for (SimpleDimension d : dims) {
            String name = d.getName();
            Set<String> values = d.getValues();
            if (GribDimension.REFTIME_DIM.equals(name)) {
                values.add(LayerTransformer.format(dt.getRefTime()));
            } else if (GribDimension.FORECAST_OFFSET_DIM.equals(name)) {
                values.add(dt.getFcstTime() + "S");
            } else if (name.startsWith(LevelDimUtil.LEVEL_DIM_PREFIX)) {
                values.add(LevelDimUtil.formatLevelValue(level));
            } else if (GribDimension.PARAM_DIM.equals(name)) {
                values.add(getParameter(rec));
            } else {
                log.warn("Unknown grib dimension: " + name);
            }
		}
    }

    /**
     * Get parameter string from record
     * 
     * @param rec
     * @return
     */
    private String getParameter(GridRecord rec) {
        String fieldName = rec.getInfo().getParameter().getAbbreviation();
        return GribRecordFinder.dbToOgcParameter(fieldName);
    }

    @Override
    protected void addToTimes(GridCompositeLayer layer, GridRecord rec) {
        String parameter = getParameter(rec);
        layer.addTime(parameter, rec.getDataTime().getValidTime().getTime());
    }

    @Override
    protected boolean initializeLayer(GridCompositeLayer layer, GridRecord rec) {
        GridInfoRecord info = rec.getInfo();
        Level level = info.getLevel();
        Parameter parameter = info.getParameter();
        if (parameter.getAbbreviation().startsWith("static")) {
            return false;
        }
        Unit<?> unit = level.getMasterLevel().getUnit();
        if (unit == null) {
            layer.setVertical(false);
        }
        try {
            AltUtil.convert(SI.METER, unit, 1);
        } catch (Exception e) {
            layer.setVertical(false);
        }
        GridCoverage cov = info.getLocation();
        if (cov == null) {
            log.warn("Recieved record without coverage!");
            return false;
        }
        layer.setCoverageName(cov.getName());
        String crsWkt = cov.getCrsWKT();
        if (crsWkt == null) {
            crsWkt = cov.getCrs().toWKT();
        }
        layer.setCrsWkt(crsWkt);
        layer.setNx(cov.getNx());
        layer.setNy(cov.getNy());
        layer.setTargetCrsCode("CRS:84");
        try {
            Envelope env = getProperBounds(layer, cov, info);
            layer.setTargetMinx(env.getMinX());
            layer.setTargetMiny(env.getMinY());
            layer.setTargetMaxx(env.getMaxX());
            layer.setTargetMaxy(env.getMaxY());
            layer.setCrs84Bounds(JTS.toGeometry(env));
        } catch (Exception e) {
            log.error("Unable to get crs84 bounds", e);
            return false;
        }
        String levelUnit = level.getMasterLevel().getUnitString();
        MasterLevel master = level.getMasterLevel();
        TreeSet<GribDimension> dims = new TreeSet<GribDimension>();
        dims.add(new GribDimension(GribDimension.REFTIME_DIM, "ISO8601"));
        dims.add(new GribDimension(GribDimension.FORECAST_OFFSET_DIM, "ISO8601"));
        String levelName = LevelDimUtil.LEVEL_DIM_PREFIX + master.getName();
        dims.add(new GribDimension(levelName, levelUnit));
        dims.add(new GribDimension(GribDimension.PARAM_DIM, parameter.getUnit()
                .toString()));
        layer.addDimensions(getParameter(rec), dims);
        return true;
    }

    protected ReferencedEnvelope getProperBounds(GribLayer layer,
            GridCoverage cov, GridInfoRecord info) throws FactoryException,
            MismatchedDimensionException, TransformException {
        String dataset = info.getDatasetId();
        // the gemglobal and gfs global grids are already in a crs:84
        // projection,
        // so there is no transformation needed.
        if (dataset.equals("GEMGlobal") || dataset.equals("GFS230")
                || dataset.equals("GlobalWave")) {
            return new ReferencedEnvelope(-180.0, 180.0, -90.0, 90.0,
                    cov.getCrs());
        }

        // the polygon is not projected properly, must get native bounds and
        // reproject into crs:84
        CoordinateReferenceSystem nativeCrs = cov.getCrs();
        Envelope env = JTS.transform(cov.getGeometry(),
                MapUtil.getTransformFromLatLon(nativeCrs))
                .getEnvelopeInternal();
        ReferencedEnvelope nativeEnv = new ReferencedEnvelope(env.getMinX(),
                env.getMaxX(), env.getMinY(), env.getMaxY(), nativeCrs);
        layer.setNativeMinX(env.getMinX());
        layer.setNativeMinY(env.getMinY());
        layer.setNativeMaxX(env.getMaxX());
        layer.setNativeMaxY(env.getMaxY());
        ReferencedEnvelope crs84Env = nativeEnv.transform(
                MapUtil.LATLON_PROJECTION, true);
        // This is to fix when non-polar coverages cross poles or opposite
        // meridian
        // This will completely break polar coverages
        double minx = crs84Env.getMinX();
        double miny = crs84Env.getMinY();
        double maxx = crs84Env.getMaxX();
        double maxy = crs84Env.getMaxY();
        if (maxx > 180 || minx < -180) {
            // coverage crosses opposite meridian, advertise all the way around
            minx = -180;
            maxx = 180;
        }
        if (maxy > 90) {
            // coverage crosses north pole, truncate
            maxy = 90;
        }
        if (miny < -90) {
            // coverage crosses south pole, truncate
            miny = -90;
        }
        return new ReferencedEnvelope(minx, maxx, miny, maxy,
                crs84Env.getCoordinateReferenceSystem());
    }

    @Override
    public String getLayerName(GridRecord rec) {
        return createLayerName(rec);
    }

    public static String createLayerName(GridRecord rec) {
        GridInfoRecord info = rec.getInfo();
        GridCoverage cov = info.getLocation();
        String levelName = info.getLevel().getMasterLevel().getName();
        return info.getDatasetId() + OgcLayer.keySeparator + cov.getName()
                + OgcLayer.keySeparator + levelName;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.db.LayerCollector#copy(com.raytheon.uf
     * .edex.ogc.common.db.SimpleLayer)
     */
    @Override
    protected GridCompositeLayer copy(GridCompositeLayer orig) {
        return new GridCompositeLayer(orig);
    }

}
