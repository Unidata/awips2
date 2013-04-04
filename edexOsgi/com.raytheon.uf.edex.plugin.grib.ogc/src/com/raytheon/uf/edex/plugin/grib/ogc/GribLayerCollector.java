package com.raytheon.uf.edex.plugin.grib.ogc;

import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;

import org.geotools.geometry.jts.JTS;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.ogc.common.db.DefaultLayerCollector;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.ogc.common.db.SimpleDimension;
import com.vividsolutions.jts.geom.Envelope;

public class GribLayerCollector extends
        DefaultLayerCollector<GribLayer, GribRecord> {

    public GribLayerCollector(LayerTransformer transformer) {
        super(transformer, GribLayer.class, GribRecord.class);
    }

    @Override
    protected void addToDims(GribLayer layer, GribRecord rec) {
        DataTime dt = rec.getDataTime();
        GribModel modelInfo = rec.getModelInfo();
        Level level = modelInfo.getLevel();
        Set<? extends SimpleDimension> dims = layer.getDimensions();
        for (SimpleDimension d : dims) {
            String name = d.getName();
            Set<String> values = d.getValues();
            if (GribDimension.REFTIME_DIM.equals(name)) {
                values.add(LayerTransformer.format(dt.getRefTime()));
            } else if (GribDimension.FORECAST_OFFSET_DIM.equals(name)) {
                values.add(dt.getFcstTime() + "S");
            } else if (GribDimension.LEVEL1_DIM.equals(name)) {
                values.add(level.getLevelOneValueAsString());
            } else if (GribDimension.LEVEL2_DIM.equals(name)) {
                values.add(level.getLevelTwoValueAsString());
            } else if (GribDimension.PERTURB_DIM.equals(name)) {
                values.add(String.valueOf(modelInfo.getPerturbationNumber()));
            } else if (GribDimension.ENSEMBLE_DIM.equals(name)) {
                values.add(String.valueOf(modelInfo.getTypeEnsemble()));
            } else if (GribDimension.VERSION_DIM.equals(name)) {
                values.add(String.valueOf(rec.getGridVersion()));
            } else {
                log.warn("Unkown grib dimension: " + name);
            }
        }
    }

    @Override
    protected void addToTimes(GribLayer layer, GribRecord rec) {
        Set<Date> times = layer.getTimes();
        times.add(rec.getDataTime().getValidTime().getTime());
    }

    @Override
    protected boolean initializeLayer(GribLayer layer, GribRecord rec) {
        GribModel model = rec.getModelInfo();
        GridCoverage cov = model.getLocation();
        if (cov == null) {
            log.warn("Recieved record without coverage!");
            return false;
        }
        layer.setNx(cov.getNx());
        layer.setNy(cov.getNy());
        layer.setTargetCrsCode("CRS:84");
        try {
            Envelope env = getProperBounds(cov);
            layer.setTargetMinx(env.getMinX());
            layer.setTargetMiny(env.getMinY());
            layer.setTargetMaxx(env.getMaxX());
            layer.setTargetMaxy(env.getMaxY());
            layer.setCrs84Bounds(JTS.toGeometry(env));
        } catch (Exception e) {
            log.error("Unable to get crs84 bounds", e);
            return false;
        }
        layer.setTimes(new TreeSet<Date>());
        String levelUnit = rec.getModelInfo().getLevelUnit();
        TreeSet<GribDimension> dims = new TreeSet<GribDimension>();
        dims.add(new GribDimension(GribDimension.REFTIME_DIM, "ISO8601"));
        dims.add(new GribDimension(GribDimension.FORECAST_OFFSET_DIM, "ISO8601"));
        dims.add(new GribDimension(GribDimension.LEVEL1_DIM, levelUnit));
        dims.add(new GribDimension(GribDimension.LEVEL2_DIM, levelUnit));
        dims.add(new GribDimension(GribDimension.PERTURB_DIM, null));
        dims.add(new GribDimension(GribDimension.ENSEMBLE_DIM, null));
        dims.add(new GribDimension(GribDimension.VERSION_DIM, null));
        layer.setDimensions(dims);
        return true;
    }

    protected ReferencedEnvelope getProperBounds(GridCoverage cov)
            throws FactoryException, MismatchedDimensionException,
            TransformException {
        // the polygon is not projected properly, must get native bounds and
        // reproject into crs:84
        CoordinateReferenceSystem nativeCrs = cov.getCrs();
        Envelope env = JTS.transform(cov.getGeometry(),
                MapUtil.getTransformFromLatLon(nativeCrs))
                .getEnvelopeInternal();
        ReferencedEnvelope nativeEnv = new ReferencedEnvelope(env.getMinX(),
                env.getMaxX(), env.getMinY(), env.getMaxY(), nativeCrs);
        return nativeEnv.transform(MapUtil.LATLON_PROJECTION, true);
    }

    @Override
    protected String getLayerName(GribRecord rec) {
        GribModel modelInfo = rec.getModelInfo();
        String modelName = modelInfo.getModelName();
        String parameter = modelInfo.getParameterAbbreviation();
        String level = modelInfo.getLevel().getMasterLevel().getName();
        return modelName + "/" + parameter + "/" + level;
    }

    @Override
    protected void sendMetaData(HashMap<String, GribLayer> layermap,
            Collection<? extends PluginDataObject> coll) {

        for (Entry<String, GribLayer> entry : layermap.entrySet()) {
            GribLayer layer = entry.getValue();

            // for (GribDimension)

            // GriddedDataSetMetaData gdsmd = new GriddedDataSetMetaData();
            // GriddedCoverage griddedCoverage = new GriddedCoverage();
            // GridCoverage gridCoverage =
            layer.toString();

        }

        for (PluginDataObject pdo : coll) {
            GribRecord gr = (GribRecord) pdo;
            gr.toString();
        }

    }
    /**
     * Get's the correct gridCoverage
     * 
     * @return
     * 
     *         protected GridCoverage getGridCoverage() {
     * 
     *         GridCoverage gridCoverage = null;
     * 
     *         if (getProvider().getProjection().getName()
     *         .equals(LatLonGridCoverage.PROJECTION_TYPE)) { gridCoverage = new
     *         LatLonGridCoverage(); } else if
     *         (getProvider().getProjection().getName()
     *         .equals(LambertConformalGridCoverage.PROJECTION_TYPE)) {
     *         gridCoverage = new LambertConformalGridCoverage(); } else if
     *         (getProvider().getProjection().getName()
     *         .equals(MercatorGridCoverage.PROJECTION_TYPE)) { gridCoverage =
     *         new MercatorGridCoverage(); } else if
     *         (getProvider().getProjection().getName()
     *         .equals(PolarStereoGridCoverage.PROJECTION_TYPE)) { gridCoverage
     *         = new PolarStereoGridCoverage(); }
     * 
     *         return gridCoverage;
     * 
     *         }
     */
}
