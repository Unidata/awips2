package com.raytheon.uf.edex.plugin.madis.ogc;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.reg.PointDataWmsSource;
import com.raytheon.uf.edex.wms.styling.FeatureStyleProvider;

public class MadisWmsSource extends PointDataWmsSource {

    private static final String geometryField = "location.location";

    private static final FeatureStyleProvider styler = new FeatureStyleProvider(
            "sld/madis/defaultMadis.sld");

    /**
     * @param props
     * @param key
     * @param layerTable
     * @param styles
     * @throws Exception
     */
    public MadisWmsSource(PluginProperties props, LayerTransformer transformer)
            throws Exception {
        super(props, "madis", transformer,
                new MadisFeatureFactory());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wms.reg.FeatureWmsSource#getGeometryField(java.lang
     * .String)
     */
    @Override
    protected String getGeometryField(String layer) {
        // metar has only one layer
        return geometryField;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wms.reg.FeatureWmsSource#getCRS(java.lang.String)
     */
    @Override
    protected CoordinateReferenceSystem getCRS(String layer) {
        
        return MapUtil.LATLON_PROJECTION;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wms.reg.FeatureWmsSource#getStyleProvider(java.lang
     * .String)
     */
    @Override
    protected FeatureStyleProvider getStyleProvider(String layer)
            throws WmsException {
        return styler;
    }
}
