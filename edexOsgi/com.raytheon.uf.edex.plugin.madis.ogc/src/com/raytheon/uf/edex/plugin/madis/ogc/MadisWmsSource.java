package com.raytheon.uf.edex.plugin.madis.ogc;

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
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.reg.PointDataWmsSource;
import com.raytheon.uf.edex.wms.styling.FeatureStyleProvider;

/**
 * MADIS WMS Source
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 18, 2013 2097       dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class MadisWmsSource extends
        PointDataWmsSource<MadisDimension, MadisLayer> {

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
    public MadisWmsSource(PluginProperties props,
            LayerTransformer<MadisDimension, MadisLayer> transformer)
            throws Exception {
        super(props, "madis", transformer, new MadisFeatureFactory());
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
        // madis has only one layer
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
