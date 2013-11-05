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
package com.raytheon.uf.edex.plugin.grib.ogc;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.WmsException.Code;
import com.raytheon.uf.edex.wms.reg.DefaultWmsSource;
import com.raytheon.uf.edex.wms.styling.ColormapStyleProvider;
import com.raytheon.uf.edex.wms.styling.ICoverageStyleProvider;

/**
 * 
 * @author jelkins
 * @version 1.0
 */
public class GribWmsSource extends
        DefaultWmsSource<GribDimension, GridParamLayer, GridRecord> {

    protected ColormapStyleProvider<GridRecord> styler = new GridStyleProvider(
            this, "Grid/Default");

    public GribWmsSource(PluginProperties props,
            LayerTransformer<GribDimension, GridParamLayer> transformer)
            throws PluginException {
        super(props, props.getPluginName(), transformer);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wms.reg.DefaultWmsSource#getRecord(java.lang.String,
     * java.lang.String, java.lang.String, java.util.Map)
     */
    @Override
    protected GridRecord getRecord(String layer, String time, String elevation,
            Map<String, String> dimensions) throws WmsException {
        LayerTransformer<GribDimension, GridParamLayer> transformer;
        List<GridRecord> res;
        try {
            transformer = getTransformer();
            res = GribRecordFinder.findWms(transformer, key, layer, time,
                    dimensions);
        } catch (OgcException e) {
            WmsException err = new WmsException(e);
            if (err.getCode().equals(Code.InternalServerError)) {
                log.error("Problem getting grib layer: " + layer);
            }
            throw err;
        } catch (PluginException e) {
            log.error("Unable to get transformer for grib", e);
            throw new WmsException(Code.InternalServerError);
        }
        if (res.isEmpty()) {
            throw new WmsException(Code.LayerNotDefined,
                    "No layer matching all specified dimensions found");
        }
        if (res.size() > 1) {
            Collections.sort(res, new GribRecordFinder.Comp());
        }
        return res.get(0);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wms.reg.DefaultWmsSource#getStyleProvider(java.lang
     * .String)
     */
    @Override
    protected ICoverageStyleProvider<GridRecord> getStyleProvider(String layer)
            throws WmsException {
        return styler;
    }

}
