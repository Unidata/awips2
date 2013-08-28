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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import org.apache.commons.lang.StringUtils;

import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcLayer;
import com.raytheon.uf.edex.ogc.common.db.ILayerCache;

/**
 * Layer cache adapter to split composite levels into parameter levels
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 26, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class GridWmsLayerCache implements ILayerCache<GribDimension, GribLayer> {

    private final ILayerCache<GribDimension, GridCompositeLayer> cache;

    /**
     * @param cache
     */
    public GridWmsLayerCache(ILayerCache<GribDimension, GridCompositeLayer> cache) {
        this.cache = cache;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.ogc.common.db.LayerCache#getLayers()
     */
    @Override
    public List<GribLayer> getLayers() throws OgcException {
        List<GridCompositeLayer> origLayers = cache.getLayers();
        List<GribLayer> rval = new ArrayList<GribLayer>(origLayers.size());
        for (GridCompositeLayer orig : origLayers) {
            String[] parts = StringUtils.split(orig.getName(),
                    OgcLayer.keySeparator);
            for (String param : orig.getParameters()) {
                rval.add(getParamLayer(orig, parts, param));
            }
        }
        return rval;
    }

    /**
     * @param orig
     * @param name
     * @param param
     * @return
     */
    private GridParamLayer getParamLayer(GridCompositeLayer orig, String name,
            String param) {
        GridParamLayer paramLayer = new GridParamLayer(param, orig);
        paramLayer.setName(name);
        return paramLayer;
    }

    /**
     * @param orig
     * @param parts
     * @param param
     * @return
     */
    private GribLayer getParamLayer(GridCompositeLayer orig, String[] parts,
            String param) {
        LinkedList<String> partList = new LinkedList<String>(
                Arrays.asList(parts));
        partList.add(parts.length - 1, param);
        return getParamLayer(orig,
                StringUtils.join(partList, OgcLayer.keySeparator), param);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.db.LayerCache#getLayer(java.lang.String)
     */
    @Override
    public GribLayer getLayer(String name) throws OgcException {
        String[] parts = StringUtils.split(name,
                OgcLayer.keySeparator);
        LinkedList<String> partList = new LinkedList<String>(
                Arrays.asList(parts));
        String param = partList.remove(parts.length - 2);
        GridCompositeLayer layer = cache.getLayer(StringUtils.join(partList,
                OgcLayer.keySeparator));
        if (layer == null) {
            return null;
        }
        return getParamLayer(layer, name, param);
    }
}
