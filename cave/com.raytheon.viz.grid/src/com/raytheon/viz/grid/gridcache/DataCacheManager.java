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
package com.raytheon.viz.grid.gridcache;

import java.util.ArrayList;
import java.util.HashMap;

import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Interface layer to the DataCache.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2009 3579       mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DataCacheManager {
    /**
     * The only instance of this class.
     */
    private static DataCacheManager instance = null;
    
    /**
     * Private constructor.
     */
    private DataCacheManager() {
    }

    /**
     * Get an instance of this class.
     * 
     * @return Instance of this class
     */
    public static synchronized DataCacheManager getInstance() {
        if (instance == null) {
            instance = new DataCacheManager();
        }

        return instance;
    }

    /**
     * Get the raw 3D float[].
     * 
     * @param lProp
     *      The LayerProperties
     * @return
     *      The float[]
     * @throws VizException
     */
    public float[] getDataCube(LayerProperty lProp) throws VizException {
        DataCache cache = DataCache.getInstance();

        DataCacheCube cube = cache.getCube(lProp);
        
        return cube.getRawData();
    }
    
    /**
     * Get the raw layer float[]
     * 
     * @param lProp
     *      The LayerProperties
     * @return
     *      The float[]
     * @throws VizException
     */
    public float[] getDataLayer(LayerProperty lProp) throws VizException {
        DataCache cache = DataCache.getInstance();
        float[] data = null;
        DataCacheCube cube = cache.getCube(lProp);
        
        // Get the layer of interest value
        HashMap<String, RequestConstraint> requestMap = lProp.getEntryQueryParameters(false);
        double requestedLevel = -999;
        if (requestMap.containsKey("modelInfo.level.levelonevalue")) {
            requestedLevel = Double.parseDouble(requestMap.get("modelInfo.level.levelonevalue").getConstraintValue());
        }

        // Find the layer of interest
        ArrayList<GribRecord> grStack = cube.getGribRecordStack();
        for (int i = 0; i < grStack.size(); i++) {
            GribRecord gr = grStack.get(i);
            
            double level = gr.getModelInfo().getLevel().getLevelonevalue();
            if ((requestedLevel != -999) && (level == requestedLevel)) {
                data = cube.getFdrStackData().get(i).getFloatData();
            }
        }
        
        return data;
    }
    
    /**
     * Get the raw float array.  It returns a layer if a layer
     * is specified in the GribRecord, otherwise returns the 
     * whole cube.
     * 
     * @param rec
     *      The GribRecord object
     * @return
     *      float[] of raw data values
     */
    public float[] getData(GribRecord rec) {
        DataCache cache = DataCache.getInstance();
        DataCacheCube cube = cache.getCube(rec);
        float[] fa = null;
        
        /* If a level is set, return data for that level */
        if (rec.getModelInfo().getLevelOneValue() != null) {
            
            double level1 = rec.getModelInfo().getLevelOneValue();

            // Find the layer of interest
            ArrayList<GribRecord> grStack = cube.getGribRecordStack();
            for (int i = 0; i < grStack.size(); i++) {
                GribRecord gr = grStack.get(i);
                
                double level = gr.getModelInfo().getLevel().getLevelonevalue();
                if ((level1 != -999) && (level == level1)) {
                    fa = cube.getFdrStackData().get(i).getFloatData();
                }
            }
            
            return fa;
        }

        // no level set, return the whole cube
        
        return cube.getRawData();
    }
}
