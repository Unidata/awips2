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
package com.raytheon.uf.edex.ogc.common.db;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType.LevelType;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Levels;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.retrieval.util.LookupManager;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.interfaces.IWCSMetaData;

/**
 * 
 * WCS Layer Collector
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/09/2012   754       dhladky      Modified from a class written by Brian Clements
 * 04/01/2013   1746      dhladky      Updated for MADIS configuration
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public abstract class WCSLayerCollector<DIMENSION extends SimpleDimension, L extends SimpleLayer<DIMENSION>, R extends PluginDataObject>
        extends DefaultLayerCollector<DIMENSION, L, R> implements
        IWCSMetaData<L, R> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(WCSLayerCollector.class);
    
    protected GriddedDataSet gds = null;
    
    protected GriddedDataSetMetaData gdsmd = null;
    
    protected Time time = null;
    

    public WCSLayerCollector(LayerTransformer<DIMENSION, L> transformer,
            Class<L> layerClass, Class<R> recordClass) {
        super(transformer, layerClass, recordClass);
    }

    @Override
    protected void setCoverage(L layer) {
        //TODO:  NOt yet implemented
    }
    

    @Override
    protected Coverage getCoverage() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    protected void setDataSet(L layer) {
        // TODO Auto-generated method stub
    }

    @Override
    protected void setDataSetMetaData(L layer) {
        // TODO Auto-generated method stub
    }
    
    /**
     * WFS uses Point Data type
     * @return
     */
    @Override
    public DataType getDataType() {
        return DataType.GRID;
    }
    
    @Override
    public GriddedDataSet getDataSet() {
        return gds;
    }
    
    @Override
    public GriddedDataSetMetaData getDataSetMetaData() {
        return gdsmd;
    }
    
    //TODO implement this when we do WCS
    @Override
    public Levels getLevels(DataLevelType type, String collectionName) {
        
        Levels levels = new Levels();

        try {

            double dz = 0.0;
            
            levels.setName(type.getType().getLevelType());
            levels.setLevelType(type.getId());

            if (type.getType().equals(LevelType.MB.getLevelType())) {
                List<Double> levelList = LookupManager.getInstance()
                        .getLevels(collectionName).getLevelXml();
                levels.setLevel(levelList);

            } else if (type.getType().equals(LevelType.SEAB.getLevelType())) {
                List<Double> levelList = LookupManager.getInstance()
                        .getLevels(collectionName).getLevelXml();
                levels.setLevel(levelList);
            } else {
                // default added when only one
                levels.addLevel(Double.NaN);
            }

            levels.setDz(dz);

        } catch (Exception e) {
            statusHandler.error("Level info" + collectionName + " url: "
                    + getDataSetMetaData().getUrl(), e);
        }

        return levels;
    }
    
    public void addAll(Collection<R> coll) {
        Map<String, L> layermap = new HashMap<String, L>(coll.size());
        for (R pdo : coll) {
            if (recordClass.equals(pdo.getClass())) {
                R rec = recordClass.cast(pdo);
                String name = getLayerName(rec);
                L layer = layermap.get(name);
                if (layer == null) {
                    layer = newLayer();
                    layer.setName(name);
                    if (initializeLayer(layer, rec)) {
                        layermap.put(name, layer);
                    } else {
                        continue;
                    }
                }
                addToTimes(layer, rec);
                addToDims(layer, rec);

                statusHandler.info("Adding layer " + layer.getName());
            }
        }
        
        sendMetaData(layermap, coll);
    }

}
