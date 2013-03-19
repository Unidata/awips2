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

import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

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
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
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
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public abstract class WCSLayerCollector<L extends SimpleLayer, R extends PluginDataObject>
        extends LayerCollector<L> implements IWCSMetaData {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WCSLayerCollector.class);

    protected Class<R> recordClass;

    protected Class<L> layerClass;
    
    protected GriddedDataSet gds = null;
    
    protected GriddedDataSetMetaData gdsmd = null;
    
    protected Time time = null;
    

    public WCSLayerCollector(LayerTransformer transformer,
            Class<L> layerClass, Class<R> recordClass) {
        super(transformer);
        this.recordClass = recordClass;
        this.layerClass = layerClass;
    }

    public void add(PluginDataObject... pdos) {
        if (pdos.length > 0) {
            addAll(Arrays.asList(pdos));
        }
    }

    @SuppressWarnings("unchecked")
    public void addAll(Collection<? extends PluginDataObject> coll) {
        HashMap<String, L> layermap = new HashMap<String, L>(coll.size());
        for (PluginDataObject pdo : coll) {
            if (recordClass.equals(pdo.getClass())) {
                R rec = (R) pdo;
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
        for (String key : layermap.keySet()) {
            try {
                updateLayer(layermap.get(key));
            } catch (DataAccessLayerException e) {
                statusHandler.error("Problem updating the layer table", e);
            }
        }

        sendMetaData(layermap, coll);
    }

    public L newLayer() {
        try {
            return layerClass.newInstance();
        } catch (Exception e) {
            statusHandler
                    .error("Unable to instantiate class: " + layerClass, e);
            throw new RuntimeException(e);
        }
    }

    protected void addToTimes(L layer, R rec) {
        Date refTime = rec.getDataTime().getRefTime();
        layer.getTimes().add(refTime);
    }

    protected void addToDims(L layer, R rec) {
        // default is to do nothing
    }

    protected abstract boolean initializeLayer(L layer, R rec);

    protected abstract String getLayerName(R rec);

    @SuppressWarnings("unchecked")
    public void purgeExpired() {
        try {
            clearLayers(layerClass);
            DaoConfig conf = DaoConfig.forClass(recordClass);
            CoreDao dao = new CoreDao(conf);
            DatabaseQuery q = new DatabaseQuery(recordClass);
            q.setMaxResults(500);
            q.addOrder("dataTime.refTime", false);
            List<R> recs = (List<R>) dao.queryByCriteria(q);
            addAll(recs);
        } catch (Exception e) {
            statusHandler.error("Problem purging layers", e);
        }
    }

    public void purgeAll() {
        try {
            clearLayers(layerClass);
        } catch (Exception e) {
            statusHandler.error("problem purging layers", e);
        }
    }
    
    protected void setCoverage(String name) {
        //TODO:  NOt yet implemented

    }
    
    /**
     * WFS uses Point Data type
     * @return
     */
    public DataType getDataType() {
        return DataType.GRID;
    }
    
    public GriddedDataSet getDataSet() {
        return gds;
    }
    
    public GriddedDataSetMetaData getDataSetMetaData() {
        return gdsmd;
    }
    
    //TODO implement this when we do WCS
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


}
