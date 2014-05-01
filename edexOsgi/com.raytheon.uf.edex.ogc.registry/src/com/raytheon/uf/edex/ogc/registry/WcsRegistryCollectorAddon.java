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
package com.raytheon.uf.edex.ogc.registry;

import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType.LevelType;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Levels;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.retrieval.util.LookupManager;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.db.SimpleDimension;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;
import com.raytheon.uf.edex.ogc.common.interfaces.IWCSMetaData;

/**
 * WCS Registry layer collector
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 25, 2013            bclement     Initial creation
 * Aug 18, 2013  #2097     dhladky      Adapted to AWIPS
 * Jan 13, 2014  #2679     dhladky      Multiple layers
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class WcsRegistryCollectorAddon<D extends SimpleDimension, L extends SimpleLayer<D>, R extends PluginDataObject>
        extends RegistryCollectorAddon<D, L, R> implements IWCSMetaData<L, R> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(WcsRegistryCollectorAddon.class);

    protected GriddedDataSet gds = null;

    protected GriddedDataSetMetaData gdsmd = null;

    protected Time time = null;
    
    public WcsRegistryCollectorAddon() {
        super();
    }

    @Override
    protected void setCoverage(L layer) {
        // TODO: NOt yet implemented
    }

    @Override
    protected void setDataSet(L layer) {
        // TODO Auto-generated method stub
    }

    @Override
    protected void setDataSetMetaData(L layer) {
        // TODO Auto-generated method stub
    }

    @Override
    public DataType getDataType() {
        return DataType.GRID;
    }

    // TODO implement this when we do WCS
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
            statusHandler.error("Level info" + collectionName + ":", e);
        }

        return levels;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.db.CollectorAddon#onCollect(com.raytheon
     * .uf.edex.ogc.common.db.SimpleLayer,
     * com.raytheon.uf.common.dataplugin.PluginDataObject)
     */
    @Override
    public void onCollect(L layer, R record) {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.ogc.common.db.CollectorAddon#onFinish()
     */
    @Override
    public void onFinish() {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.ogc.common.db.CollectorAddon#onPurgeAll()
     */
    @Override
    public void onPurgeAll() {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.db.CollectorAddon#onPurgeExpired(java
     * .util.Set)
     */
    @Override
    public void onPurgeExpired(Set<Date> timesToKeep) {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.interfaces.IWCSMetaData#sendMetaData(
     * java.util.Map, java.util.Collection)
     */
    @Override
    public void sendMetaData(Map<String, L> layermap, Collection<R> coll) {
        // TODO Auto-generated method stub

    }

    @Override
    protected Coverage getCoverage(String layerName) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    protected DataSet<?, ?> getDataSet(String layerName) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    protected DataSetMetaData<?> getDataSetMetaData(String layerName) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String isWithinLayer(R record) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ISpatialObject getSpatial(R record) {
        // TODO Auto-generated method stub
        return null;
    }

}
