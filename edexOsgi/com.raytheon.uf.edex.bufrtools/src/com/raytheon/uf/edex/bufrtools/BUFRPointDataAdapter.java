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
package com.raytheon.uf.edex.bufrtools;

import java.io.File;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * Abstract base class for implementing data adapters for creating specific
 * subclasses of PluginDataObject from BUFR data.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20090629           2519 jkorman     Initial implementation.
 * May 14, 2014 2536       bclement    moved WMO Header to common, added breaks/default to switch
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public abstract class BUFRPointDataAdapter<T extends PluginDataObject> {

    public int PDV_FILL_INT = -9999;
    
    public double PDV_FILL_DBL = -9999.0;
    
    /** The logger */
    protected Log logger = LogFactory.getLog(getClass());

    private final PointDataDescription pointDataDescription;

    private final PointDataPluginDao<T> pointDataDao;

    private Map<File, PointDataContainer> containerMap;
    
    private String pluginName;
    
    /**
     * 
     * @param container
     */
    public BUFRPointDataAdapter(PointDataDescription pdd, PointDataPluginDao<T> dao, String pluginName) {
        pointDataDescription = pdd;
        pointDataDao = dao;
        this.pluginName = pluginName;
        containerMap = new HashMap<File,PointDataContainer>();
    }
    
    /**
     * @return the pointDataDescription
     */
    public PointDataDescription getPointDataDescription() {
        return pointDataDescription;
    }

    /**
     * 
     * @param obsData
     * @return
     */
    public PointDataContainer getContainer(T obsData) {
        
        File file = pointDataDao.getFullFilePath(obsData);
        PointDataContainer container = containerMap.get(file);
        if (container == null) {
            container = PointDataContainer.build(pointDataDescription);
            containerMap.put(file, container);
        }
        return container;
    }

    /**
     * 
     * @param obsData
     * @return
     */
    public PointDataContainer getContainer(T obsData, int size) {
        
        File file = pointDataDao.getFullFilePath(obsData);
        PointDataContainer container = containerMap.get(file);
        if (container == null) {
            container = PointDataContainer.build(pointDataDescription, size);
            containerMap.put(file, container);
        }
        return container;
    }

    
    /**
     * Construct a single observation instance from the BUFR decoded data contained in
     * the specified separator.
     * 
     * @param iterator
     *            An iterator containing decoded BUFR data.
     * @param wmoHeader
     *            a wmoHeader
     * @return A ProfilerObs instance, or null in the event of an error.
     */
    public abstract T createData(Iterator<BUFRDataDocument> iterator, WMOHeader wmoHeader);
    
    /**
     * Construct a list of observation instances from the BUFR decoded data contained in
     * the specified separator.
     * 
     * @param iterator Iterator to the data document to be decoded.
     * @param wmoHeader
     * @return
     */
    public abstract List<T> createDataList(Iterator<BUFRDataDocument> iterator, WMOHeader wmoHeader);
    
    /**
     * @return the pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * @param pluginName the pluginName to set
     */
    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * 
     * @param packet
     * @param defaultValue
     * @return
     */
    public double getDouble(IBUFRDataPacket packet, double defaultValue) {
        double retValue = defaultValue;
        if(packet != null) {
            Object o = packet.getValue();
            if(o instanceof Double) {
                retValue = ((Double) o).doubleValue();
            } else if(o instanceof Long) {
                retValue = ((Long) o).doubleValue();
            } 
        }
        return retValue;
    }

    /**
     * 
     * @param packet
     * @param defaultValue
     * @return
     */
    public int getInt(IBUFRDataPacket packet, int defaultValue) {
        int retValue = defaultValue;
        if(packet != null) {
            Object o = packet.getValue();
            if(o instanceof Double) {
                retValue = ((Double) o).intValue();
            } else if(o instanceof Long) {
                retValue = ((Long) o).intValue();
            } 
        }
        return retValue;
    }
    
    /**
     * 
     * @param parmName
     * @param view
     * @param packet
     */
    public void setViewData(String parmName, PointDataView view, IBUFRDataPacket packet) {
        setViewData(parmName,view,packet, 0);
    }

    /**
     * @param parmName
     * @param view
     * @param packet
     */
    public void setViewData(String parmName, PointDataView view, IBUFRDataPacket packet, int index) {
        if(packet != null) {
            Type t = view.getType(parmName);
            Object o = packet.getValue();
            if(o != null) {
                switch (t) {
                case STRING:
                    if(o instanceof String) {
                        view.setString(parmName,(String) o, index);
                    } 
                    break;
                case INT:
                    if(o instanceof Double) {
                        view.setInt(parmName,((Double) o).intValue(), index);
                    } else if(o instanceof Long) {
                        view.setInt(parmName,((Long) o).intValue(), index);
                    } 
                    break;
                case LONG:
                    if(o instanceof Double) {
                        view.setLong(parmName,((Double) o).longValue(), index);
                    } else if(o instanceof Long) {
                        view.setLong(parmName,(Long) o, index);
                    } 
                    break;
                case FLOAT:
                    if(o instanceof Double) {
                        view.setFloat(parmName,((Double) o).floatValue(), index);
                    } else if(o instanceof Long) {
                        view.setFloat(parmName,((Long) o).floatValue(), index);
                    } 
                    break;
                default:
                    logger.warn("Unsupported point data view type: " + t);
                }
            }
        }
    }

}
