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
package com.raytheon.uf.common.dataplugin.scan.data;

import java.awt.Point;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.PointUtil;
import com.raytheon.uf.common.time.DataTime;
import org.locationtech.jts.geom.Coordinate;

/**
 * 
 * SCAN Model Data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/01/13     1569        D. Hladky   removed XML where not needed, removed SS println's left in code
 * 05/13/15     4487        D. Hladky   ISerializable is deprecated. Thread safety possible issue.
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

public class ModelData {

    private final ConcurrentMap<String, ConcurrentHashMap<String, GridRecord>> gribMap;

    private transient final Log logger = LogFactory.getLog(getClass());

    /**
     * Public constructor
     */
    public ModelData() {
        gribMap = new ConcurrentHashMap<String, ConcurrentHashMap<String, GridRecord>>();
    }

    /**
     * Check for type
     * 
     * @param type
     * @return
     */
    public boolean isType(String modelName, String type) {
        boolean key = false;

        if (gribMap.get(modelName) != null) {
            if (gribMap.get(modelName).keySet().contains(type)) {
                key = true;
            }
        }
        return key;
    }

    /**
     * Gets the grib record by URI
     * 
     * @return
     * @throws Exception
     */
    private synchronized GridRecord getRecord(String modelName, String prodType)
            throws Exception {
        return gribMap.get(modelName).get(prodType);
    }

    /**
     * Setter for Grib Record
     * 
     * @param gr
     */
    public synchronized void setGridRecord(String modelName, String prodType,
            GridRecord gr) {
        if (gribMap.get(modelName) != null) {
            if (gribMap.get(modelName).get(prodType) != null) {
                if (!gribMap.get(modelName).get(prodType).getDataURI()
                        .equals(gr.getDataURI())) {
                    // replace it
                    gribMap.get(modelName).put(prodType, gr);
                }
            } else {
                gribMap.get(modelName).put(prodType, gr);
            }
        } else {
            ConcurrentHashMap<String, GridRecord> modelHash = new ConcurrentHashMap<String, GridRecord>();
            modelHash.put(prodType, gr);
            // add new
            gribMap.put(modelName, modelHash);
        }
    }

    /**
     * Getter for Grib Record
     * 
     * @param gr
     */
    public synchronized GridRecord getGridRecord(String modelName,
            String prodType) {
        return gribMap.get(modelName).get(prodType);
    }

    /**
     * Get the grid geometry for a certain point
     * 
     * @param spatial
     * @return
     */
    private GridGeometry2D getGridGeometry(ISpatialObject spatial) {
        return MapUtil.getGridGeometry(spatial);
    }

    /**
     * Gets the nearest point in the grib
     * 
     * @param coor
     * @return
     */
    private Point getPoint(String modelName, String prodType, Coordinate coor) {
        Point point = null;
        try {
            point = PointUtil.determineIndex(coor,
                    getRecord(modelName, prodType).getSpatialObject().getCrs(),
                    getGridGeometry(getRecord(modelName, prodType)
                            .getSpatialObject()));
        } catch (Exception e) {
            logger.debug("No Grib point available.....");
        }
        return point;
    }

    /**
     * gets the value for a given type
     * 
     * @param coor
     * @return
     * @throws VizException
     */
    public double getValue(String modelName, String prodType, Coordinate coor) {
        double value = -99999.0;
        try {
        	//System.out.println("Freezing level---debug0 getValue "+modelName+" "+prodType+" "+coor+" "+gribMap.size());
            Point point = getPoint(modelName, prodType, coor);
            GridRecord gribRec = getRecord(modelName, prodType);
            FloatDataRecord rec = (FloatDataRecord) getRecord(modelName,
                    prodType).getMessageData();
            //System.out.println("Freezing level---debug1 getValue "+rec.getFloatData().length+" "+
            //		gribRec.getSpatialObject().getNx()+","+gribRec.getSpatialObject().getNy()+" "+point);
            value = rec.getFloatData()[(gribRec.getSpatialObject().getNx() * 
            		(gribRec.getSpatialObject().getNy()-point.y)) + point.x];
        } catch (Exception e) {
            logger.error("No Grib value available....." + modelName + " "
                    + prodType);
        }
        return value;
    }

    /**
     * Gets the time of the data
     * 
     * @return
     * @throws VizException
     */
    public DataTime getTime(String modelName, String prodType) {
        try {
            return getRecord(modelName, prodType).getDataTime();
        } catch (Exception e) {
            logger.error("No time value available....." + modelName + " "
                    + prodType);
            return null;
        }
    }

}
