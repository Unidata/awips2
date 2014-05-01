package com.raytheon.uf.edex.dat.utils;

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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map.Entry;
import java.util.TreeSet;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.PointUtil;
import com.raytheon.uf.common.monitor.xml.SCANModelParameterXML;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Executes finding the freezing level for a given model
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 19Nov 2011              dhladky     Initial creation
 * 29 Jan 2013  15729      wkwock      fix the algorithm
 * Jan 07, 2013            njensen     Change some logs to debug
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class FreezingLevel {

    // pre -determined set of level_ids needed 1000-500MB vertically
    private static LinkedHashMap<String, Integer> ghLevels = null;

    // pre -determined set of level_ids needed 1000-500MB vertically
    private static LinkedHashMap<String, Integer> tLevels = null;

    private HashMap<Integer, Date> times = null;

    private String modelName = null;

    /** these are only used for the MPE RUC **/
    private int zeroCount = 0;

    private int sixCount = 0;

    private int twelveCount = 0;

    private int eighteenCount = 0;

    // reference time
    Calendar refTime = null;
    private transient final Log logger = LogFactory.getLog(getClass());
    
    public FreezingLevel(String modelName) {
        this.modelName = modelName;
        times = new HashMap<Integer, Date>();
        zeroCount = 0;
        sixCount = 0;
        twelveCount = 0;
        eighteenCount = 0;
        refTime = Calendar.getInstance();
        // only for get data for hour 00z,06z,12z, or 18z
        int adjustedHour = (refTime.get(Calendar.HOUR_OF_DAY) / 6) * 6;
        refTime.set(Calendar.HOUR_OF_DAY, adjustedHour);

        // populates what ever is missing, sets prevalent forecast hour
        for (Entry<String, Integer> entry : getGHLevelMap().entrySet()) {
            populateRecord(modelName, entry.getKey(), refTime.getTime());
        }

        for (Entry<String, Integer> entry : getTLevelMap().entrySet()) {
            populateRecord(modelName, entry.getKey(), refTime.getTime());
        }
    }

    /**
     * find (x,y) coordinate for lat,lon(coor)
     * @param coors
     */
    public DirectPosition2D findXYloc(Coordinate coor, String type){
        ScanDataCache cache = ScanDataCache.getInstance();

        ISpatialObject iso = cache.getModelData().getGridRecord(modelName, type).getSpatialObject();
        CoordinateReferenceSystem crs=iso.getCrs();
        GridGeometry2D mapGeometry = MapUtil.getGridGeometry(iso);
        DirectPosition2D resultPoint;
        try {
        	resultPoint = PointUtil.determineExactIndex(
		        coor, crs, mapGeometry);
        	return resultPoint;
       	} catch (Exception e) {
        	logger.error("Error: Freezing level -- unable to find x,y coordinate for lat,lon:"+coor);
        }
       	return null;
    }
    
    /**
     * get the bi-linear interpolation value amount the nearest 4 points
     * 
     * @param modelName, prodType, coor
     * @return bi-linear interpolation amount the nearest 4 points
     * @throws VizException
     */
    public Double getValue(String modelName, String prodType, Coordinate coor) {
        double value = -99999.0;
        try {
        	//xyLoc is the location in x,y
        	DirectPosition2D xyLoc = findXYloc(coor, prodType);

        	//data from hdf5
        	ScanDataCache cache = ScanDataCache.getInstance();
            GridRecord gribRec = cache.getModelData().getGridRecord(modelName, prodType);
            FloatDataRecord rec = (FloatDataRecord) gribRec.getMessageData();
            
            //dimension of the record from hdf5, recNx =151 and recNy=113 during development
            int recNx = gribRec.getSpatialObject().getNx();
            
            //get four nearest points/values form the record around xyLoc
            xyLoc.y=xyLoc.y * 0.9941; //A special adjustment due to PointUtil.determineExactIndex
            xyLoc.x=xyLoc.x * 0.9983; // is not as accurate as A1

            int x0=(int)(xyLoc.x);
            int x1=x0+1;
            int y0=(int)(xyLoc.y);
            int y1=y0+1;

            double p1=xyLoc.x-x0;
            double p2=1-p1;
            double p3=xyLoc.y-y0;
            double p4=1-p3;
            
            double value0 = rec.getFloatData()[(recNx * y0) + x0];
            double value1 = rec.getFloatData()[(recNx * y0) + x1];
            double value2 = rec.getFloatData()[(recNx * y1) + x0];
            double value3 = rec.getFloatData()[(recNx * y1) + x1];

            //do a bi-linear interpolation amount the nearest 4 points
            value = (p1*p4*value1)+(p2*p4*value0)+(p1*p3*value3)+(p2*p3*value2);
            logger.debug("bi-linear interpolation: "+value+"-->("+value0+","+value1+
            		","+value2+","+value3+") at "+xyLoc);
        } catch (Exception e) {
            logger.error("No Grib value available....." + modelName + " "
                    + prodType+" lat,lon:"+coor);
            return null;
        }
        return value;
    }

    /**
     * Give me the freezing level for an array of points
     * 
     * @param coors
     * @return
     */
    public HashMap<Coordinate, Float> getFreezingLevel(
            ArrayList<Coordinate> coors) {
        HashMap<Coordinate, Float> freezingMap = new HashMap<Coordinate, Float>();
        ScanDataCache cache = ScanDataCache.getInstance();

        //get data from hdf5 files
        for (Coordinate coor : coors) {

            HashMap<Integer, Double> ghValues = new HashMap<Integer, Double>();
            HashMap<Integer, Double> tValues = new HashMap<Integer, Double>();

            for (Entry<String, Integer> entry : getGHLevelMap().entrySet()) {
                if (cache.getModelData().isType(modelName, entry.getKey())) {
                    ghValues.put(entry.getValue(), getValue(modelName, entry.getKey(), coor));
                } else {
                	ghValues.put(entry.getValue(),null);
                }
            }

            for (Entry<String, Integer> entry : getTLevelMap().entrySet()) {
                if (cache.getModelData().isType(modelName, entry.getKey())) {
                    tValues.put(entry.getValue(), getValue(modelName, entry.getKey(), coor));
                }
            }
        
        //here's the calculation
            Double fLevel = 0.0;
            Integer jtopLevel = null;
            Integer ktopLevel = null;
            int foundValFlag=-1;//-1=all ghValue and tValue are null,
           //0=all ghValue<=-9000 and  tValue<=273.16, 1=found a fLevel

            TreeSet<Integer> ts= new TreeSet<Integer>(ghValues.keySet());//want an asc sorted list
            Iterator<Integer> it = ts.iterator();

            //for (Integer level : ghValues.keySet()) {
            while (it.hasNext()) {
            	Integer level = (Integer) it.next();

                Double tValue = tValues.get(level);
                Double ghValue = ghValues.get(level);

                if (ghValue != null && tValue != null && foundValFlag ==-1){
                	foundValFlag=0;
                }

                if (ghValue != null && ghValue.doubleValue() > -9000) {
                    if (tValue != null && tValue.doubleValue() > 273.16) {

                        fLevel = (ghValues.get(ktopLevel) - ((ghValues
                                .get(ktopLevel) - ghValue) * ((273.16 - tValues
                                .get(jtopLevel)) / (tValue - tValues
                                .get(jtopLevel))))) * .00328;
                        logger.debug("***Freezing level: "+fLevel+"="
                                + "(" + ghValues.get(ktopLevel)
                                + " - ((" + ghValues.get(ktopLevel) + " - "
                                + ghValue + ") * ((273.16 - "
                                + tValues.get(jtopLevel) + ") / (" + tValue
                                + " - " + tValues.get(jtopLevel)
                                + ")))) * .00328");
                        foundValFlag=1;
                        freezingMap.put(coor, fLevel.floatValue());
                        break;
                    } else {
                        jtopLevel = level;
                        ktopLevel = level;
                    }
                }
            }
            
            if (foundValFlag==0) {//this means all tValue are <= 273.16
            	freezingMap.put(coor, 0.0f);
            	logger.debug("*** FreezingLevel = 0.0");
            }
        }

        return freezingMap;
    }

    /**
     * Get the Level hash for GeoPotential Heights
     * 
     * @return
     */
    private LinkedHashMap<String, Integer> getGHLevelMap() {
        if (ghLevels == null) {
            ghLevels = new LinkedHashMap<String, Integer>();
            ghLevels.put("GH500", 500);
            ghLevels.put("GH525", 525);
            ghLevels.put("GH550", 550);
            ghLevels.put("GH575", 575);
            ghLevels.put("GH600", 600);
            ghLevels.put("GH625", 625);
            ghLevels.put("GH650", 650);
            ghLevels.put("GH675", 675);
            ghLevels.put("GH700", 700);
            ghLevels.put("GH725", 725);
            ghLevels.put("GH750", 750);
            ghLevels.put("GH775", 775);
            ghLevels.put("GH800", 800);
            ghLevels.put("GH825", 825);
            ghLevels.put("GH850", 850);
            ghLevels.put("GH875", 875);
            ghLevels.put("GH900", 900);
            ghLevels.put("GH925", 925);
            ghLevels.put("GH950", 950);
            ghLevels.put("GH975", 975);
            ghLevels.put("GH1000", 1000);
        }

        return ghLevels;
    }

    /**
     * Get the Level hash for Temp at Height
     * 
     * @return
     */
    private LinkedHashMap<String, Integer> getTLevelMap() {
        if (tLevels == null) {
            tLevels = new LinkedHashMap<String, Integer>();
            tLevels.put("T500", 500);
            tLevels.put("T525", 525);
            tLevels.put("T550", 550);
            tLevels.put("T575", 575);
            tLevels.put("T600", 600);
            tLevels.put("T625", 625);
            tLevels.put("T650", 650);
            tLevels.put("T675", 675);
            tLevels.put("T700", 700);
            tLevels.put("T725", 725);
            tLevels.put("T750", 750);
            tLevels.put("T775", 775);
            tLevels.put("T800", 800);
            tLevels.put("T825", 825);
            tLevels.put("T850", 850);
            tLevels.put("T875", 875);
            tLevels.put("T900", 900);
            tLevels.put("T925", 925);
            tLevels.put("T950", 950);
            tLevels.put("T975", 975);
            tLevels.put("T1000", 1000);
        }

        return tLevels;
    }

    /**
     * Populates the cache with records
     * 
     * @param model
     * @param param
     * @return
     */
    private GridRecord populateRecord(String model, String param, Date refTime) {
        int interval = 1440;

        SCANModelParameterXML paramXML = new SCANModelParameterXML();
        paramXML.setModelName(model);
        paramXML.setParameterName(param);
        String sql = getSQL(interval, model, param, refTime);
        logger.debug("Freezing level sql="+sql);
        GridRecord modelRec = DATUtils.getMostRecentGridRecord(interval, sql,
                paramXML);

        if (modelRec != null) {
            int fcHour = modelRec.getDataTime().getFcstTime() / 3600;
            addForecastTime(fcHour);
            times.put(fcHour, modelRec.getDataTime().getRefTime());
        }

        return modelRec;
    }

    /**
     * Used only by the MPE RUC for SHEF output
     * 
     * @param ft
     */
    private void addForecastTime(Integer ft) {
        if (ft >= 0 && ft <= 3) {
            zeroCount++;
        } else if (ft > 3 && ft <= 9) {
            sixCount++;
        } else if (ft > 9 && ft <= 15) {
            twelveCount++;
        } else if (ft > 15 && ft <= 23) {
            eighteenCount++;
        }
    }

    /**
     * Find the most prevelent time available. Checks for slow data updates
     * 
     * @return
     */
    public int getForecastHour() {
        Integer count = null;
        if ((zeroCount > sixCount) && (zeroCount > twelveCount)
                && (zeroCount > eighteenCount)) {
            count = 0;
        } else if ((sixCount > zeroCount) && (sixCount > twelveCount)
                && (sixCount > eighteenCount)) {
            count = 6;
        } else if ((twelveCount > zeroCount) && (twelveCount > sixCount)
                && (twelveCount > eighteenCount)) {
            count = 12;
        } else if ((eighteenCount > zeroCount) && (eighteenCount > sixCount)
                && (eighteenCount > twelveCount)) {
            count = 18;
        }
        return count;
    }

    /**
     * Gets the time
     * 
     * @param forecastHour
     * @return
     */
    public Date getReferenceTime(int forecastHour) {
        Date retDate = null;
        if (times != null) {
            for (Integer fh : times.keySet()) {
                if (fh == forecastHour) {
                    retDate = times.get(forecastHour);
                    break;
                }
            }
        }

        return retDate;
    }

    /**
     * Gets the times
     * 
     * @return
     */
    public HashMap<Integer, Date> getTimes() {
        return times;
    }

    /**
     * The SQL
     * 
     * @return
     */
    private String getSQL(int interval, String model, String param, Date refTime) {
        String paramName = null;
        String level = null;
        SimpleDateFormat sdt = new SimpleDateFormat("yyyy-MM-dd HH:00:00");

        String refTimeStr = sdt.format(refTime);
        if (param.startsWith("GH")) {
            paramName = "GH";
            level = param.substring(2, param.length());
        } else {
            paramName = "T";
            level = param.substring(1, param.length());
        }

        // Gets the most recent record of it's type
        String sql = "select grid.datauri from grid, grid_info, level where grid.info_id = grid_info.id and grid_info.level_id = level.id and grid_info.parameter_abbreviation = \'"
                + paramName
                + "\' and grid_info.datasetId = \'"
                + model
                + "\' and level.masterlevel_name = 'MB' and level.levelonevalue = '"
                + level + "\' and grid.reftime=\'" + refTimeStr + "\' and grid.forecasttime=0 order by grid.reftime desc limit 1";
        return sql;
    }
}
