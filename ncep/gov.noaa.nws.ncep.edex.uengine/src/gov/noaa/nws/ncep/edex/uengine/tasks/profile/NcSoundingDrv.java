package gov.noaa.nws.ncep.edex.uengine.tasks.profile;

/**
 * 
 * gov.noaa.nws.ncep.edex.uengine.tasks.profile.NcSoundingDrv
 * 
 * This java class performs sounding data query Python driver functions.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 09/13/2010	301			Chin Chen	Initial coding
 * 10/2010		301			T. Lee		Ad-hoc UAIR data retrievals
 * 11/05/2010   301         Chin Chen   fix some minor pointer checking issues
 * 11/29/2010   301         Chin Chen   add new APIs - query by lat/lon array, stn id array, stn number array
 * 12/2010		301			T. Lee/NCEP	Stn ID query case insensitive
 * 12/16/2010   301         Chin Chen   add support of BUFRUA observed sounding and PFC (NAM and GFS) model sounding data
 * May 2011     301         Chin Chen   add support of grib sounding data
 * June 2011    301         Chin Chen   add support of time range query
 * Sept 2011    457         S. Gurung   Renamed h5 to nc
 * Sep  2011    465           Archana     Added methods to use NcSoundingLayer2 
 * Oct 2011     465         Archana       Used the Amount class to set values for Met parameters
 * Noc 2011                 Chin Chen     changed Ncuair table query algorithm for performance improvement
 * 01/05/2012               S. Gurung     Removed references to UAIR (performed cleanup)
 * 01/05/2012               Chin Chen    fixed bug that cause exception when query NCUair with bad result 
 * 02/28/2012               Chin Chen   modify several sounding query algorithms for better performance
 * 03/28/2012               Chin Chen   modify Grid data sounding query algorithms for better performance
 * 06/25/2014               Chin Chen    support dropsonde
 * </pre>
 *  Python Script example to query multiple locations at one request:
 *  The following 3 query examples, returns same results.
 *  use lat/lon array
 import NcSoundingDataRequest
 sndRq = NcSoundingDataRequest.NcSoundingDataRequest()
 sndRq.setSndType('UAIR')
 sndRq.setRefTime(1290254400000L) 
 sndRq.setMerge(1)
 return sndRq.getSoundingDataBylaLonArray([[37.72999954223633,-122.20999908447266],[32.849998474121094,-117.11000061035156]])
 * use stnId array, use ref time string as input
 import NcSoundingDataRequest
 sndRq = NcSoundingDataRequest.NcSoundingDataRequest()
 sndRq.setSndType('UAIR')
 sndRq.setRefTimeStr('2010-11-20 12')
 sndRq.setMerge(1)
 return sndRq.getSoundingDataByStnIdArray(['OAK','NKX'])
 * use stn number array
 import NcSoundingDataRequest
 sndRq = NcSoundingDataRequest.NcSoundingDataRequest()
 sndRq.setSndType('UAIR')
 sndRq.setRefTime(1290254400000L)
 sndRq.setMerge(1)
 return sndRq.getSoundingDataByStnNumArray(['72293','72493'])


 import NcSoundingDataRequest
 sndRq = NcSoundingDataRequest.NcSoundingDataRequest()
 sndRq.setSndType('NAMSND')
 sndRq.setRefTime('2010-12-08 12:00')
 sndRq.setValidTime('2010-12-10 19:00')
 sndRq.setMerge(1)
 return sndRq.getSoundingDataByStnIdArray(['ATLH'])

 * 
 * 
 * 
 import NcSoundingDataRequest
 sndRq = NcSoundingDataRequest.NcSoundingDataRequest()
 sndRq.setSndType('NCUAIR')
 sndRq.setDataType('ALLDATA')
 sndRq.setRefTime(1320278400000L)
 sndRq.setValidTimeStart(1320278400000L)
 sndRq.setValidTimeEnd(1320278400000L)
 sndRq.setMerge(1)
 sndRq.setLevel('500')
 sndRq.getSoundingLayer2DataByLatLonArray([32.37,-64.68,47.47,-111.38,53.96,-101.09,60.03,-111.93,28.200000762939453,-87.5999984741211,28.63,-106.08,51.28,-80.59,36.23333,-86.55,34.77556,-76.87917,18.72,-110.95,53.29,-60.36,28.88,-118.3,55.03,-131.57,17.98,-92.92,19.3,-81.35,43.93,-60.01])
 *


 * 
 * 
 * 
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.common.dataplugin.ncuair.NcUairMaxWind;
import gov.noaa.nws.ncep.common.dataplugin.ncuair.NcUairObsLevels;
import gov.noaa.nws.ncep.common.dataplugin.ncuair.NcUairRecord;
import gov.noaa.nws.ncep.common.dataplugin.ncuair.NcUairTropopause;
import gov.noaa.nws.ncep.edex.common.metparameters.AirTemperature;
import gov.noaa.nws.ncep.edex.common.metparameters.Amount;
import gov.noaa.nws.ncep.edex.common.metparameters.DewPointTemp;
import gov.noaa.nws.ncep.edex.common.metparameters.HeightAboveSeaLevel;
import gov.noaa.nws.ncep.edex.common.metparameters.Omega;
import gov.noaa.nws.ncep.edex.common.metparameters.PressureLevel;
import gov.noaa.nws.ncep.edex.common.metparameters.WindDirection;
import gov.noaa.nws.ncep.edex.common.metparameters.WindSpeed;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.NcUnits;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer.DataType;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer2;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingModel;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile.MdlSndType;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile.ObsSndType;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile.PfcSndType;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile.SndQueryKeyType;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfoCollection;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.bufrua.UAObs;
import com.vividsolutions.jts.geom.Coordinate;

public class NcSoundingDrv {
    private static final UnitConverter metersPerSecondToKnots = SI.METERS_PER_SECOND
            .getConverterTo(NonSI.KNOT);

    private Double lat, lon;

    private long refTime = 0, validTimeStart = 0, validTimeEnd = 0;

    private String timeLine, refTimeStr, validTimeStartStr = null,
            validTimeEndStr = null;

    private Calendar refTimeCal, validTimeStartCal, validTimeEndCal;

    private String stid, level, dataType, sndType, queryType, tableName;

    private int merge;

    private boolean useNcSoundingLayer2 = false;

    private int[] dbIdList;

    private String[] stnIdArr;

    private String[] stnNumArr;

    private String modelName;

    private String pluginName;

    private double[][] latLonArray; // e.g. at nth element, lat=[n][0],
                                    // lon=[n][1]

    private Coordinate[] coordinateArray;

    private long[] rangeTimeArr;

    private List<Calendar> rangeTimeCalLst = new ArrayList<Calendar>();

    private List<String> rangeTimeStringLst = new ArrayList<String>();

    public double[][] getLatLons() {
        return latLonArray;
    }

    public boolean isUseNcSoundingLayer2() {
        return useNcSoundingLayer2;
    }

    public void setUseNcSoundingLayer2(boolean useNcSoundingLayer2) {
        this.useNcSoundingLayer2 = useNcSoundingLayer2;
        // System.out.println("useNcSoundingLayer2 set to "+useNcSoundingLayer2);
    }

    public long[] getRangeTimeArr() {
        return rangeTimeArr;
    }

    public void setRangeTimeArr(long[] rangeTimeArr) {
        this.rangeTimeArr = rangeTimeArr;
        rangeTimeStringLst.clear();
        rangeTimeCalLst.clear();
        for (int i = 0; i < rangeTimeArr.length; i++) {
            Calendar timeCal = Calendar
                    .getInstance(TimeZone.getTimeZone("GMT"));
            // reset time
            timeCal.setTimeInMillis(rangeTimeArr[i]);
            String tStr = String.format("%1$tY-%1$tm-%1$td %1$tH:00:00",
                    timeCal);
            this.rangeTimeStringLst.add(tStr);
            this.rangeTimeCalLst.add(timeCal);
            // System.out.println("setRangeTimeArr: time in long="+rangeTimeArr[i]+
            // " in str="+tStr);
        }
    }

    public String[] getStnIdArr() {
        return stnIdArr;
    }

    public void setStnIdArr(String[] stnIdArr) {
        this.stnIdArr = stnIdArr;
        // stnId and coordinateArray (or latLonArray) should be mutual exclusive
        coordinateArray = null;
        latLonArray = null;
    }

    public String[] getStnNumArr() {
        return stnNumArr;
    }

    public void setStnNumArr(String[] stnNumArr) {
        this.stnNumArr = stnNumArr;
    }

    public void setLatLons(double[] latLons) {
        // from python script, I only know a way to pass one dimensional array,
        // therefore convert it 2-D here.
        if (latLons.length > 0) {
            latLonArray = new double[latLons.length / 2][2];
            for (int i = 0, j = 0; i < latLons.length; i = i + 2) {
                this.latLonArray[j][0] = latLons[i];
                this.latLonArray[j][1] = latLons[i + 1];
                j++;
                // System.out.println("latlons = "+ latLonArray[j][0] + ","+
                // latLonArray[j][1]);
            }
            coordinateArray = new Coordinate[latLons.length / 2];
            for (int i = 0, j = 0; i < latLons.length; i = i + 2) {
                Coordinate coor = new Coordinate();
                coor.y = latLons[i]; // latitude
                coor.x = latLons[i + 1];// longitude
                this.coordinateArray[j] = coor;
                // System.out.println("i=" + i+ " j=" + j+" latlons = "+
                // latLons[i+1] + ","+ latLons[i]);
                j++;
            }
            stnIdArr = null; // stnId and coordinateArray (or latLonArray)
                             // should be mutual exclusive
        }
    }

    /** The logger */
    protected final transient Log logger = LogFactory.getLog(getClass());

    public String getRefTimeStr() {
        return refTimeStr;
    }

    /*
     * Reference time String should have this format: "yyyy-mm-dd hh"
     */
    public void setRefTimeStr(String refTimeStr) {
        this.refTimeStr = refTimeStr;
        refTimeCal = convertTimeStrToCalendar(refTimeStr);

    }

    public void setValidTimeStartStr(String validTimeStartStr) {
        this.validTimeStartStr = validTimeStartStr;
        validTimeStartCal = convertTimeStrToCalendar(validTimeStartStr);
    }

    public void setValidTimeEndStr(String validTimeEndStr) {
        this.validTimeEndStr = validTimeEndStr;
        validTimeEndCal = convertTimeStrToCalendar(validTimeEndStr);

    }

    private Calendar convertTimeStrToCalendar(String intimeStr) {
        int year, mon, date, hr;
        String timeStr = new String(intimeStr);
        int index = timeStr.indexOf('-');

        if (index >= 4) {
            year = Integer.parseInt(timeStr.substring(index - 4, index));
            timeStr = timeStr.substring(index + 1);
            index = timeStr.indexOf('-');
            if (index >= 2) {
                mon = Integer.parseInt(timeStr.substring(index - 2, index));
                timeStr = timeStr.substring(index + 1);
                index = timeStr.indexOf(' ');
                if (index >= 2) {
                    date = Integer
                            .parseInt(timeStr.substring(index - 2, index));
                    timeStr = timeStr.substring(index + 1);
                    // index = refTimeStr.indexOf(':');
                    if (timeStr.length() >= 2) {
                        hr = Integer.parseInt(timeStr.substring(0, 2));
                        Calendar cal;
                        cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
                        // reset time
                        cal.setTimeInMillis(0);
                        // set new time
                        cal.set(year, mon - 1, date, hr, 0, 0);
                        return cal;
                    }
                }
            }
        }
        return null;
    }

    public void setQueryType(String queryType) {
        this.queryType = queryType;
    }

    public String getTimeLine() {
        return timeLine;
    }

    public void setTimeLine(String timeLine) {
        this.timeLine = timeLine;
    }

    public NcSoundingDrv() {
        super();
        dbIdList = null;
        level = "-9999";
        merge = 0;
        queryType = "LATLON";
        dataType = "ALLDATA";
    }

    public String getTableName() {
        return tableName;
    }

    public void setTableName(String tableName) {
        this.tableName = tableName;

    }

    public String getDataType() {
        return dataType;
    }

    public void setDataType(String dataType) {
        this.dataType = dataType;
    }

    public int[] getDbIdList() {
        return dbIdList;
    }

    public void setDbIdList(int[] dbIdList) {
        this.dbIdList = dbIdList;
    }

    public double getLat() {
        return lat;
    }

    public void setLat(double lat) {
        this.lat = lat;
    }

    public void setLevel(String level) {
        this.level = level;
    }

    public void setStid(String stid) {
        this.stid = stid;
    }

    public double getLon() {
        return lon;
    }

    public String getValidTimeStr() {
        return validTimeStartStr;
    }

    public String getStid() {
        return stid;
    }

    public void setLon(double lon) {
        this.lon = lon;
    }

    public void setMerge(int merge) {
        // for native model sounding and model sounding, there is no need to
        // merge. Set merge
        // to false accordingly.
        this.merge = merge;
    }

    public long getRefTime() {
        return refTime;
    }

    public void setRefTime(long refTime) {
        this.refTime = refTime;
        Calendar timeCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        // reset time
        timeCal.setTimeInMillis(refTime);
        refTimeStr = String.format("%1$tY-%1$tm-%1$td %1$tH:00:00", timeCal);
        refTimeCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        refTimeCal.setTimeInMillis(refTime);
    }

    public long getValidTimeStart() {
        return validTimeStart;
    }

    public void setValidTimeStart(long validTimeStart) {
        this.validTimeStart = validTimeStart;
        validTimeStartCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        validTimeStartCal.setTimeInMillis(validTimeStart);
    }

    public long getValidTimeEnd() {
        return validTimeEnd;
    }

    public void setValidTimeEnd(long validTimeEnd) {
        this.validTimeEnd = validTimeEnd;
        validTimeEndCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        validTimeEndCal.setTimeInMillis(validTimeEnd);
    }

    public String getSndType() {
        return sndType;
    }

    public void setSndType(String sndType) {
        this.sndType = sndType;
    }

    boolean proces = true;

    public void setModelName(String aModelName) {
        this.modelName = aModelName;
    }

    public void setPluginName(String aPluginName) {
        this.pluginName = aPluginName;
    }

    public String getModelName() {
        return modelName;
    }

    public String getPluginName() {
        return pluginName;
    }

    // for static sounding type query
    public Object getSoundingRangeTimeLine() throws Exception {
        Object returnedObject = null;
        if (sndType.equals(ObsSndType.NCUAIR.toString())
                || sndType.equals(ObsSndType.DROP.toString())
                || sndType.equals(ObsSndType.TAMDAR.toString())) {

            // *System.out.println (
            // "getSoundingTimeLine Processing UAIR request.");
            returnedObject = ObservedSoundingQuery
                    .getObservedSndTimeLine(sndType);
        } else if (sndType.equals(PfcSndType.NAMSND.toString())
                || sndType.equals(PfcSndType.GFSSND.toString())) {
            returnedObject = PfcSoundingQuery.getPfcSndRangeTimeLine(sndType,
                    refTimeStr);
        }
        /*
         * else if (sndType.equals(MdlSndType.GFSSNDMDL.toString())||
         * sndType.equals(MdlSndType.NAMSNDMDL.toString()) ||
         * sndType.equals(MdlSndType.RUC2SNDMDL.toString())||
         * sndType.equals(MdlSndType.NGMSNDMDL.toString()) ||
         * sndType.equals(MdlSndType.UKMETSNDMDL.toString())) { returnedObject =
         * MdlSoundingQuery.getMdlSndRangeTimeLine(sndType, refTimeStr,
         * tableName); }
         */
        return returnedObject;
    }

    // for static sounding type query
    public Object getSoundingTimeLine() throws Exception {
        Object returnedObject = null;
        if (sndType.equals(ObsSndType.NCUAIR.toString())
                || sndType.equals(ObsSndType.BUFRUA.toString())
                || sndType.equals(ObsSndType.DROP.toString())
                || sndType.equals(ObsSndType.TAMDAR.toString())) {

            // *System.out.println (
            // "getSoundingTimeLine Processing UAIR request.");
            returnedObject = ObservedSoundingQuery
                    .getObservedSndTimeLine(sndType);
        } else if (sndType.equals(PfcSndType.NAMSND.toString())
                || sndType.equals(PfcSndType.GFSSND.toString())) {
            returnedObject = PfcSoundingQuery.getPfcSndTimeLine(sndType);

        } /*
           * else if (sndType.equals(MdlSndType.GFSSNDMDL.toString())||
           * sndType.equals(MdlSndType.NAMSNDMDL.toString()) ||
           * sndType.equals(MdlSndType.RUC2SNDMDL.toString())||
           * sndType.equals(MdlSndType.NGMSNDMDL.toString()) ||
           * sndType.equals(MdlSndType.UKMETSNDMDL.toString())) { returnedObject
           * = MdlSoundingQuery.getMdlSndTimeLine(sndType, tableName); }
           */

        return returnedObject;
    }

    // for model sounding query - its model type is returned during query time.
    public Object getMdlSoundingRangeTimeLine() throws Exception {
        Object returnedObject = null;
        returnedObject = MdlSoundingQuery.getMdlSndRangeTimeLine(sndType,
                refTimeStr, tableName);
        return returnedObject;
    }

    // for model sounding query - its model type is returned during query time.
    public Object getMdlSoundingTimeLine() throws Exception {
        Object returnedObject = null;

        returnedObject = MdlSoundingQuery.getMdlSndTimeLine(sndType, tableName);

        return returnedObject;
    }

    public Object getSoundingStnInfoCol() throws Exception {
        Object returnedObject = null;
        // System.out.println ( "getSoundingStnInfoCol sndType ="+sndType);
        NcSoundingStnInfoCollection stnInfoCol = null;
        if (sndType.equals(ObsSndType.NCUAIR.toString())
                || sndType.equals(ObsSndType.BUFRUA.toString())
                || sndType.equals(ObsSndType.DROP.toString())
                || sndType.equals(ObsSndType.TAMDAR.toString())) {
            stnInfoCol = ObservedSoundingQuery.getObservedSndStnInfoCol(
                    sndType, timeLine);
        } else if (sndType.equals(PfcSndType.NAMSND.toString())
                || sndType.equals(PfcSndType.GFSSND.toString())
                || sndType.equals(PfcSndType.RUCPTYPSND.toString())
                || sndType.equals(PfcSndType.RUC2SND.toString())) {
            stnInfoCol = PfcSoundingQuery.getPfcSndStnInfoCol(sndType,
                    timeLine, refTimeStr);
        }

        else
            return returnedObject;

        returnedObject = stnInfoCol;
        return returnedObject;
    }

    private List<NcSoundingLayer> getSndLayersFromNcUairRecordObsLevel(
            NcUairRecord record) {
        List<NcSoundingLayer> sndLayers = new ArrayList<NcSoundingLayer>();
        Set<NcUairObsLevels> obLevels = record.getObsLevels();
        // System.out.println("The datauri for this record is: " +
        // record.getDataURI() );
        if (obLevels.size() > 0) {
            for (NcUairObsLevels obLev : obLevels) {
                NcSoundingLayer sndLayer = new NcSoundingLayer();
                sndLayer.setTemperature(obLev.getTemp());
                sndLayer.setDewpoint(obLev.getDwpt());
                sndLayer.setGeoHeight(obLev.getHght());
                // System.out.println("Sounding layer height =  " +
                // sndLayer.getGeoHeight() );
                sndLayer.setPressure(obLev.getPres());
                sndLayer.setWindDirection(obLev.getDrct());
                if (obLev.getSped() >= 0)
                    sndLayer.setWindSpeed((float) metersPerSecondToKnots
                            .convert(obLev.getSped()));
                else
                    sndLayer.setWindSpeed(obLev.getSped());
                sndLayers.add(sndLayer);
            }
        }
        // System.out.println("ObsLevel="+obLevels.size()+" sndLayers="+sndLayers.size());
        return sndLayers;
    }

    private List<NcSoundingLayer2> getSoundingLayer2FromNcUairRecordObsLevel(
            NcUairRecord record) {
        List<NcSoundingLayer2> sndLayers = new ArrayList<NcSoundingLayer2>();
        Set<NcUairObsLevels> obLevels = record.getObsLevels();

        // System.out.println("The datatype for this record is: " +
        // record.getDataType() );
        if (obLevels.size() > 0) {
            for (NcUairObsLevels obLev : obLevels) {
                // System.out.println("\n\nFrom NcUairObsLevel:");
                // System.out.println("Temperature = " + obLev.getTemp());
                // System.out.println("Pressure = " + obLev.getPres());
                // System.out.println("Dewpoint = " + obLev.getDwpt());
                // System.out.println("Height = " + obLev.getHght());
                // System.out.println("Wind direction = " + obLev.getDrct());
                // System.out.println("Wind speed in m/s= " + obLev.getSped());
                try {
                    NcSoundingLayer2 sndLayer = new NcSoundingLayer2();
                    /*
                     * (Non-Javadoc) The units for each quantity are chosen
                     * based upon the units defined for these quantities in the
                     * pointdata description file for NcUair
                     */
                    AirTemperature airTemp = new AirTemperature();
                    airTemp.setValue(new Amount(obLev.getTemp(), SI.CELSIUS));
                    DewPointTemp dewPoint = new DewPointTemp();
                    dewPoint.setValue(new Amount(obLev.getDwpt(), SI.CELSIUS));
                    HeightAboveSeaLevel height = new HeightAboveSeaLevel();
                    height.setValue(obLev.getHght(), SI.METER);

                    // System.out.println("Sounding layer height =  " +
                    // sndLayer.getGeoHeight().doubleValue() );
                    PressureLevel pressure = new PressureLevel();
                    pressure.setValue(new Amount(obLev.getPres(),
                            NcUnits.MILLIBAR));

                    WindDirection windDirection = new WindDirection();
                    windDirection.setValue(obLev.getDrct(), NonSI.DEGREE_ANGLE);
                    WindSpeed windSpeed = new WindSpeed();
                    float speed = obLev.getSped();

                    /*
                     * ( Non-Javadoc ) There are no negative speed values
                     * decoded except for either -999 or -9999 to indicate that
                     * the speed is missing. The check for the positive speed
                     * value ensures that the unit conversion happens for
                     * non-missing speed values.
                     */
                    if (speed >= 0) {
                        double convertedSpeed = metersPerSecondToKnots
                                .convert(speed);
                        windSpeed.setValue(convertedSpeed, NonSI.KNOT);
                    } else {
                        windSpeed.setValueToMissing();
                    }

                    // System.out.println("\nFrom MetParameters:");
                    // System.out.println("Temperature = " +
                    // airTemp.getValue().floatValue());
                    // System.out.println("Pressure = " +
                    // pressure.getValue().floatValue());
                    // System.out.println("Dewpoint = " +
                    // dewPoint.getValue().floatValue());
                    // System.out.println("Height = " +
                    // height.getValue().floatValue());
                    // System.out.println("Wind direction = " +
                    // windDirection.getValue().floatValue());
                    // System.out.println("Wind speed = " +
                    // windSpeed.getValue().floatValue());
                    sndLayer.setTemperature(airTemp);
                    sndLayer.setPressure(pressure);
                    sndLayer.setDewpoint(dewPoint);
                    sndLayer.setGeoHeight(height);
                    sndLayer.setWindDirection(windDirection);
                    sndLayer.setWindSpeed(windSpeed);
                    sndLayers.add(sndLayer);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
        // System.out.println("ObsLevel="+obLevels.size()+" sndLayers="+sndLayers.size());
        return sndLayers;
    }

    private List<NcSoundingLayer> getSndLayersFromNcUairRecordTrop(
            NcUairRecord record) {
        List<NcSoundingLayer> sndLayers = new ArrayList<NcSoundingLayer>();
        Set<NcUairTropopause> trops = record.getTropopause();
        if (trops.size() > 0) {
            for (NcUairTropopause trop : trops) {
                NcSoundingLayer sndLayer = new NcSoundingLayer();
                sndLayer.setTemperature(trop.getTemp());
                sndLayer.setDewpoint(trop.getDwpt());
                sndLayer.setPressure(trop.getPres());
                sndLayer.setWindDirection(trop.getDrct());
                if (trop.getSped() >= 0)
                    sndLayer.setWindSpeed((float) metersPerSecondToKnots
                            .convert(trop.getSped()));
                else
                    sndLayer.setWindSpeed(trop.getSped());
                sndLayers.add(sndLayer);
            }
        }
        // System.out.println("trops="+trops.size()+" sndLayers="+sndLayers.size());
        return sndLayers;
    }

    private List<NcSoundingLayer2> getSoundingLayer2FromNcUairRecordTrop(
            NcUairRecord record) {
        List<NcSoundingLayer2> sndLayers = new ArrayList<NcSoundingLayer2>();
        Set<NcUairTropopause> trops = record.getTropopause();
        if (trops.size() > 0) {
            for (NcUairTropopause trop : trops) {
                try {
                    NcSoundingLayer2 sndLayer = new NcSoundingLayer2();
                    /*
                     * (Non-Javadoc) The units for each quantity are chosen
                     * based upon the units defined for these quantities in the
                     * pointdata description file for NcUair
                     */
                    AirTemperature airTemp = new AirTemperature();
                    airTemp.setValue(new Amount(trop.getTemp(), SI.CELSIUS));
                    DewPointTemp dewPoint = new DewPointTemp();
                    dewPoint.setValue(new Amount(trop.getDwpt(), SI.CELSIUS));
                    PressureLevel pressure = new PressureLevel();
                    pressure.setValue(new Amount(trop.getPres(),
                            NcUnits.MILLIBAR));
                    WindDirection windDirection = new WindDirection();
                    windDirection.setValue(trop.getDrct(), NonSI.DEGREE_ANGLE);
                    WindSpeed windSpeed = new WindSpeed();
                    float speed = trop.getSped();
                    /*
                     * ( Non-Javadoc ) There are no negative speed values
                     * decoded except for either -999 or -9999 to indicate that
                     * the speed is missing. The check for the positive speed
                     * value ensures that the unit conversion happens for
                     * non-missing speed values.
                     */
                    if (speed >= 0) {
                        double convertedSpeed = metersPerSecondToKnots
                                .convert(speed);
                        windSpeed.setValue(convertedSpeed, NonSI.KNOT);
                    } else {
                        windSpeed.setValueToMissing();
                    }
                    sndLayer.setTemperature(airTemp);
                    sndLayer.setPressure(pressure);
                    sndLayer.setDewpoint(dewPoint);
                    sndLayer.setWindDirection(windDirection);
                    sndLayer.setWindSpeed(windSpeed);
                    sndLayers.add(sndLayer);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
        // System.out.println("trops="+trops.size()+" sndLayers="+sndLayers.size());
        return sndLayers;
    }

    private List<NcSoundingLayer> getSndLayersFromNcUairRecordMaxw(
            NcUairRecord record) {
        List<NcSoundingLayer> sndLayers = new ArrayList<NcSoundingLayer>();
        Set<NcUairMaxWind> maxWinds = record.getMaxWind();
        if (maxWinds.size() > 0) {
            for (NcUairMaxWind maxWind : maxWinds) {
                NcSoundingLayer sndLayer = new NcSoundingLayer();
                sndLayer.setPressure(maxWind.getPres());
                sndLayer.setWindDirection(maxWind.getDrct());
                if (maxWind.getSped() >= 0)
                    sndLayer.setWindSpeed((float) metersPerSecondToKnots
                            .convert(maxWind.getSped()));
                else
                    sndLayer.setWindSpeed(maxWind.getSped());
                sndLayers.add(sndLayer);
            }
        }
        // System.out.println("maxWinds="+maxWinds.size()+" sndLayers="+sndLayers.size());
        return sndLayers;
    }

    private List<NcSoundingLayer2> getSoundingLayer2FromNcUairRecordMaxw(
            NcUairRecord record) {
        List<NcSoundingLayer2> sndLayers = new ArrayList<NcSoundingLayer2>();
        Set<NcUairMaxWind> maxWinds = record.getMaxWind();
        if (maxWinds.size() > 0) {
            /*
             * (Non-Javadoc) The units for each quantity are chosen based upon
             * the units defined for these quantities in the pointdata
             * description file for NcUair
             */
            for (NcUairMaxWind maxWind : maxWinds) {
                try {
                    NcSoundingLayer2 sndLayer = new NcSoundingLayer2();
                    PressureLevel pressure = new PressureLevel();
                    // pressure.setValueAs(maxWind.getPres(), "hPa" );
                    pressure.setValue(new Amount(maxWind.getPres(),
                            NcUnits.MILLIBAR));
                    WindDirection windDirection = new WindDirection();
                    windDirection.setValue(maxWind.getDrct(),
                            NonSI.DEGREE_ANGLE);
                    WindSpeed windSpeed = new WindSpeed();
                    float speed = maxWind.getSped();
                    /*
                     * ( Non-Javadoc ) There are no negative speed values
                     * decoded except for either -999 or -9999 to indicate that
                     * the speed is missing. The check for the positive speed
                     * value ensures that the unit conversion happens for
                     * non-missing speed values.
                     */
                    if (speed >= 0) {
                        double convertedSpeed = metersPerSecondToKnots
                                .convert(speed);
                        windSpeed.setValue(convertedSpeed, NonSI.KNOT);
                    } else {
                        windSpeed.setValueToMissing();
                    }
                    sndLayer.setPressure(pressure);
                    sndLayer.setWindDirection(windDirection);
                    sndLayer.setWindSpeed(windSpeed);
                    sndLayers.add(sndLayer);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
        // System.out.println("maxWinds="+maxWinds.size()+" sndLayers="+sndLayers.size());
        return sndLayers;
    }

    /*
     * CHin: 2012 Feb 13: Support PFC sounding with one lat/lon location
     * only..... Query PFC sounding data in one shot. to see performce
     * difference
     * 
     * public Object getSoundingDataByRangeTimeArray() throws Exception { //long
     * t01 = System.currentTimeMillis(); Object returnedObject = new Object();
     * //System.out.println ( " getSoundingDataByLatLonArray ");
     * 
     * 
     * //MergeSounding ms = new MergeSounding(); NcSoundingCube cube = new
     * NcSoundingCube(); //List<NcSoundingProfile> soundingProfileList = new
     * ArrayList<NcSoundingProfile>(); NcSoundingCube.QueryStatus
     * failedRtnStatus = NcSoundingCube.QueryStatus.FAILED; Coordinate[]
     * coorArray = new Coordinate[1]; Coordinate latLon = new Coordinate();
     * latLon.y= lat; latLon.x= lon; coorArray[0]=latLon;
     * //List<NcSoundingProfile>
     * pfs2=PfcSoundingQuery.getPfcSndDataBySoundTimeRangeArray(lat, lon, null,
     * refTimeStr, rangeTimeStringLst, sndType, SndQueryKeyType.LATLON);
     * List<NcSoundingProfile>
     * pfs=PfcSoundingQuery.getPfcSndDataGeneric(coorArray, null, refTimeStr,
     * rangeTimeStringLst, sndType,level);
     * 
     * if(pfs.size() == 0 ) cube.setRtnStatus(failedRtnStatus); else
     * cube.setRtnStatus(NcSoundingCube.QueryStatus.OK);
     * 
     * cube.setSoundingProfileList(pfs); returnedObject = cube;
     * 
     * //long t02 = System.currentTimeMillis();
     * //System.out.println("PFC/OBS cube retreival took " + (t02 - t01));
     * return returnedObject; }
     */
    /*
     * This API is for getting multiple locations sounding info at one shot.
     * latLonArray is used to input lat/lon for each location.
     * 
     * Chin's Note @ 02/27/2012 This API is currently only used for query grid
     * and bufrua data by Nsharp.
     */
    private Object getSoundingDataByLatLonArray() throws Exception {
        // long t01 = System.currentTimeMillis();
        Object returnedObject = new Object();
        // System.out.println ( " getSoundingDataByLatLonArray entered");

        List<String> timeLineStrList = new ArrayList<String>();
        List<Calendar> timeLimeCalList = new ArrayList<Calendar>();

        NcSoundingCube cube = new NcSoundingCube();
        List<NcSoundingProfile> soundingProfileList = new ArrayList<NcSoundingProfile>();
        SndQueryKeyType sndQuery = SndQueryKeyType.LATLON;
        NcSoundingCube.QueryStatus failedRtnStatus = NcSoundingCube.QueryStatus.FAILED;
        MergeSounding ms = new MergeSounding();
        NcSoundingProfile pf = null;
        if (sndType.equals(MdlSndType.ANY.toString())) {
            // temp fix for now...
            if (validTimeEnd != 0 && validTimeEnd > validTimeStart) {
                // range of time line request
                timeLimeCalList = ObservedSoundingQuery
                        .getObservedSndTimeRangeList(sndType,
                                validTimeStartCal, validTimeEndCal);
                for (int i = 0; i < timeLimeCalList.size(); i++) {
                    Calendar timeCal = timeLimeCalList.get(i);
                    String timeStr = String.format(
                            "%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS", timeCal);
                    timeLineStrList.add(timeStr);

                }
            } else {
                // one single time line
                timeLineStrList.add(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
                        .format(refTimeCal.getTime()));
                timeLimeCalList.add(refTimeCal);
            }
            if (latLonArray != null) {
                /*
                 * old way for ( int i=0; i < latLonArray.length ; i++) { double
                 * lat = latLonArray[i][0]; double lon = latLonArray[i][1]; for
                 * (int j=0; j< timeLineStrList.size(); j++){ String timeStr =
                 * timeLineStrList.get(j); pf = MdlSoundingQuery.getMdlSndData(
                 * lat, lon, timeStr, validTimeStartStr, pluginName, modelName
                 * ); if(pf.getRtnStatus() != NcSoundingCube.QueryStatus.OK){
                 * failedRtnStatus = pf.getRtnStatus(); pf = null; } if(pf !=
                 * null && pf.getSoundingLyLst().size()>0) {
                 * soundingProfileList.add(pf); pf = null; } } }
                 */
                // Chin Note: using new API to query multiple Points at one shot
                soundingProfileList = MdlSoundingQuery
                        .getMdlSndDataProfileList(latLonArray,
                                timeLineStrList.get(0), validTimeStartStr,
                                pluginName, modelName);
            }
        } else if (sndType.equals(ObsSndType.BUFRUA.toString())) {
            int arrLen = 0;
            if (latLonArray != null) {
                arrLen = latLonArray.length;
                sndQuery = SndQueryKeyType.LATLON;
            } else if (stnIdArr != null) {
                arrLen = stnIdArr.length;
                sndQuery = SndQueryKeyType.STNID;
            }
            double lat = 0, lon = 0;
            String stnId = "";
            for (int i = 0; i < arrLen; i++) {

                // Calendar timeCal = timeLimeCalList.get(j);

                if (latLonArray != null) {
                    // make sure we have right precision...
                    lat = latLonArray[i][0];
                    lon = latLonArray[i][1];
                } else {
                    stnId = stnIdArr[i];
                }
                /*
                 * Process sounding data.
                 */

                List<NcSoundingLayer> sls = new ArrayList<NcSoundingLayer>();
                List<NcSoundingLayer> ttaa = new ArrayList<NcSoundingLayer>();
                List<NcSoundingLayer> ttbb = new ArrayList<NcSoundingLayer>();
                List<NcSoundingLayer> ttcc = new ArrayList<NcSoundingLayer>();
                List<NcSoundingLayer> ttdd = new ArrayList<NcSoundingLayer>();
                List<NcSoundingLayer> ppaa = new ArrayList<NcSoundingLayer>();
                List<NcSoundingLayer> ppbb = new ArrayList<NcSoundingLayer>();
                List<NcSoundingLayer> ppcc = new ArrayList<NcSoundingLayer>();
                List<NcSoundingLayer> ppdd = new ArrayList<NcSoundingLayer>();
                List<NcSoundingLayer> trop_a = new ArrayList<NcSoundingLayer>();
                List<NcSoundingLayer> trop_c = new ArrayList<NcSoundingLayer>();
                List<NcSoundingLayer> wmax_a = new ArrayList<NcSoundingLayer>();
                List<NcSoundingLayer> wmax_c = new ArrayList<NcSoundingLayer>();
                for (Calendar timeCal : rangeTimeCalLst) {
                    if (merge == 0) {
                        // ms.unMergedUairSounding
                        // *System.out.println ( " Request unmerged data");
                        if (dataType.equals(DataType.ALLDATA.toString()))
                            pf = ObservedSoundingQuery
                                    .getObservedSndBufruaAllData(lat, lon,
                                            stnId, timeCal, sndQuery);
                        else
                            pf = ObservedSoundingQuery
                                    .getObservedSndBufruaData(lat, lon, stnId,
                                            timeCal, dataType, sndQuery);

                    } else {

                        // Get TTAA. If not existent, try ship data (UUAA). If
                        // level is not null or missing,
                        // the body of code will return a sounding list with MAN
                        // data or single level data.
                        // *System.out.println ( " Request merged data at lat="+
                        // lat+" lon="+lon+ " refT="+
                        // timeCal.getTime().toGMTString());

                        ttaa = ObservedSoundingQuery.getObservedSndBufruaData(
                                lat, lon, stnId, timeCal, "TTAA", sndQuery)
                                .getSoundingLyLst();
                        ttbb = ObservedSoundingQuery.getObservedSndBufruaData(
                                lat, lon, stnId, timeCal, "TTBB", sndQuery)
                                .getSoundingLyLst();
                        ttcc = ObservedSoundingQuery.getObservedSndBufruaData(
                                lat, lon, stnId, timeCal, "TTCC", sndQuery)
                                .getSoundingLyLst();
                        ttdd = ObservedSoundingQuery.getObservedSndBufruaData(
                                lat, lon, stnId, timeCal, "TTDD", sndQuery)
                                .getSoundingLyLst();
                        // ppaa =
                        // ObservedSoundingQuery.getObservedSndBufruaData(lat,
                        // lon, stnId, timeCal, "PPAA",
                        // sndQuery).getSoundingLyLst();
                        ppbb = ObservedSoundingQuery.getObservedSndBufruaData(
                                lat, lon, stnId, timeCal, "PPBB", sndQuery)
                                .getSoundingLyLst();
                        // ppcc =
                        // ObservedSoundingQuery.getObservedSndBufruaData(lat,
                        // lon, stnId, timeCal, "PPCC",
                        // sndQuery).getSoundingLyLst();
                        ppdd = ObservedSoundingQuery.getObservedSndBufruaData(
                                lat, lon, stnId, timeCal, "PPDD", sndQuery)
                                .getSoundingLyLst();
                        wmax_a = ObservedSoundingQuery
                                .getObservedSndBufruaData(lat, lon, stnId,
                                        timeCal, "MAXWIND_A", sndQuery)
                                .getSoundingLyLst();
                        wmax_c = ObservedSoundingQuery
                                .getObservedSndBufruaData(lat, lon, stnId,
                                        timeCal, "MAXWIND_C", sndQuery)
                                .getSoundingLyLst();
                        trop_a = ObservedSoundingQuery
                                .getObservedSndBufruaData(lat, lon, stnId,
                                        timeCal, "TROPOPAUSE_A", sndQuery)
                                .getSoundingLyLst();
                        trop_c = ObservedSoundingQuery
                                .getObservedSndBufruaData(lat, lon, stnId,
                                        timeCal, "TROPOPAUSE_C", sndQuery)
                                .getSoundingLyLst();
                        pf = ObservedSoundingQuery.getObservedSndStnInfo(lat,
                                lon, stnId, sndType, timeCal, sndQuery);
                        sls = ms.mergeUairSounding(level, ttaa, ttbb, ttcc,
                                ttdd, ppaa, ppbb, ppcc, ppdd, trop_a, trop_c,
                                wmax_a, wmax_c, pf.getStationElevation());

                        // System.out.println("BUFRUA Number of Layers after merge:"+sls.size()
                        // + " level="+level +
                        // " ms.isNumber(level)="+ms.isNumber(level));
                        // for(NcSoundingLayer ly: sls){
                        // System.out.println("Pre= "+ly.getPressure()+
                        // " Dew= "+ ly.getDewpoint()+ " T= "+
                        // ly.getTemperature()+" H="+ly.getGeoHeight()+" WSp="+ly.getWindSpeed());
                        // }

                        if (level.toUpperCase().equalsIgnoreCase("MAN"))
                            pf.setSoundingLyLst(sls);
                        else if (ms.isNumber(level) >= 0) {
                            if (sls.size() == 1) {
                                // System.out.println("NCUAIR get one layer using level = "+
                                // level);
                                pf.setSoundingLyLst(sls);
                            } else {
                                pf = null;
                                // System.out.println("NCUAIR get 0 layer using level = "+
                                // level);
                            }
                        } else {
                            if (sls.isEmpty() || sls.size() <= 1)
                                pf = null;
                            else
                                pf.setSoundingLyLst(sls);
                        }

                    }
                    if (pf != null && pf.getSoundingLyLst().size() > 0) {
                        soundingProfileList.add(pf);
                        pf = null;
                    }
                }
            }
        }

        if (soundingProfileList.size() == 0)
            cube.setRtnStatus(failedRtnStatus);
        else
            cube.setRtnStatus(NcSoundingCube.QueryStatus.OK);

        cube.setSoundingProfileList(soundingProfileList);
        returnedObject = cube;

        // long t02 = System.currentTimeMillis();
        // System.out.println("PFC/OBS cube retreival took " + (t02 - t01));
        return returnedObject;
    }

    /*
     * Chin:: 2/21/2012 Only NCUair and PFC (modelsounding DB) query is
     * supported now Use generic query API Mainly used by CAVE Resource plotting
     * Returned Sounding data are stored in NcSoundingLayer2.
     */
    public Object getSoundingData2Generic() throws Exception {
        long t01 = System.currentTimeMillis();
        Object returnedObject = new Object();
        // *System.out.println ( " getSoundingDataByLatLonArray ");
        if (stnIdArr.length <= 0) {
            returnedObject = null;
            return returnedObject;
        }
        // List<String> timeLineStrList=new ArrayList<String>();;
        // List<Calendar> timeLimeCalList = new ArrayList<Calendar>();

        NcSoundingCube cube = new NcSoundingCube();
        List<NcSoundingProfile> soundingProfileList = new ArrayList<NcSoundingProfile>(
                0);
        NcSoundingCube.QueryStatus failedRtnStatus = NcSoundingCube.QueryStatus.FAILED;

        NcSoundingProfile pf;
        MergeSounding2 ms = new MergeSounding2();
        if (sndType.equals(ObsSndType.NCUAIR.toString())) {
            // Note: in NC Uair table, we only use ref time for query
            if (rangeTimeStringLst != null && rangeTimeStringLst.size() == 0) {
                // one single time line
                rangeTimeStringLst.add(refTimeStr);
            }
            // for (int j=0; j< rangeTimeStringLst.size(); j++){
            // String timeStr = rangeTimeStringLst.get(j);
            List<NcUairRecord[]> uairRecordArrList;
            long t003 = System.currentTimeMillis();
            uairRecordArrList = ObservedSoundingQuery
                    .getObservedSndNcUairDataGeneric(coordinateArray, stnIdArr,
                            rangeTimeStringLst, rangeTimeArr);
            long t004 = System.currentTimeMillis();
            System.out.println("getObservedSndNcUairDataGeneric query took "
                    + (t004 - t003) + "ms");
            if (uairRecordArrList != null && uairRecordArrList.size() > 0) {
                long t005 = System.currentTimeMillis();
                // for each station, processing its records list and keep in one
                // profile
                for (NcUairRecord[] recordArray : uairRecordArrList) {

                    // System.out.println("getSoundingDataByLatLonArray:NcUair: lat="+lat+
                    // " lon="+lon);
                    // make sure we have right precision...

                    if (merge == 0) {
                        // Chin...need more coding
                        pf = null;
                    } else {
                        pf = new NcSoundingProfile();
                        /*
                         * Chin: If caller need all query stns returned (no
                         * matter there is no data), then we may need to add
                         * this code... if(recordArray != null &&
                         * recordArray.length == 1){ //could be a special case
                         * that the record is just used to return this stn's
                         * lat/lon back if(recordArray[0].getNil() == true){
                         * pf.setStationLatitude
                         * ((float)recordArray[0].getLatitude());
                         * pf.setStationLongitude
                         * ((float)recordArray[0].getLongitude());
                         * soundingProfileList.add(pf); continue; // skip this
                         * stn as there are no real data retrieved } }
                         */
                        if (recordArray != null && recordArray.length > 0) {
                            List<NcSoundingLayer2> sls = new ArrayList<NcSoundingLayer2>();
                            List<NcSoundingLayer2> ttaa = new ArrayList<NcSoundingLayer2>();
                            List<NcSoundingLayer2> ttbb = new ArrayList<NcSoundingLayer2>();
                            List<NcSoundingLayer2> ttcc = new ArrayList<NcSoundingLayer2>();
                            List<NcSoundingLayer2> ttdd = new ArrayList<NcSoundingLayer2>();
                            List<NcSoundingLayer2> ppaa = new ArrayList<NcSoundingLayer2>();
                            List<NcSoundingLayer2> ppbb = new ArrayList<NcSoundingLayer2>();
                            List<NcSoundingLayer2> ppcc = new ArrayList<NcSoundingLayer2>();
                            List<NcSoundingLayer2> ppdd = new ArrayList<NcSoundingLayer2>();
                            List<NcSoundingLayer2> trop_a = new ArrayList<NcSoundingLayer2>();
                            List<NcSoundingLayer2> trop_c = new ArrayList<NcSoundingLayer2>();
                            List<NcSoundingLayer2> wmax_a = new ArrayList<NcSoundingLayer2>();
                            List<NcSoundingLayer2> wmax_c = new ArrayList<NcSoundingLayer2>();

                            for (int k = 0; k < recordArray.length; k++) {
                                NcUairRecord record = recordArray[k];
                                if (record.getDataType().equals("TTAA")
                                        || record.getDataType().equals("XXAA")) {
                                    ttaa = getSoundingLayer2FromNcUairRecordObsLevel(record);
                                    trop_a = getSoundingLayer2FromNcUairRecordTrop(record);
                                    wmax_a = getSoundingLayer2FromNcUairRecordMaxw(record);
                                } else if (record.getDataType().equals("TTBB")
                                        || record.getDataType().equals("XXBB")) {
                                    ttbb = getSoundingLayer2FromNcUairRecordObsLevel(record);
                                } else if (record.getDataType().equals("TTCC")
                                        || record.getDataType().equals("XXCC")) {
                                    ttcc = getSoundingLayer2FromNcUairRecordObsLevel(record);
                                    trop_c = getSoundingLayer2FromNcUairRecordTrop(record);
                                    wmax_c = getSoundingLayer2FromNcUairRecordMaxw(record);
                                } else if (record.getDataType().equals("TTDD")
                                        || record.getDataType().equals("XXDD")) {
                                    ttdd = getSoundingLayer2FromNcUairRecordObsLevel(record);
                                } else if (record.getDataType().equals("PPAA")) {
                                    ppaa = getSoundingLayer2FromNcUairRecordObsLevel(record);
                                } else if (record.getDataType().equals("PPBB")) {
                                    ppbb = getSoundingLayer2FromNcUairRecordObsLevel(record);
                                } else if (record.getDataType().equals("PPCC")) {
                                    ppcc = getSoundingLayer2FromNcUairRecordObsLevel(record);
                                } else if (record.getDataType().equals("PPDD")) {
                                    ppdd = getSoundingLayer2FromNcUairRecordObsLevel(record);
                                }
                            }
                            pf.setStationElevation((float) recordArray[0]
                                    .getElevation());
                            pf.setStationId(recordArray[0].getStationId());
                            if (recordArray[0].getStnum() != null
                                    && recordArray[0].getStnum().length() > 0)
                                pf.setStationNum(Integer
                                        .parseInt(recordArray[0].getStnum()));
                            pf.setStationLatitude(recordArray[0].getLatitude());
                            pf.setStationLongitude(recordArray[0]
                                    .getLongitude());
                            // System.out.println(" input lat="+lat+" pf's lat="+pf.getStationLatitude()+" elv="+pf.getStationElevation()+" stnId="+pf.getStationId());
                            sls = ms.mergeUairSounding(level, ttaa, ttbb, ttcc,
                                    ttdd, ppaa, ppbb, ppcc, ppdd, trop_a,
                                    trop_c, wmax_a, wmax_c,
                                    pf.getStationElevation());

                            if (level.toUpperCase().equalsIgnoreCase("MAN")) {
                                pf.setSoundingLyLst2(sls);
                                // System.out.println("sls set to the sounding profile");
                            } else if (ms.isNumber(level) >= 0) {
                                if (sls.size() == 1) {
                                    // System.out.println("NcUair get one layer using level = "+
                                    // level);
                                    pf.setSoundingLyLst2(sls);
                                } else {
                                    pf = null;
                                    // System.out.println("NcUair get 0 layer using level = "+
                                    // level);
                                }
                            } else {
                                if (sls.isEmpty() || sls.size() <= 1) {
                                    pf = null;
                                    // System.out.println("not MAN level &  sls is empty or 1");
                                } else {
                                    pf.setSoundingLyLst2(sls);
                                    // System.out.println("sls set to the sounding profile for level = "
                                    // + level);
                                }
                            }
                        }
                    }
                    if (pf != null && pf.getSoundingLyLst2().size() > 0) {
                        // System.out.println(" pf is not null, so adding a profile to the list of NcSoundingProfiles ");
                        soundingProfileList.add(pf);
                        pf = null;
                    }

                }
                long t006 = System.currentTimeMillis();
                System.out
                        .println("getSoundingData2Generic total sounding time merging for "
                                + uairRecordArrList.size()
                                + " profiles took "
                                + (t006 - t005) + "ms");
            }
            // }
        } else if (sndType.equals(PfcSndType.NAMSND.toString())
                || sndType.equals(PfcSndType.GFSSND.toString())
                || sndType.equals(PfcSndType.RUCPTYPSND.toString())
                || sndType.equals(PfcSndType.RUC2SND.toString())) {

            List<NcSoundingProfile> listReturned = PfcSoundingQuery
                    .getPfcSndDataGeneric(null, stnIdArr, refTimeStr,
                            rangeTimeStringLst, sndType, level);
            soundingProfileList.addAll(listReturned);
            convertPfcNcSoundingLayerToNcSoundingLayer2(soundingProfileList);
            // sysPrintProfileLayer2(soundingProfileList);

        }
        /*
         * else if(sndType.equals(MdlSndType.ANY.toString()) ) {
         * 
         * pf = MdlSoundingQuery.getMdlSndData( lat, lon, refTimeStr,
         * validTimeStartStr, pluginName, modelName ); if(pf.getRtnStatus() !=
         * NcSoundingCube.QueryStatus.OK){ failedRtnStatus = pf.getRtnStatus();
         * pf = null; } }
         */
        /*
         * else if(sndType.equals(ObsSndType.BUFRUA.toString())){
         * List<NcSoundingLayer2> sls = new ArrayList<NcSoundingLayer2>();
         * List<NcSoundingLayer2> ttaa = new ArrayList<NcSoundingLayer2>();
         * List<NcSoundingLayer2> ttbb = new ArrayList<NcSoundingLayer2>();
         * List<NcSoundingLayer2> ttcc = new ArrayList<NcSoundingLayer2>();
         * List<NcSoundingLayer2> ttdd = new ArrayList<NcSoundingLayer2>();
         * List<NcSoundingLayer2> ppaa = new ArrayList<NcSoundingLayer2>();
         * List<NcSoundingLayer2> ppbb = new ArrayList<NcSoundingLayer2>();
         * List<NcSoundingLayer2> ppcc = new ArrayList<NcSoundingLayer2>();
         * List<NcSoundingLayer2> ppdd = new ArrayList<NcSoundingLayer2>();
         * List<NcSoundingLayer2> trop_a = new ArrayList<NcSoundingLayer2>();
         * List<NcSoundingLayer2> trop_c = new ArrayList<NcSoundingLayer2>();
         * List<NcSoundingLayer2> wmax_a = new ArrayList<NcSoundingLayer2>();
         * List<NcSoundingLayer2> wmax_c = new ArrayList<NcSoundingLayer2>(); if
         * ( merge == 0 ) { // ms.unMergedUairSounding //*System.out.println (
         * " Request unmerged data");
         * if(dataType.equals(DataType.ALLDATA.toString())) pf =
         * ObservedSoundingQuery.getObservedSndBufruaAllData(lat, lon,"",
         * refTimeCal, sndQuery); else pf =
         * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, "",
         * refTimeCal, dataType, sndQuery);
         * 
         * } else {
         * 
         * // Get TTAA. If not existent, try ship data (UUAA). If level is not
         * null or missing, // the body of code will return a sounding list with
         * MAN data or single level data. //*System.out.println (
         * " Request merged data at lat="+ lat+" lon="+lon+ " refT="+
         * refTimeCal.getTime().toGMTString());
         * 
         * pf = ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, "",
         * refTimeCal,"TTAA", sndQuery); ttaa = pf.getSoundingLyLst2(); ttbb =
         * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, "",
         * refTimeCal, "TTBB", sndQuery).getSoundingLyLst2(); ttcc =
         * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, "",
         * refTimeCal, "TTCC", sndQuery).getSoundingLyLst2(); ttdd =
         * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, "",
         * refTimeCal, "TTDD", sndQuery).getSoundingLyLst2(); //ppaa =
         * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, "",
         * refTimeCal, "PPAA", sndQuery).getSoundingLyLst2(); ppbb =
         * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, "",
         * refTimeCal, "PPBB", sndQuery).getSoundingLyLst2(); //ppcc =
         * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, "",
         * refTimeCal, "PPCC", sndQuery).getSoundingLyLst2(); ppdd =
         * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, "",
         * refTimeCal, "PPDD", sndQuery).getSoundingLyLst2(); wmax_a =
         * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, "",
         * refTimeCal, "MAXWIND_A", sndQuery).getSoundingLyLst2(); wmax_c =
         * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, "",
         * refTimeCal, "MAXWIND_C", sndQuery).getSoundingLyLst2(); trop_a =
         * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, "",
         * refTimeCal, "TROPOPAUSE_A", sndQuery).getSoundingLyLst2(); trop_c =
         * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, "",
         * refTimeCal, "TROPOPAUSE_C", sndQuery).getSoundingLyLst2(); pf =
         * ObservedSoundingQuery.getObservedSndStnInfo(lat, lon,"",sndType,
         * refTimeCal,sndQuery); sls =
         * ms.mergeUairSounding(level,ttaa,ttbb,ttcc,
         * ttdd,ppaa,ppbb,ppcc,ppdd,trop_a
         * ,trop_c,wmax_a,wmax_c,pf.getStationElevation());
         * 
         * //System.out.println("BUFRUA Number of Layers after merge:"+sls.size()
         * + " level="+level + " ms.isNumber(level)="+ms.isNumber(level));
         * //for(NcSoundingLayer ly: sls){ //
         * System.out.println("Pre= "+ly.getPressure()+ " Dew= "+
         * ly.getDewpoint()+ " T= "+
         * ly.getTemperature()+" H="+ly.getGeoHeight()+
         * " WSp="+ly.getWindSpeed()); //}
         * 
         * if (level.toUpperCase().equalsIgnoreCase("MAN") )
         * pf.setSoundingLyLst2(sls); else if( ms.isNumber(level)>=0 ){
         * if(sls.size() == 1){
         * //System.out.println("NcUair get one layer using level = "+ level);
         * pf.setSoundingLyLst2(sls); } else { pf = null;
         * //System.out.println("NcUair get 0 layer using level = "+ level); } }
         * else { if(sls.isEmpty() || sls.size() <=1) pf = null; else
         * pf.setSoundingLyLst2(sls); }
         * 
         * }
         * 
         * if(pf != null && pf.getSoundingLyLst2().size()>0) {
         * //System.out.println(
         * " pf is not null, so adding a profile to the list of NcSoundingProfiles "
         * );
         * 
         * // TODO : move this into the ObservedSoundingQuery methods. //
         * pf.setStationLatitude( lat ); pf.setStationLongitude( lon );
         * 
         * 
         * soundingProfileList.add(pf);
         * //pf.setStationLatitude(lat.floatValue());
         * //pf.setStationLongitude(lon.floatValue()); pf = null; } }
         */
        if (soundingProfileList.size() == 0) {
            // System.out.println(" Return status  from NcSoundingDrv.getSoundingLayer2DataUsingLatLonArray() is set to failed ");
            cube.setRtnStatus(failedRtnStatus);

        } else {
            // System.out.println();
            cube.setRtnStatus(NcSoundingCube.QueryStatus.OK);
            // long t02 = System.currentTimeMillis();
            // System.out.println("Return status from NcSoundingDrv.getSoundingLayer2DataUsingLatLonArray() - success, cube retreival took "
            // + (t02 - t01));
        }
        cube.setSoundingProfileList(soundingProfileList);
        /*
         * for(int i =0; i < cube.getSoundingProfileList().size();i++){
         * System.out.println("lat/lon="+
         * cube.getSoundingProfileList().get(i).getStationLatitude
         * ()+"/"+cube.getSoundingProfileList().get(i).getStationLongitude()+
         * " temp="
         * +cube.getSoundingProfileList().get(i).getSoundingLyLst2().get(
         * 0).getTemperature
         * ()+" dewp="+cube.getSoundingProfileList().get(i).getSoundingLyLst2
         * ().get(0).getDewpoint()+" press="+
         * cube.getSoundingProfileList().get(i
         * ).getSoundingLyLst2().get(0).getPressure() +
         * " height="+cube.getSoundingProfileList
         * ().get(i).getSoundingLyLst2().get(0).getGeoHeight()+
         * " windSp="+cube.getSoundingProfileList
         * ().get(i).getSoundingLyLst2().get
         * (0).getWindSpeed()+" windDir="+cube.getSoundingProfileList
         * ().get(i).getSoundingLyLst2().get(0).getWindDirection()+
         * " omega="+cube
         * .getSoundingProfileList().get(i).getSoundingLyLst2().get
         * (0).getOmega()); }
         */
        returnedObject = cube;
        // long t02 = System.currentTimeMillis();
        // System.out.println("getSoundingData2Generic query took "+(t02-t01)+" ms in total");

        return returnedObject;
    }

    /*
     * Chin:: 2/22/2012 Only NCUair and PFC (modelsounding DB) query is
     * supported now Use generic query API
     */
    public Object getSoundingDataGeneric() throws Exception {
        long t01 = System.currentTimeMillis();
        Object returnedObject = new Object();
        // *System.out.println ( " getSoundingDataByLatLonArray ");

        NcSoundingCube cube = new NcSoundingCube();
        List<NcSoundingProfile> soundingProfileList = new ArrayList<NcSoundingProfile>(
                0);
        NcSoundingCube.QueryStatus failedRtnStatus = NcSoundingCube.QueryStatus.FAILED;

        if (sndType.equals(ObsSndType.NCUAIR.toString())) {
            // Note: in NC Uair table, we only use ref time for query
            if (rangeTimeStringLst != null && rangeTimeStringLst.size() == 0) {
                // one single time line
                rangeTimeStringLst.add(refTimeStr);
            }

            List<NcUairRecord[]> uairRecordArrList;
            // long t003 = System.currentTimeMillis();
            uairRecordArrList = ObservedSoundingQuery
                    .getObservedSndNcUairDataGeneric(coordinateArray, stnIdArr,
                            rangeTimeStringLst, rangeTimeArr);
            // long t004 = System.currentTimeMillis();
            // System.out.println("getObservedSndNcUairDataGeneric API call took "+(t004-t003)+"ms");
            if (uairRecordArrList != null && uairRecordArrList.size() > 0) {
                // long t005 = System.currentTimeMillis();
                soundingProfileList = processQueryReturnedNcUairData(
                        uairRecordArrList, useNcSoundingLayer2);
                // long t006 = System.currentTimeMillis();
                // System.out.println("getSoundingDataGeneric total sounding time merging for "+uairRecordArrList.size()+" profiles took "+(t006-t005)+"ms");
            }

        } else if (sndType.equals(PfcSndType.NAMSND.toString())
                || sndType.equals(PfcSndType.GFSSND.toString())
                || sndType.equals(PfcSndType.RUCPTYPSND.toString())
                || sndType.equals(PfcSndType.RUC2SND.toString())) {

            List<NcSoundingProfile> listReturned = PfcSoundingQuery
                    .getPfcSndDataGeneric(coordinateArray, stnIdArr,
                            refTimeStr, rangeTimeStringLst, sndType, level);
            soundingProfileList.addAll(listReturned);
            if (useNcSoundingLayer2 == true) {
                convertPfcNcSoundingLayerToNcSoundingLayer2(soundingProfileList);
                // sysPrintProfileLayer2(soundingProfileList);
            }

        } else if (sndType.equals(MdlSndType.ANY.toString())) {
            return getSoundingDataByLatLonArray();
        } else if (sndType.equals(ObsSndType.BUFRUA.toString())) {
            return getSoundingDataByLatLonArray();
        }
        if (soundingProfileList.size() == 0) {
            // System.out.println(" Return status  from NcSoundingDrv.getSoundingLayer2DataUsingLatLonArray() is set to failed ");
            cube.setRtnStatus(failedRtnStatus);

        } else {
            // System.out.println();
            cube.setRtnStatus(NcSoundingCube.QueryStatus.OK);
            // long t02 = System.currentTimeMillis();
            // System.out.println("Return status from NcSoundingDrv.getSoundingLayer2DataUsingLatLonArray() - success, cube retreival took "
            // + (t02 - t01));
        }
        cube.setSoundingProfileList(soundingProfileList);
        /*
         * for(int i =0; i < cube.getSoundingProfileList().size();i++){
         * System.out.println("lat/lon="+
         * cube.getSoundingProfileList().get(i).getStationLatitude
         * ()+"/"+cube.getSoundingProfileList().get(i).getStationLongitude()+
         * " temp="
         * +cube.getSoundingProfileList().get(i).getSoundingLyLst2().get(
         * 0).getTemperature
         * ()+" dewp="+cube.getSoundingProfileList().get(i).getSoundingLyLst2
         * ().get(0).getDewpoint()+" press="+
         * cube.getSoundingProfileList().get(i
         * ).getSoundingLyLst2().get(0).getPressure() +
         * " height="+cube.getSoundingProfileList
         * ().get(i).getSoundingLyLst2().get(0).getGeoHeight()+
         * " windSp="+cube.getSoundingProfileList
         * ().get(i).getSoundingLyLst2().get
         * (0).getWindSpeed()+" windDir="+cube.getSoundingProfileList
         * ().get(i).getSoundingLyLst2().get(0).getWindDirection()+
         * " omega="+cube
         * .getSoundingProfileList().get(i).getSoundingLyLst2().get
         * (0).getOmega()); }
         */
        returnedObject = cube;
        // long t02 = System.currentTimeMillis();
        // System.out.println("getSoundingDataGeneric API took "+(t02-t01)+" ms in total");

        return returnedObject;
    }

    /*
     * This API is for getting multiple locations sounding info at one shot.
     * StnIdArray or StnNumArray is used as input stn info for each location.
     * 
     * Chin Note: 02/27/12 obsoleting this one. Use getSoundingDataGeneric()
     * 
     * public Object getSoundingDataByStnArray() throws Exception { Object
     * returnedObject = new Object(); //System.out.println (
     * " getSoundingData ");
     * 
     * 
     * NcSoundingCube cube = new NcSoundingCube(); List<NcSoundingProfile>
     * soundingProfileList = new ArrayList<NcSoundingProfile>(); SndQueryKeyType
     * sndQuery; String[] stnArray;
     * if(queryType.equals(SndQueryKeyType.STNID.toString())){ sndQuery =
     * SndQueryKeyType.STNID; stnArray = stnIdArr.clone();
     * 
     * } else if(queryType.equals(SndQueryKeyType.STNNUM.toString())){ sndQuery
     * = SndQueryKeyType.STNNUM; stnArray = stnNumArr.clone(); } else {
     * returnedObject = null; return returnedObject; } if(stnArray.length <= 0){
     * returnedObject = null; return returnedObject; } List<String>
     * timeLineStrList=new ArrayList<String>(); List<Calendar> timeLimeCalList =
     * new ArrayList<Calendar>(); if(validTimeEnd !=0 && validTimeEnd >
     * validTimeStart){ //range of time line request timeLimeCalList =
     * ObservedSoundingQuery.getObservedSndTimeRangeList(sndType,
     * validTimeStartCal, validTimeEndCal); for (int i=0; i<
     * timeLimeCalList.size(); i++){ Calendar timeCal = timeLimeCalList.get(i);
     * String timeStr = String.format("%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS",
     * timeCal); timeLineStrList.add(timeStr);
     * 
     * } } else { //one single time line timeLineStrList.add(new
     * SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(refTimeCal.getTime()));
     * timeLimeCalList.add(refTimeCal); } String stn; for ( int i=0; i <
     * stnArray.length ; i++) {
     * 
     * stn = stnArray[i]; stn.toUpperCase(Locale.ENGLISH); //System.out.println
     * ( "Request getSoundingData at " + stn); MergeSounding ms = new
     * MergeSounding(); for (int j=0; j< timeLineStrList.size(); j++){ String
     * timeStr = timeLineStrList.get(j); Calendar timeCal =
     * timeLimeCalList.get(j);
     * 
     * List<NcSoundingLayer> sls = new ArrayList<NcSoundingLayer>();
     * List<NcSoundingLayer> ttaa = new ArrayList<NcSoundingLayer>();
     * List<NcSoundingLayer> ttbb = new ArrayList<NcSoundingLayer>();
     * List<NcSoundingLayer> ttcc = new ArrayList<NcSoundingLayer>();
     * List<NcSoundingLayer> ttdd = new ArrayList<NcSoundingLayer>();
     * List<NcSoundingLayer> ppaa = new ArrayList<NcSoundingLayer>();
     * List<NcSoundingLayer> ppbb = new ArrayList<NcSoundingLayer>();
     * List<NcSoundingLayer> ppcc = new ArrayList<NcSoundingLayer>();
     * List<NcSoundingLayer> ppdd = new ArrayList<NcSoundingLayer>();
     * List<NcSoundingLayer> trop_a = new ArrayList<NcSoundingLayer>();
     * List<NcSoundingLayer> trop_c = new ArrayList<NcSoundingLayer>();
     * List<NcSoundingLayer> wmax_a = new ArrayList<NcSoundingLayer>();
     * List<NcSoundingLayer> wmax_c = new ArrayList<NcSoundingLayer>();
     * 
     * NcSoundingProfile pf = null; if
     * (sndType.equals(ObsSndType.NCUAIR.toString())){
     * //System.out.println("getSoundingDataByLatLonArray:NcUair: lat="+lat+
     * " lon="+lon); if ( merge == 0 ) { //Chin...need more coding pf = null; }
     * else{
     * 
     * long t001 = System.currentTimeMillis(); //get TTAA & TROPOPAUSE_A &
     * MAXWIND_A NcUairRecord record =
     * ObservedSoundingQuery.getObservedSndNcUairData(lat, lon, "", timeStr,
     * "TTAA", sndQuery); if(record != null){ ttaa =
     * getSndLayersFromNcUairRecordObsLevel(record); trop_a =
     * getSndLayersFromNcUairRecordTrop(record); wmax_a =
     * getSndLayersFromNcUairRecordMaxw(record); } //get TTCC & TROPOPAUSE_C &
     * MAXWIND_C record = ObservedSoundingQuery.getObservedSndNcUairData(0d, 0d,
     * stn, timeStr, "TTCC", sndQuery); if(record != null){ ttcc =
     * getSndLayersFromNcUairRecordObsLevel(record); trop_c =
     * getSndLayersFromNcUairRecordTrop(record); wmax_c =
     * getSndLayersFromNcUairRecordMaxw(record); } //get TTBB record =
     * ObservedSoundingQuery.getObservedSndNcUairData(0d, 0d, stn, timeStr,
     * "TTBB", sndQuery); if(record != null){ ttbb =
     * getSndLayersFromNcUairRecordObsLevel(record); } //get TTDD record =
     * ObservedSoundingQuery.getObservedSndNcUairData(0d, 0d, stn, timeStr,
     * "TTDD", sndQuery); if(record != null){ ttdd =
     * getSndLayersFromNcUairRecordObsLevel(record); } //get PPAA record =
     * ObservedSoundingQuery.getObservedSndNcUairData(0d, 0d, stn, timeStr,
     * "PPAA", sndQuery); if(record != null){ ppaa =
     * getSndLayersFromNcUairRecordObsLevel(record); } //get PPBB record =
     * ObservedSoundingQuery.getObservedSndNcUairData(0d, 0d, stn, timeStr,
     * "PPBB", sndQuery); if(record != null){ ppbb =
     * getSndLayersFromNcUairRecordObsLevel(record); } //get PPCC record =
     * ObservedSoundingQuery.getObservedSndNcUairData(0d, 0d, stn, timeStr,
     * "PPCC", sndQuery); if(record != null){ ppcc =
     * getSndLayersFromNcUairRecordObsLevel(record); } //get PPDD record =
     * ObservedSoundingQuery.getObservedSndNcUairData(0d, 0d, stn, timeStr,
     * "PPDD", sndQuery); if(record != null){ ppdd =
     * getSndLayersFromNcUairRecordObsLevel(record); } pf =
     * ObservedSoundingQuery.getObservedSndStnInfo(0d, 0d, stn,sndType,
     * refTimeCal,sndQuery); long t02 = System.currentTimeMillis();
     * System.out.println("NcUair (by stn) profile retreival  took " + (t02 -
     * t001)); sls =
     * ms.mergeUairSounding(level,ttaa,ttbb,ttcc,ttdd,ppaa,ppbb,ppcc
     * ,ppdd,trop_a,trop_c,wmax_a,wmax_c,pf.getStationElevation());
     * //System.out.println("NcUair Number of Layers:"+sls.size());
     * //for(NcSoundingLayer ly: sls){ //
     * System.out.println("Pre= "+ly.getPressure()+ " Dew= "+ ly.getDewpoint()+
     * " T= "+
     * ly.getTemperature()+" H="+ly.getGeoHeight()+" WSp="+ly.getWindSpeed());
     * //}
     * 
     * pf.setSoundingLyLst(sls); } } else if
     * (//sndType.equals(ObsSndType.UAIR.toString()) ||
     * sndType.equals(ObsSndType.DROP.toString()) ||
     * sndType.equals(ObsSndType.TAMDAR.toString())) {
     * 
     * if ( merge == 0 ) { // ms.unMergedUairSounding //System.out.println (
     * " Request unmerged data");
     * if(dataType.equals(DataType.ALLDATA.toString())) pf =
     * ObservedSoundingQuery.getObservedSndAllData(0d, 0d, stn, timeCal,
     * sndType, sndQuery); else pf =
     * ObservedSoundingQuery.getObservedSndData(0d, 0d, stn, timeCal, sndType,
     * dataType, sndQuery);
     * 
     * 
     * } else {
     * 
     * // Get TTAA. If not existent, try ship data (UUAA). If level is not null
     * or missing, // the body of code will return a sounding list with MAN data
     * or single level data. //*System.out.println (
     * " Request merged data at lat="+ lat+" lon="+lon+ " refT="+
     * refTimeCal.getTime().toGMTString());
     * 
     * // TO DO -----> add station ID and station number and a list of stations
     * queries. pf = ObservedSoundingQuery.getObservedSndData(0d, 0d, stn,
     * timeCal, sndType, "TTAA", sndQuery); ttaa = pf.getSoundingLyLst(); if
     * (ttaa.size() == 0) { ttaa = ObservedSoundingQuery.getObservedSndData(0d,
     * 0d, stn, timeCal, sndType, "UUAA", sndQuery).getSoundingLyLst(); }
     * 
     * ttbb = ObservedSoundingQuery.getObservedSndData(0d, 0d, stn, timeCal,
     * sndType, "TTBB", sndQuery).getSoundingLyLst(); if (ttbb.size() == 0) {
     * ttbb = ObservedSoundingQuery.getObservedSndData(0d, 0d, stn, timeCal,
     * sndType, "UUBB", sndQuery).getSoundingLyLst(); }
     * 
     * ttcc = ObservedSoundingQuery.getObservedSndData(0d, 0d, stn, timeCal,
     * sndType, "TTCC", sndQuery).getSoundingLyLst(); if (ttcc.size() == 0) {
     * ttcc = ObservedSoundingQuery.getObservedSndData(0d, 0d, stn, timeCal,
     * sndType, "UUCC", sndQuery).getSoundingLyLst(); }
     * 
     * ttdd = ObservedSoundingQuery.getObservedSndData(0d, 0d, stn, timeCal,
     * sndType, "TTDD", sndQuery).getSoundingLyLst(); if (ttdd.size() == 0) {
     * ttdd = ObservedSoundingQuery.getObservedSndData(0d, 0d, stn, timeCal,
     * sndType, "UUDD", sndQuery).getSoundingLyLst(); }
     * 
     * ppaa = ObservedSoundingQuery.getObservedSndData(0d, 0d, stn, timeCal,
     * sndType, "PPAA", sndQuery).getSoundingLyLst(); ppbb =
     * ObservedSoundingQuery.getObservedSndData(0d, 0d, stn, timeCal, sndType,
     * "PPBB", sndQuery).getSoundingLyLst(); ppcc =
     * ObservedSoundingQuery.getObservedSndData(0d, 0d, stn, timeCal, sndType,
     * "PPCC", sndQuery).getSoundingLyLst(); ppdd =
     * ObservedSoundingQuery.getObservedSndData(0d, 0d, stn, timeCal, sndType,
     * "PPDD", sndQuery).getSoundingLyLst(); wmax_a =
     * ObservedSoundingQuery.getObservedSndData(0d, 0d, stn, timeCal, sndType,
     * "MAXWIND_A", sndQuery).getSoundingLyLst(); wmax_c =
     * ObservedSoundingQuery.getObservedSndData(0d, 0d, stn, timeCal, sndType,
     * "MAXWIND_C", sndQuery).getSoundingLyLst(); trop_a =
     * ObservedSoundingQuery.getObservedSndData(0d, 0d, stn, timeCal, sndType,
     * "TROPOPAUSE_A", sndQuery).getSoundingLyLst(); trop_c =
     * ObservedSoundingQuery.getObservedSndData(0d, 0d, stn, timeCal, sndType,
     * "TROPOPAUSE_C", sndQuery).getSoundingLyLst(); pf =
     * ObservedSoundingQuery.getObservedSndStnInfo(0d, 0d,stn,sndType,
     * timeCal,sndQuery); sls =
     * ms.mergeUairSounding(level,ttaa,ttbb,ttcc,ttdd,ppaa
     * ,ppbb,ppcc,ppdd,trop_a,trop_c,wmax_a,wmax_c,pf.getStationElevation());
     * pf.setSoundingLyLst(sls); }
     * 
     * } else if(sndType.equals(ObsSndType.BUFRUA.toString())){ if ( merge == 0
     * ) { // ms.unMergedUairSounding //*System.out.println (
     * " Request unmerged data");
     * if(dataType.equals(DataType.ALLDATA.toString())) pf =
     * ObservedSoundingQuery.getObservedSndBufruaAllData(0d, 0d, stn,
     * refTimeCal, sndQuery); else pf =
     * ObservedSoundingQuery.getObservedSndBufruaData(0d, 0d, stn, refTimeCal,
     * dataType, sndQuery);
     * 
     * } else {
     * 
     * // Get TTAA. If not existent, try ship data (UUAA). If level is not null
     * or missing, // the b0dy of c0de will return a sounding list with MAN data
     * or single level data. //*System.out.println (
     * " Request merged data at lat="+ lat+" lon="+lon+ " refT="+
     * refTimeCal.getTime().toGMTString());
     * 
     * pf = ObservedSoundingQuery.getObservedSndBufruaData(0d, 0d, stn,
     * refTimeCal,"TTAA", sndQuery); ttaa = pf.getSoundingLyLst(); ttbb =
     * ObservedSoundingQuery.getObservedSndBufruaData(0d, 0d, stn, refTimeCal,
     * "TTBB", sndQuery).getSoundingLyLst(); ttcc =
     * ObservedSoundingQuery.getObservedSndBufruaData(0d, 0d, stn, refTimeCal,
     * "TTCC", sndQuery).getSoundingLyLst(); ttdd =
     * ObservedSoundingQuery.getObservedSndBufruaData(0d, 0d, stn, refTimeCal,
     * "TTDD", sndQuery).getSoundingLyLst(); //ppaa =
     * ObservedSoundingQuery.getObservedSndBufruaData(0d, 0d, stn, refTimeCal,
     * "PPAA", sndQuery).getSoundingLyLst(); ppbb =
     * ObservedSoundingQuery.getObservedSndBufruaData(0d, 0d, stn, refTimeCal,
     * "PPBB", sndQuery).getSoundingLyLst(); //ppcc =
     * ObservedSoundingQuery.getObservedSndBufruaData(0d, 0d, stn, refTimeCal,
     * "PPCC", sndQuery).getSoundingLyLst(); ppdd =
     * ObservedSoundingQuery.getObservedSndBufruaData(0d, 0d, stn, refTimeCal,
     * "PPDD", sndQuery).getSoundingLyLst(); wmax_a =
     * ObservedSoundingQuery.getObservedSndBufruaData(0d, 0d, stn, refTimeCal,
     * "MAXWIND_A", sndQuery).getSoundingLyLst(); wmax_c =
     * ObservedSoundingQuery.getObservedSndBufruaData(0d, 0d, stn, refTimeCal,
     * "MAXWIND_C", sndQuery).getSoundingLyLst(); trop_a =
     * ObservedSoundingQuery.getObservedSndBufruaData(0d, 0d, stn, refTimeCal,
     * "TROPOPAUSE_A", sndQuery).getSoundingLyLst(); trop_c =
     * ObservedSoundingQuery.getObservedSndBufruaData(0d, 0d, stn, refTimeCal,
     * "TROPOPAUSE_C", sndQuery).getSoundingLyLst(); pf =
     * ObservedSoundingQuery.getObservedSndStnInfo(0d, 0d,stn,sndType,
     * refTimeCal,sndQuery); sls =
     * ms.mergeUairSounding(level,ttaa,ttbb,ttcc,ttdd
     * ,ppaa,ppbb,ppcc,ppdd,trop_a,
     * trop_c,wmax_a,wmax_c,pf.getStationElevation()); pf.setSoundingLyLst(sls);
     * } } else if(sndType.equals(PfcSndType.NAMSND.toString()) ||
     * sndType.equals(PfcSndType.GFSSND.toString()) ||
     * sndType.equals(PfcSndType.RUCPTYPSND.toString()) ||
     * sndType.equals(PfcSndType.RUC2SND.toString())) { //*System.out.println (
     * " Processing native model data"); pf =
     * PfcSoundingQuery.getPfcSndData(0d,0d, stn, refTimeCal, validTimeStartCal,
     * sndType,sndQuery); ms.nativeModelSounding(pf.getSoundingLyLst(),
     * pf.getStationElevation()); if ( ms.isNumber (level) == 0 ) { //level is
     * an integer >=0. It means user request a single level float rlev = new
     * Integer(Integer.parseInt(level.trim())).floatValue();
     * pf.setSoundingLyLst(ms.getSingLevel(rlev, pf.getSoundingLyLst())); } else
     * if ( ms.isNumber (level) == 1 ) { //level is an float >=0. It also means
     * user request a single level float rlev = new
     * Float(Float.parseFloat(level.trim()));
     * pf.setSoundingLyLst(ms.getSingLevel(rlev, pf.getSoundingLyLst())); }
     * 
     * } else if(sndType.equals(MdlSndType.ANY.toString())) { //model sounding
     * query by stn is not supported yet pf = null; } else { pf = null; } if(pf
     * != null && pf.getSoundingLyLst().size()>0) { soundingProfileList.add(pf);
     * pf.setStationId(stn);
     * 
     * pf = null; } } } if(soundingProfileList.size() == 0 )
     * cube.setRtnStatus(NcSoundingCube.QueryStatus.FAILED); else
     * cube.setRtnStatus(NcSoundingCube.QueryStatus.OK);
     * 
     * cube.setSoundingProfileList(soundingProfileList); returnedObject = cube;
     * 
     * 
     * return returnedObject; }
     */
    public Object getModels() throws Exception {
        Object returnedObject = new Object();
        NcSoundingModel mdls = MdlSoundingQuery.getMdls(pluginName);
        returnedObject = mdls;
        return returnedObject;
    }

    /*
     * Chin: this is not completed yet.... When use point data query for bufrua
     * is supported, then we have to work on this one.
     */
    private List<NcSoundingProfile> processQueryReturnedBufruaData(
            List<UAObs[]> uairRecordArrList, boolean useNcSndLayer2) {
        List<NcSoundingProfile> soundingProfileList = new ArrayList<NcSoundingProfile>(
                0);
        /*
         * if ( merge == 0 ) { return soundingProfileList;
         * 
         * } else { for(UAObs[] recordArray:uairRecordArrList){
         * 
         * NcSoundingProfile pf;
         * 
         * pf = new NcSoundingProfile(); if(useNcSndLayer2== true){ //need more
         * code } else { if(recordArray != null && recordArray.length >0){
         * MergeSounding ms = new MergeSounding(); List<NcSoundingLayer> sls =
         * new ArrayList<NcSoundingLayer>(); List<NcSoundingLayer> ttaa = new
         * ArrayList<NcSoundingLayer>(); List<NcSoundingLayer> ttbb = new
         * ArrayList<NcSoundingLayer>(); List<NcSoundingLayer> ttcc = new
         * ArrayList<NcSoundingLayer>(); List<NcSoundingLayer> ttdd = new
         * ArrayList<NcSoundingLayer>(); List<NcSoundingLayer> ppaa = new
         * ArrayList<NcSoundingLayer>(); List<NcSoundingLayer> ppbb = new
         * ArrayList<NcSoundingLayer>(); List<NcSoundingLayer> ppcc = new
         * ArrayList<NcSoundingLayer>(); List<NcSoundingLayer> ppdd = new
         * ArrayList<NcSoundingLayer>(); List<NcSoundingLayer> trop_a = new
         * ArrayList<NcSoundingLayer>(); List<NcSoundingLayer> trop_c = new
         * ArrayList<NcSoundingLayer>(); List<NcSoundingLayer> wmax_a = new
         * ArrayList<NcSoundingLayer>(); List<NcSoundingLayer> wmax_c = new
         * ArrayList<NcSoundingLayer>();
         * 
         * for(int k=0; k< recordArray.length; k++){ UAObs record=
         * recordArray[k]; if(record.getReportType() ==
         * NcSoundingLayer.dataTypeMap.get("TTAA")){ ttaa =
         * getSndLayersFromNcUairRecordObsLevel(record); trop_a =
         * getSndLayersFromNcUairRecordTrop(record); wmax_a =
         * getSndLayersFromNcUairRecordMaxw(record); } else
         * if(record.getDataType().equals("TTBB")){ ttbb =
         * getSndLayersFromNcUairRecordObsLevel(record); } else
         * if(record.getDataType().equals("TTCC")){ ttcc =
         * getSndLayersFromNcUairRecordObsLevel(record); trop_c =
         * getSndLayersFromNcUairRecordTrop(record); wmax_c =
         * getSndLayersFromNcUairRecordMaxw(record); } else
         * if(record.getDataType().equals("TTDD")){ ttdd =
         * getSndLayersFromNcUairRecordObsLevel(record); } else
         * if(record.getDataType().equals("PPAA")){ ppaa=
         * getSndLayersFromNcUairRecordObsLevel(record); } else
         * if(record.getDataType().equals("PPBB")){ ppbb =
         * getSndLayersFromNcUairRecordObsLevel(record); } else
         * if(record.getDataType().equals("PPCC")){ ppcc =
         * getSndLayersFromNcUairRecordObsLevel(record); } else
         * if(record.getDataType().equals("PPDD")){ ppdd =
         * getSndLayersFromNcUairRecordObsLevel(record); } } pf = new
         * NcSoundingProfile();
         * pf.setStationElevation((float)recordArray[0].getElevation());
         * pf.setStationId(recordArray[0].getStationId());
         * if(recordArray[0].getStnum() != null &&
         * recordArray[0].getStnum().length()>0)
         * pf.setStationNum(Integer.parseInt(recordArray[0].getStnum()));
         * pf.setStationLatitude(recordArray[0].getLatitude());
         * pf.setStationLongitude(recordArray[0].getLongitude());
         * pf.setFcsTime(recordArray[0].getDataTime().getRefTime().getTime());
         * //
         * System.out.println(" input lat="+lat+" pf's lat="+pf.getStationLatitude
         * ()+" elv="+pf.getStationElevation()+" stnId="+pf.getStationId()); sls
         * =
         * ms.mergeUairSounding(level,ttaa,ttbb,ttcc,ttdd,ppaa,ppbb,ppcc,ppdd,trop_a
         * ,trop_c,wmax_a,wmax_c,pf.getStationElevation()); if
         * (level.toUpperCase().equalsIgnoreCase("MAN") )
         * pf.setSoundingLyLst(sls); else if( ms.isNumber(level)>=0 ){
         * if(sls.size() == 1){
         * //System.out.println("NCUAIR get one layer using level = "+ level);
         * pf.setSoundingLyLst(sls); } else { pf = null;
         * //System.out.println("NCUAIR get 0 layer using level = "+ level); } }
         * else { if(sls.isEmpty() || sls.size() <=1) pf = null; else
         * pf.setSoundingLyLst(sls); } } else pf = null; } if(pf != null &&
         * (pf.getSoundingLyLst2().size()>0||pf.getSoundingLyLst().size()>0 )) {
         * //System.out.println(
         * " pf is not null, so adding a profile to the list of NcSoundingProfiles "
         * ); soundingProfileList.add(pf); pf = null; } } }
         */
        return soundingProfileList;
    }

    private List<NcSoundingProfile> processQueryReturnedNcUairData(
            List<NcUairRecord[]> uairRecordArrList, boolean useNcSndLayer2) {
        List<NcSoundingProfile> soundingProfileList = new ArrayList<NcSoundingProfile>(
                0);
        for (NcUairRecord[] recordArray : uairRecordArrList) {

            NcSoundingProfile pf;

            if (merge == 0) {
                // Chin...need more coding
                pf = null;
            } else {
                pf = new NcSoundingProfile();
                if (useNcSndLayer2 == true) {
                    // use NcSoundingLayer2
                    if (recordArray != null && recordArray.length > 0) {
                        MergeSounding2 ms2 = new MergeSounding2();
                        List<NcSoundingLayer2> sls = new ArrayList<NcSoundingLayer2>();
                        List<NcSoundingLayer2> ttaa = new ArrayList<NcSoundingLayer2>();
                        List<NcSoundingLayer2> ttbb = new ArrayList<NcSoundingLayer2>();
                        List<NcSoundingLayer2> ttcc = new ArrayList<NcSoundingLayer2>();
                        List<NcSoundingLayer2> ttdd = new ArrayList<NcSoundingLayer2>();
                        List<NcSoundingLayer2> ppaa = new ArrayList<NcSoundingLayer2>();
                        List<NcSoundingLayer2> ppbb = new ArrayList<NcSoundingLayer2>();
                        List<NcSoundingLayer2> ppcc = new ArrayList<NcSoundingLayer2>();
                        List<NcSoundingLayer2> ppdd = new ArrayList<NcSoundingLayer2>();
                        List<NcSoundingLayer2> trop_a = new ArrayList<NcSoundingLayer2>();
                        List<NcSoundingLayer2> trop_c = new ArrayList<NcSoundingLayer2>();
                        List<NcSoundingLayer2> wmax_a = new ArrayList<NcSoundingLayer2>();
                        List<NcSoundingLayer2> wmax_c = new ArrayList<NcSoundingLayer2>();

                        for (int k = 0; k < recordArray.length; k++) {
                            NcUairRecord record = recordArray[k];
                            if (record.getDataType().equals("TTAA")
                                    || record.getDataType().equals("XXAA")) {
                                ttaa = getSoundingLayer2FromNcUairRecordObsLevel(record);
                                trop_a = getSoundingLayer2FromNcUairRecordTrop(record);
                                wmax_a = getSoundingLayer2FromNcUairRecordMaxw(record);
                            } else if (record.getDataType().equals("TTBB")
                                    || record.getDataType().equals("XXBB")) {
                                ttbb = getSoundingLayer2FromNcUairRecordObsLevel(record);
                            } else if (record.getDataType().equals("TTCC")
                                    || record.getDataType().equals("XXCC")) {
                                ttcc = getSoundingLayer2FromNcUairRecordObsLevel(record);
                                trop_c = getSoundingLayer2FromNcUairRecordTrop(record);
                                wmax_c = getSoundingLayer2FromNcUairRecordMaxw(record);
                            } else if (record.getDataType().equals("TTDD")
                                    || record.getDataType().equals("XXDD")) {
                                ttdd = getSoundingLayer2FromNcUairRecordObsLevel(record);
                            } else if (record.getDataType().equals("PPAA")) {
                                ppaa = getSoundingLayer2FromNcUairRecordObsLevel(record);
                            } else if (record.getDataType().equals("PPBB")) {
                                ppbb = getSoundingLayer2FromNcUairRecordObsLevel(record);
                            } else if (record.getDataType().equals("PPCC")) {
                                ppcc = getSoundingLayer2FromNcUairRecordObsLevel(record);
                            } else if (record.getDataType().equals("PPDD")) {
                                ppdd = getSoundingLayer2FromNcUairRecordObsLevel(record);
                            }
                        }
                        pf.setStationElevation((float) recordArray[0]
                                .getElevation());
                        pf.setStationId(recordArray[0].getStationId());
                        if (recordArray[0].getStnum() != null
                                && recordArray[0].getStnum().length() > 0)
                            pf.setStationNum(Integer.parseInt(recordArray[0]
                                    .getStnum()));
                        pf.setStationLatitude(recordArray[0].getLatitude());
                        pf.setStationLongitude(recordArray[0].getLongitude());
                        pf.setFcsTime(recordArray[0].getDataTime().getRefTime()
                                .getTime());
                        // System.out.println("m2 input lat=" + lat +
                        // " pf's lat="
                        // + pf.getStationLatitude() + " elv="
                        // + pf.getStationElevation() + " stnId="
                        // + pf.getStationId());
                        if (useNcSndLayer2)
                            sls = ms2.mergeUairSounding(level, ttaa, ttbb,
                                    ttcc, ttdd, ppaa, ppbb, ppcc, ppdd, trop_a,
                                    trop_c, wmax_a, wmax_c,
                                    pf.getStationElevation());

                        if (level.toUpperCase().equalsIgnoreCase("MAN")) {
                            pf.setSoundingLyLst2(sls);
                            // System.out.println("sls set to the sounding profile");
                        } else if (ms2.isNumber(level) >= 0) {
                            if (sls.size() == 1) {
                                // System.out.println("NcUair get one layer using level = "+
                                // level);
                                pf.setSoundingLyLst2(sls);
                            } else {
                                pf = null;
                                // System.out.println("NcUair get 0 layer using level = "+
                                // level);
                            }
                        } else {
                            if (sls.isEmpty() || sls.size() <= 1) {
                                pf = null;
                                // System.out.println("not MAN level &  sls is empty or 1");
                            } else {
                                pf.setSoundingLyLst2(sls);
                                // System.out.println("sls set to the sounding profile for level = "
                                // + level);
                            }
                        }
                    }
                } else {
                    // use NcSoundingLayer
                    if (recordArray != null && recordArray.length > 0) {
                        MergeSounding ms = new MergeSounding();
                        List<NcSoundingLayer> sls = new ArrayList<NcSoundingLayer>();
                        List<NcSoundingLayer> ttaa = new ArrayList<NcSoundingLayer>();
                        List<NcSoundingLayer> ttbb = new ArrayList<NcSoundingLayer>();
                        List<NcSoundingLayer> ttcc = new ArrayList<NcSoundingLayer>();
                        List<NcSoundingLayer> ttdd = new ArrayList<NcSoundingLayer>();
                        List<NcSoundingLayer> ppaa = new ArrayList<NcSoundingLayer>();
                        List<NcSoundingLayer> ppbb = new ArrayList<NcSoundingLayer>();
                        List<NcSoundingLayer> ppcc = new ArrayList<NcSoundingLayer>();
                        List<NcSoundingLayer> ppdd = new ArrayList<NcSoundingLayer>();
                        List<NcSoundingLayer> trop_a = new ArrayList<NcSoundingLayer>();
                        List<NcSoundingLayer> trop_c = new ArrayList<NcSoundingLayer>();
                        List<NcSoundingLayer> wmax_a = new ArrayList<NcSoundingLayer>();
                        List<NcSoundingLayer> wmax_c = new ArrayList<NcSoundingLayer>();
                        for (int k = 0; k < recordArray.length; k++) {
                            NcUairRecord record = recordArray[k];
                            if (record.getDataType().equals("TTAA")
                                    || record.getDataType().equals("XXAA")) {
                                ttaa = getSndLayersFromNcUairRecordObsLevel(record);
                                trop_a = getSndLayersFromNcUairRecordTrop(record);
                                wmax_a = getSndLayersFromNcUairRecordMaxw(record);
                            } else if (record.getDataType().equals("TTBB")
                                    || record.getDataType().equals("XXBB")) {
                                ttbb = getSndLayersFromNcUairRecordObsLevel(record);
                            } else if (record.getDataType().equals("TTCC")
                                    || record.getDataType().equals("XXCC")) {
                                ttcc = getSndLayersFromNcUairRecordObsLevel(record);
                                trop_c = getSndLayersFromNcUairRecordTrop(record);
                                wmax_c = getSndLayersFromNcUairRecordMaxw(record);
                            } else if (record.getDataType().equals("TTDD")
                                    || record.getDataType().equals("XXDD")) {
                                ttdd = getSndLayersFromNcUairRecordObsLevel(record);
                            } else if (record.getDataType().equals("PPAA")) {
                                ppaa = getSndLayersFromNcUairRecordObsLevel(record);
                            } else if (record.getDataType().equals("PPBB")) {
                                ppbb = getSndLayersFromNcUairRecordObsLevel(record);
                            } else if (record.getDataType().equals("PPCC")) {
                                ppcc = getSndLayersFromNcUairRecordObsLevel(record);
                            } else if (record.getDataType().equals("PPDD")) {
                                ppdd = getSndLayersFromNcUairRecordObsLevel(record);
                            }
                        }
                        pf = new NcSoundingProfile();
                        pf.setStationElevation((float) recordArray[0]
                                .getElevation());
                        pf.setStationId(recordArray[0].getStationId());
                        if (recordArray[0].getStnum() != null
                                && recordArray[0].getStnum().length() > 0)
                            pf.setStationNum(Integer.parseInt(recordArray[0]
                                    .getStnum()));
                        pf.setStationLatitude(recordArray[0].getLatitude());
                        pf.setStationLongitude(recordArray[0].getLongitude());
                        pf.setFcsTime(recordArray[0].getDataTime().getRefTime()
                                .getTime());
                        // System.out.println(" input lat="+lat+" pf's lat="+pf.getStationLatitude()+" elv="+pf.getStationElevation()+" stnId="+pf.getStationId());
                        sls = ms.mergeUairSounding(level, ttaa, ttbb, ttcc,
                                ttdd, ppaa, ppbb, ppcc, ppdd, trop_a, trop_c,
                                wmax_a, wmax_c, pf.getStationElevation());
                        // System.out.println("NCUAIR Number of Layers after merge:"+sls.size()
                        // + " level="+level +
                        // " ms.isNumber(level)="+ms.isNumber(level));
                        // for(NcSoundingLayer ly: sls){
                        // System.out.println("Pre= "+ly.getPressure()+
                        // " Dew= "+ ly.getDewpoint()+ " T= "+
                        // ly.getTemperature()+" H="+ly.getGeoHeight()+" WSp="+ly.getWindSpeed());
                        // }

                        if (level.toUpperCase().equalsIgnoreCase("MAN"))
                            pf.setSoundingLyLst(sls);
                        else if (ms.isNumber(level) >= 0) {
                            if (sls.size() == 1) {
                                // System.out.println("NCUAIR get one layer using level = "+
                                // level);
                                pf.setSoundingLyLst(sls);
                            } else {
                                pf = null;
                                // System.out.println("NCUAIR get 0 layer using level = "+
                                // level);
                            }
                        } else {
                            if (sls.isEmpty() || sls.size() <= 1)
                                pf = null;
                            else
                                pf.setSoundingLyLst(sls);
                        }
                    } else
                        pf = null;
                }
                if (pf != null
                        && (pf.getSoundingLyLst2().size() > 0 || pf
                                .getSoundingLyLst().size() > 0)) {
                    // System.out.println(" pf is not null, so adding a profile to the list of NcSoundingProfiles ");
                    soundingProfileList.add(pf);
                    pf = null;
                }
            }
        }
        return soundingProfileList;
    }

    /*
     * Convert sounding data saved in NcSoundingLayer list to NcSoundingLayer2
     * list remove NcSoundingLayer data to have a smaller size for sending back
     * to client
     */
    private void convertPfcNcSoundingLayerToNcSoundingLayer2(
            List<NcSoundingProfile> pfLst) {
        for (NcSoundingProfile pf : pfLst) {
            List<NcSoundingLayer2> soundLy2List = new ArrayList<NcSoundingLayer2>();
            for (NcSoundingLayer level : pf.getSoundingLyLst()) {
                NcSoundingLayer2 soundingLy2;
                try {
                    soundingLy2 = new NcSoundingLayer2();
                    AirTemperature airTemp;
                    airTemp = new AirTemperature();
                    airTemp.setValue(new Amount(level.getTemperature(),
                            SI.CELSIUS));
                    soundingLy2.setTemperature(airTemp);

                    DewPointTemp dewPoint = new DewPointTemp();
                    dewPoint.setValue(new Amount(level.getDewpoint(),
                            SI.CELSIUS));
                    soundingLy2.setDewpoint(dewPoint);

                    PressureLevel pressure = new PressureLevel();
                    pressure.setValue(new Amount(level.getPressure(),
                            NcUnits.MILLIBAR));
                    soundingLy2.setPressure(pressure);

                    WindDirection windDirection = new WindDirection();
                    windDirection.setValue(level.getWindDirection(),
                            NonSI.DEGREE_ANGLE);
                    soundingLy2.setWindDirection(windDirection);

                    WindSpeed windSpeed = new WindSpeed();
                    // HDF5 data in unit of Knots, no conversion needed
                    windSpeed.setValue(level.getWindSpeed(), NonSI.KNOT);
                    soundingLy2.setWindSpeed(windSpeed);

                    HeightAboveSeaLevel height = new HeightAboveSeaLevel();
                    height.setValue(level.getGeoHeight(), SI.METER);
                    soundingLy2.setGeoHeight(height);

                    Omega omega = new Omega();
                    omega.setValueAs(level.getOmega(), "");
                    soundingLy2.setOmega(omega);
                    // soundingLy.setPressure(level.getPressure().floatValue()/100);
                    // soundingLy.setWindU(level.getUcWind().floatValue()); //
                    // HDF5 data in unit of Knots, no conversion needed
                    // soundingLy.setWindV(level.getVcWind().floatValue());
                    // soundingLy.setSpecHumidity(level.getSpecificHumidity().floatValue());
                    soundLy2List.add(soundingLy2);
                } catch (Exception e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }

            }
            // Collections.sort(soundLyList,reversePressureComparator());
            pf.setSoundingLyLst2(soundLy2List);
            pf.getSoundingLyLst().clear();
        }
    }

    private void sysPrintProfileLayer2(List<NcSoundingProfile> pfs) {
        System.out
                .println("-----------------------------------------------------------------\n sysPrintProfileLayer2: profile size ="
                        + pfs.size());
        for (int i = 0; i < pfs.size(); i++) {
            if (pfs.get(i).getStationId().indexOf('K') == 0) {
                System.out.println("pf" + i + " stn="
                        + pfs.get(i).getStationId() + " lat/lon="
                        + pfs.get(i).getStationLatitude() + "/"
                        + pfs.get(i).getStationLongitude());
                if (pfs.get(i).getSoundingLyLst2().size() > 0) {
                    System.out.println(" temp="
                            + pfs.get(i).getSoundingLyLst2().get(0)
                                    .getTemperature()
                            + " dewp="
                            + pfs.get(i).getSoundingLyLst2().get(0)
                                    .getDewpoint()
                            + " press="
                            + pfs.get(i).getSoundingLyLst2().get(0)
                                    .getPressure()
                            + " height="
                            + pfs.get(i).getSoundingLyLst2().get(0)
                                    .getGeoHeight()
                            + " windSp="
                            + pfs.get(i).getSoundingLyLst2().get(0)
                                    .getWindSpeed()
                            + " windDir="
                            + pfs.get(i).getSoundingLyLst2().get(0)
                                    .getWindDirection() + " omega="
                            + pfs.get(i).getSoundingLyLst2().get(0).getOmega());
                }
            }
        }
    }

}
