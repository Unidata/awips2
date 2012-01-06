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
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.common.dataplugin.h5uair.H5MaxWind;
import gov.noaa.nws.ncep.common.dataplugin.h5uair.H5ObsLevels;
import gov.noaa.nws.ncep.common.dataplugin.h5uair.H5Tropopause;
import gov.noaa.nws.ncep.common.dataplugin.h5uair.H5UairRecord;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer.DataType;
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
import java.util.Locale;
import java.util.Set;
import java.util.TimeZone;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

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

    private int[] dbIdList;

    private String[] stnIdArr;

    private String[] stnNumArr;

    private String modelName;

    private String pluginName;

    private float[][] latLonArray; // e.g. at nth element, lat=[n][0],
                                   // lon=[n][1]

    public float[][] getLatLons() {
        return latLonArray;
    }

    public String[] getStnIdArr() {
        return stnIdArr;
    }

    public void setStnIdArr(String[] stnIdArr) {
        this.stnIdArr = stnIdArr;
    }

    public String[] getStnNumArr() {
        return stnNumArr;
    }

    public void setStnNumArr(String[] stnNumArr) {
        this.stnNumArr = stnNumArr;
    }

    public void setLatLons(float[] latLons) {

        // from python script, I only know a way to pass one dimensional array,
        // therefore convert it 2-D here.
        if (latLons.length > 0) {
            latLonArray = new float[latLons.length / 2][2];
            for (int i = 0, j = 0; i < latLons.length; i++, j++) {
                this.latLonArray[j][0] = latLons[i];
                this.latLonArray[j][1] = latLons[++i];
                // System.out.println("latlons = "+ latLonArray[j][0] + ","+
                // latLonArray[j][1]);
            }
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
        /*
         * int year, mon, date, hr; int index = refTimeStr.indexOf('-'); if
         * (index >= 4 ){ year = Integer.parseInt(refTimeStr.substring(index-4,
         * index)); refTimeStr = refTimeStr.substring(index+1); index =
         * refTimeStr.indexOf('-'); if(index >= 2 ){ mon =
         * Integer.parseInt(refTimeStr.substring(index-2, index)); refTimeStr =
         * refTimeStr.substring(index+1); index = refTimeStr.indexOf(' ');
         * if(index >= 2 ){ date =
         * Integer.parseInt(refTimeStr.substring(index-2, index)); refTimeStr =
         * refTimeStr.substring(index+1); //index = refTimeStr.indexOf(':');
         * if(refTimeStr.length() >= 2 ){ hr =
         * Integer.parseInt(refTimeStr.substring(0, 2)); refTimeCal =
         * Calendar.getInstance(TimeZone.getTimeZone("GMT")); // reset time
         * refTimeCal.setTimeInMillis(0); // set new time refTimeCal.set(year,
         * mon-1, date, hr, 0,0); System.out.println("set time Str " +
         * refTimeStr + " cal time in GMT " + refTimeCal.getTime().toGMTString()
         * + " in msec = " + refTimeCal.getTimeInMillis()); } } } }
         */

    }

    public void setValidTimeStartStr(String validTimeStartStr) {
        this.validTimeStartStr = validTimeStartStr;
        validTimeStartCal = convertTimeStrToCalendar(validTimeStartStr);
        /*
         * int year, mon, date, hr; int index = validTimeStartStr.indexOf('-');
         * if (index >= 4 ){ year =
         * Integer.parseInt(validTimeStartStr.substring(index-4, index));
         * validTimeStartStr = validTimeStartStr.substring(index+1); index =
         * validTimeStartStr.indexOf('-'); if(index >= 2 ){ mon =
         * Integer.parseInt(validTimeStartStr.substring(index-2, index));
         * validTimeStartStr = validTimeStartStr.substring(index+1); index =
         * validTimeStartStr.indexOf(' '); if(index >= 2 ){ date =
         * Integer.parseInt(validTimeStartStr.substring(index-2, index));
         * validTimeStartStr = validTimeStartStr.substring(index+1); //index =
         * refTimeStr.indexOf(':'); if(validTimeStartStr.length() >= 2 ){ hr =
         * Integer.parseInt(validTimeStartStr.substring(0, 2));
         * validTimeStartCal =
         * Calendar.getInstance(TimeZone.getTimeZone("GMT")); // reset time
         * validTimeStartCal.setTimeInMillis(0); // set new time
         * validTimeStartCal.set(year, mon-1, date, hr, 0,0);
         * System.out.println("set valid time start Str " + validTimeStartStr +
         * " cal time in GMT " + validTimeStartCal.getTime().toGMTString() +
         * " in msec = " + validTimeStartCal.getTimeInMillis()); } } } }
         */

    }

    public void setValidTimeEndStr(String validTimeEndStr) {
        this.validTimeEndStr = validTimeEndStr;
        validTimeEndCal = convertTimeStrToCalendar(validTimeEndStr);
        /*
         * int year, mon, date, hr; int index = validTimeEndStr.indexOf('-'); if
         * (index >= 4 ){ year =
         * Integer.parseInt(validTimeEndStr.substring(index-4, index));
         * validTimeEndStr = validTimeEndStr.substring(index+1); index =
         * validTimeEndStr.indexOf('-'); if(index >= 2 ){ mon =
         * Integer.parseInt(validTimeEndStr.substring(index-2, index));
         * validTimeEndStr = validTimeEndStr.substring(index+1); index =
         * validTimeEndStr.indexOf(' '); if(index >= 2 ){ date =
         * Integer.parseInt(validTimeEndStr.substring(index-2, index));
         * validTimeEndStr = validTimeEndStr.substring(index+1); //index =
         * refTimeStr.indexOf(':'); if(validTimeEndStr.length() >= 2 ){ hr =
         * Integer.parseInt(validTimeEndStr.substring(0, 2)); validTimeEndCal =
         * Calendar.getInstance(TimeZone.getTimeZone("GMT")); // reset time
         * validTimeEndCal.setTimeInMillis(0); // set new time
         * validTimeEndCal.set(year, mon-1, date, hr, 0,0);
         * System.out.println("set valid time end Str " + validTimeEndStr +
         * " cal time in GMT " + validTimeEndCal.getTime().toGMTString() +
         * " in msec = " + validTimeEndCal.getTimeInMillis()); } } } }
         */

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

    /*
     * public Object execute() throws Exception { Object returnedObject = new
     * Object(); System.out.println ( " Enter execute "); MergeSounding ms = new
     * MergeSounding();
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
     * NcSoundingProfile pf = null;
     * if(sndType.equals(PfcSndType.NAMSND.toString()) ||
     * sndType.equals(PfcSndType.GFSSND.toString()) ||
     * sndType.equals(PfcSndType.RUC2SND.toString())) { System.out.println (
     * " Processing native model data"); pf =
     * PfcSoundingQuery.getPfcSndData(lat, lon, refTime, validTime, sndType);
     * ms.nativeModelSounding(pf.getSoundingLyLst(), pf.getStationElevation());
     * } else if (sndType.equals(ObsSndType.UAIR.toString()) ||
     * sndType.equals(ObsSndType.DROP.toString()) ||
     * sndType.equals(ObsSndType.TAMDAR.toString())) {
     * 
     * 
     * System.out.println (
     * " Processing UAIR data!  sndType could be UAIR, BUFRUA, DROP, TAMDAR, etc"
     * );
     * 
     * if(dbIdList!= null) { pf =
     * ObservedSoundingQuery.getObservedSndData(dbIdList, sndType, dataType); }
     * else {
     * 
     * if ( merge == 0 ) { // ms.unMergedUairSounding System.out.println (
     * " Request unmerged data");
     * 
     * pf = ObservedSoundingQuery.getObservedSndData(lat, lon, refTime, sndType,
     * "ALLDATA"); } else {
     * 
     * // Get TTAA. If not existent, try ship data (UUAA). If level is not null
     * or missing, // the body of code will return a sounding list with MAN data
     * or single level data. System.out.println ( " Request merged data");
     * 
     * // TO DO -----> add station ID and station number and a list of stations
     * queries. pf = ObservedSoundingQuery.getObservedSndData(lat, lon, refTime,
     * sndType, "TTAA"); ttaa = pf.getSoundingLyLst(); if (ttaa.size() == 0) {
     * ttaa = ObservedSoundingQuery.getObservedSndData(lat, lon, refTime,
     * sndType, "UUAA").getSoundingLyLst(); }
     * 
     * ttbb = ObservedSoundingQuery.getObservedSndData(lat, lon, refTime,
     * sndType, "TTBB").getSoundingLyLst(); if (ttbb.size() == 0) { ttbb =
     * ObservedSoundingQuery.getObservedSndData(lat, lon, refTime, sndType,
     * "UUBB").getSoundingLyLst(); }
     * 
     * ttcc = ObservedSoundingQuery.getObservedSndData(lat, lon, refTime,
     * sndType, "TTCC").getSoundingLyLst(); if (ttcc.size() == 0) { ttcc =
     * ObservedSoundingQuery.getObservedSndData(lat, lon, refTime, sndType,
     * "UUCC").getSoundingLyLst(); }
     * 
     * ttdd = ObservedSoundingQuery.getObservedSndData(lat, lon, refTime,
     * sndType, "TTDD").getSoundingLyLst(); if (ttdd.size() == 0) { ttdd =
     * ObservedSoundingQuery.getObservedSndData(lat, lon, refTime, sndType,
     * "UUDD").getSoundingLyLst(); }
     * 
     * ppaa = ObservedSoundingQuery.getObservedSndData(lat, lon, refTime,
     * sndType, "PPAA").getSoundingLyLst(); ppbb =
     * ObservedSoundingQuery.getObservedSndData(lat, lon, refTime, sndType,
     * "PPBB").getSoundingLyLst(); ppcc =
     * ObservedSoundingQuery.getObservedSndData(lat, lon, refTime, sndType,
     * "PPCC").getSoundingLyLst(); ppdd =
     * ObservedSoundingQuery.getObservedSndData(lat, lon, refTime, sndType,
     * "PPDD").getSoundingLyLst(); wmax_a =
     * ObservedSoundingQuery.getObservedSndData(lat, lon, refTime, sndType,
     * "MAXWIND_A").getSoundingLyLst(); wmax_c =
     * ObservedSoundingQuery.getObservedSndData(lat, lon, refTime, sndType,
     * "MAXWIND_C").getSoundingLyLst(); trop_a =
     * ObservedSoundingQuery.getObservedSndData(lat, lon, refTime, sndType,
     * "TROPOPAUSE_A").getSoundingLyLst(); trop_c =
     * ObservedSoundingQuery.getObservedSndData(lat, lon, refTime, sndType,
     * "TROPOPAUSE_C").getSoundingLyLst(); sls =
     * ms.mergeUairSounding(level,ttaa,
     * ttbb,ttcc,ttdd,ppaa,ppbb,ppcc,ppdd,trop_a,
     * trop_c,wmax_a,wmax_c,pf.getStationElevation()); pf.setSoundingLyLst(sls);
     * } } } else if(sndType.equals(ObsSndType.BUFRUA.toString())){ pf =
     * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, refTime,
     * dataType);
     * 
     * // TO DO: need to add "dataType" to bufr data
     * 
     * if ( merge == 0 ) { // ms.unMergedUairSounding System.out.println (
     * " Request unmerged data");
     * 
     * //// pf = ObservedSoundingQuery.getObservedSndBufruaData(lat, lon,
     * refTime, sndType, "ALLDATA"); } else {
     * 
     * // Get TTAA. If not existent, try ship data (UUAA). If level is not null
     * or missing, // the body of code will return a sounding list with MAN data
     * or single level data. System.out.println ( " Request merged data");
     * 
     * // TO DO -----> add station ID and station number and a list of stations
     * queries. //// pf = ObservedSoundingQuery.getObservedSndBufruaData(lat,
     * lon, refTime, sndType, "TTAA"); ttaa = pf.getSoundingLyLst(); if
     * (ttaa.size() == 0) { //// ttaa =
     * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, refTime,
     * sndType, "UUAA").getSoundingLyLst(); }
     * 
     * //// ttbb = ObservedSoundingQuery.getObservedSndBufruaData(lat, lon,
     * refTime, sndType, "TTBB").getSoundingLyLst(); if (ttbb.size() == 0) {
     * //// ttbb = ObservedSoundingQuery.getObservedSndBufruaData(lat, lon,
     * refTime, sndType, "UUBB").getSoundingLyLst(); }
     * 
     * //// ttcc = ObservedSoundingQuery.getObservedSndBufruaData(lat, lon,
     * refTime, sndType, "TTCC").getSoundingLyLst(); if (ttcc.size() == 0) {
     * //// ttcc = ObservedSoundingQuery.getObservedSndBufruaData(lat, lon,
     * refTime, sndType, "UUCC").getSoundingLyLst(); }
     * 
     * //// ttdd = ObservedSoundingQuery.getObservedSndBufruaData(lat, lon,
     * refTime, sndType, "TTDD").getSoundingLyLst(); if (ttdd.size() == 0) {
     * //// ttdd = ObservedSoundingQuery.getObservedSndBufruaData(lat, lon,
     * refTime, sndType, "UUDD").getSoundingLyLst(); }
     * 
     * //// ppaa = ObservedSoundingQuery.getObservedSndBufruaData(lat, lon,
     * refTime, sndType, "PPAA").getSoundingLyLst(); //// ppbb =
     * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, refTime,
     * sndType, "PPBB").getSoundingLyLst(); //// ppcc =
     * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, refTime,
     * sndType, "PPCC").getSoundingLyLst(); //// ppdd =
     * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, refTime,
     * sndType, "PPDD").getSoundingLyLst(); //// wmax_a =
     * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, refTime,
     * sndType, "MAXWIND_A").getSoundingLyLst(); //// wmax_c =
     * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, refTime,
     * sndType, "MAXWIND_C").getSoundingLyLst(); //// trop_a =
     * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, refTime,
     * sndType, "TROPOPAUSE_A").getSoundingLyLst(); //// trop_c =
     * ObservedSoundingQuery.getObservedSndBufruaData(lat, lon, refTime,
     * sndType, "TROPOPAUSE_C").getSoundingLyLst(); sls =
     * ms.mergeUairSounding(level
     * ,ttaa,ttbb,ttcc,ttdd,ppaa,ppbb,ppcc,ppdd,trop_a,
     * trop_c,wmax_a,wmax_c,pf.getStationElevation()); pf.setSoundingLyLst(sls);
     * } }
     * 
     * 
     * else {
     * 
     * pf = null; } returnedObject = pf; return returnedObject; }
     */
    // for static sounding type query
    public Object getSoundingRangeTimeLine() throws Exception {
        Object returnedObject = null;
        if (sndType.equals(ObsSndType.UAIR.toString())
                || sndType.equals(ObsSndType.H5UAIR.toString())
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
        if (sndType.equals(ObsSndType.UAIR.toString())
                || sndType.equals(ObsSndType.H5UAIR.toString())
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
        if (sndType.equals(ObsSndType.UAIR.toString())
                || sndType.equals(ObsSndType.H5UAIR.toString())
                || sndType.equals(ObsSndType.BUFRUA.toString())
                || sndType.equals(ObsSndType.DROP.toString())
                || sndType.equals(ObsSndType.TAMDAR.toString())) {
            stnInfoCol = ObservedSoundingQuery.getObservedSndStnInfoCol(
                    sndType, timeLine);
        } else if (sndType.equals(PfcSndType.NAMSND.toString())
                || sndType.equals(PfcSndType.GFSSND.toString())
                || sndType.equals(PfcSndType.RUCPTYPSND.toString())
                || sndType.equals(PfcSndType.RUC2SND.toString())) {
            stnInfoCol = PfcSoundingQuery
                    .getPfcSndStnInfoCol(sndType, timeLine);
        }

        else
            return returnedObject;

        returnedObject = stnInfoCol;
        return returnedObject;
    }

    private List<NcSoundingLayer> getSndLayersFromH5UairRecordObsLevel(
            H5UairRecord record) {
        List<NcSoundingLayer> sndLayers = new ArrayList<NcSoundingLayer>();
        Set<H5ObsLevels> obLevels = record.getObsLevels();

        if (obLevels.size() > 0) {
            for (H5ObsLevels obLev : obLevels) {
                NcSoundingLayer sndLayer = new NcSoundingLayer();
                sndLayer.setTemperature(obLev.getTemp());
                sndLayer.setDewpoint(obLev.getDwpt());
                sndLayer.setGeoHeight(obLev.getHght());
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

    private List<NcSoundingLayer> getSndLayersFromH5UairRecordTrop(
            H5UairRecord record) {
        List<NcSoundingLayer> sndLayers = new ArrayList<NcSoundingLayer>();
        Set<H5Tropopause> trops = record.getTropopause();
        if (trops.size() > 0) {
            for (H5Tropopause trop : trops) {
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

    private List<NcSoundingLayer> getSndLayersFromH5UairRecordMaxw(
            H5UairRecord record) {
        List<NcSoundingLayer> sndLayers = new ArrayList<NcSoundingLayer>();
        Set<H5MaxWind> maxWinds = record.getMaxWind();
        if (maxWinds.size() > 0) {
            for (H5MaxWind maxWind : maxWinds) {
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

    /*
     * This API is for getting multiple locations sounding info at one shot.
     * latLonArray is used to input lat/lon for each location. Use StnID as
     * input location is NOT SUPPORTED in this API
     */
    public Object getSoundingDataByLatLonArray() throws Exception {
        // long t01 = System.currentTimeMillis();
        Object returnedObject = new Object();
        // *System.out.println ( " getSoundingDataByLatLonArray ");
        if (latLonArray.length <= 0) {
            returnedObject = null;
            return returnedObject;
        }
        List<String> timeLineStrList = new ArrayList<String>();
        ;
        List<Calendar> timeLimeCalList = new ArrayList<Calendar>();
        if (validTimeEnd != 0 && validTimeEnd > validTimeStart) {
            // range of time line request
            timeLimeCalList = ObservedSoundingQuery
                    .getObservedSndTimeRangeList(sndType, validTimeStartCal,
                            validTimeEndCal);
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
        NcSoundingCube cube = new NcSoundingCube();
        List<NcSoundingProfile> soundingProfileList = new ArrayList<NcSoundingProfile>();
        SndQueryKeyType sndQuery = SndQueryKeyType.LATLON;
        NcSoundingCube.QueryStatus failedRtnStatus = NcSoundingCube.QueryStatus.FAILED;
        for (int i = 0; i < latLonArray.length; i++) {
            for (int j = 0; j < timeLineStrList.size(); j++) {
                String timeStr = timeLineStrList.get(j);
                Calendar timeCal = timeLimeCalList.get(j);

                MergeSounding ms = new MergeSounding();
                // make sure we have right precision...
                lat = Double.parseDouble(Float.toString(latLonArray[i][0]));
                lon = Double.parseDouble(Float.toString(latLonArray[i][1]));
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

                NcSoundingProfile pf = null;
                if (sndType.equals(PfcSndType.NAMSND.toString())
                        || sndType.equals(PfcSndType.GFSSND.toString())
                        || sndType.equals(PfcSndType.RUCPTYPSND.toString())
                        || sndType.equals(PfcSndType.RUC2SND.toString())) {

                    // *System.out.println ( " Processing native model data");
                    pf = PfcSoundingQuery.getPfcSndData2(lat, lon, "",
                            refTimeCal, validTimeStartCal, sndType, sndQuery);
                    // PfcSoundingQuery.getPfcSndData(lat, lon, "",refTimeCal,
                    // validTimeCal, sndType,sndQuery);
                    ms.nativeModelSounding(pf.getSoundingLyLst(),
                            pf.getStationElevation());
                } else if (sndType.equals(MdlSndType.ANY.toString())) {

                    pf = MdlSoundingQuery.getMdlSndData(lat, lon, refTimeStr,
                            validTimeStartStr, pluginName, modelName);
                    if (pf.getRtnStatus() != NcSoundingCube.QueryStatus.OK) {
                        failedRtnStatus = pf.getRtnStatus();
                        pf = null;
                    }
                } else if (sndType.equals(ObsSndType.H5UAIR.toString())) {
                    // System.out.println("getSoundingDataByLatLonArray:H5Uair: lat="+lat+
                    // " lon="+lon);
                    // make sure we have right precision...

                    if (merge == 0) {
                        // Chin...need more coding
                        pf = ObservedSoundingQuery.getObservedSndH5UairAllData(
                                lat, lon, "", timeStr, "TTAA", sndQuery);
                    } else {
                        long t001 = System.currentTimeMillis();

                        // get TTAA & TROPOPAUSE_A & MAXWIND_A
                        H5UairRecord record = ObservedSoundingQuery
                                .getObservedSndH5UairData(lat, lon, "",
                                        timeStr, "TTAA", sndQuery);
                        if (record != null) {
                            ttaa = getSndLayersFromH5UairRecordObsLevel(record);
                            trop_a = getSndLayersFromH5UairRecordTrop(record);
                            wmax_a = getSndLayersFromH5UairRecordMaxw(record);
                        }
                        // get TTCC & TROPOPAUSE_C & MAXWIND_C
                        record = ObservedSoundingQuery
                                .getObservedSndH5UairData(lat, lon, "",
                                        timeStr, "TTCC", sndQuery);
                        if (record != null) {
                            ttcc = getSndLayersFromH5UairRecordObsLevel(record);
                            trop_c = getSndLayersFromH5UairRecordTrop(record);
                            wmax_c = getSndLayersFromH5UairRecordMaxw(record);
                        }
                        // get TTBB
                        record = ObservedSoundingQuery
                                .getObservedSndH5UairData(lat, lon, "",
                                        timeStr, "TTBB", sndQuery);
                        if (record != null) {
                            ttbb = getSndLayersFromH5UairRecordObsLevel(record);
                        }
                        // get TTDD
                        record = ObservedSoundingQuery
                                .getObservedSndH5UairData(lat, lon, "",
                                        timeStr, "TTDD", sndQuery);
                        if (record != null) {
                            ttdd = getSndLayersFromH5UairRecordObsLevel(record);
                        }
                        // get PPAA
                        record = ObservedSoundingQuery
                                .getObservedSndH5UairData(lat, lon, "",
                                        timeStr, "PPAA", sndQuery);
                        if (record != null) {
                            ppaa = getSndLayersFromH5UairRecordObsLevel(record);
                        }
                        // get PPBB
                        record = ObservedSoundingQuery
                                .getObservedSndH5UairData(lat, lon, "",
                                        timeStr, "PPBB", sndQuery);
                        if (record != null) {
                            ppbb = getSndLayersFromH5UairRecordObsLevel(record);
                        }
                        // get PPCC
                        record = ObservedSoundingQuery
                                .getObservedSndH5UairData(lat, lon, "",
                                        timeStr, "PPCC", sndQuery);
                        if (record != null) {
                            ppcc = getSndLayersFromH5UairRecordObsLevel(record);
                        }
                        // get PPDD
                        record = ObservedSoundingQuery
                                .getObservedSndH5UairData(lat, lon, "",
                                        timeStr, "PPDD", sndQuery);
                        if (record != null) {
                            ppdd = getSndLayersFromH5UairRecordObsLevel(record);
                        }
                        pf = ObservedSoundingQuery.getObservedSndStnInfo(lat,
                                lon, "", sndType, timeCal, sndQuery);
                        long t02 = System.currentTimeMillis();
                        // System.out.println("H5UAIR profile retreival "+timeStr+
                        // " at lat="+lat+" lon="+lon+" took " + (t02 - t001));
                        sls = ms.mergeUairSounding(level, ttaa, ttbb, ttcc,
                                ttdd, ppaa, ppbb, ppcc, ppdd, trop_a, trop_c,
                                wmax_a, wmax_c, pf.getStationElevation());
                        // System.out.println("H5UAIR Number of Layers after merge:"+sls.size()
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
                                // System.out.println("H5UAIR get one layer using level = "+
                                // level);
                                pf.setSoundingLyLst(sls);
                            } else {
                                pf = null;
                                // System.out.println("H5UAIR get 0 layer using level = "+
                                // level);
                            }
                        } else {
                            if (sls.isEmpty() || sls.size() <= 1)
                                pf = null;
                            else
                                pf.setSoundingLyLst(sls);
                        }
                    }
                } else if (sndType.equals(ObsSndType.UAIR.toString())
                        || sndType.equals(ObsSndType.DROP.toString())
                        || sndType.equals(ObsSndType.TAMDAR.toString())) {

                    if (merge == 0) {
                        // ms.unMergedUairSounding
                        // *System.out.println ( " Request unmerged data");
                        if (dataType.equals(DataType.ALLDATA.toString()))
                            pf = ObservedSoundingQuery.getObservedSndAllData(
                                    lat, lon, "", timeCal, sndType, sndQuery);
                        else
                            pf = ObservedSoundingQuery.getObservedSndData(lat,
                                    lon, "", timeCal, sndType, dataType,
                                    sndQuery);

                    } else {

                        // Get TTAA. If not existent, try ship data (UUAA). If
                        // level is not null or missing,
                        // the body of code will return a sounding list with MAN
                        // data or single level data.
                        // *System.out.println ( " Request merged data at lat="+
                        // lat+" lon="+lon+ " refT="+
                        // refTimeCal.getTime().toGMTString());
                        long t001 = System.currentTimeMillis();
                        pf = ObservedSoundingQuery.getObservedSndData(lat, lon,
                                "", timeCal, sndType, "TTAA", sndQuery);
                        ttaa = pf.getSoundingLyLst();
                        if (ttaa.size() == 0) {
                            ttaa = ObservedSoundingQuery.getObservedSndData(
                                    lat, lon, "", timeCal, sndType, "UUAA",
                                    sndQuery).getSoundingLyLst();
                        }

                        ttbb = ObservedSoundingQuery.getObservedSndData(lat,
                                lon, "", timeCal, sndType, "TTBB", sndQuery)
                                .getSoundingLyLst();
                        if (ttbb.size() == 0) {
                            ttbb = ObservedSoundingQuery.getObservedSndData(
                                    lat, lon, "", timeCal, sndType, "UUBB",
                                    sndQuery).getSoundingLyLst();
                        }

                        ttcc = ObservedSoundingQuery.getObservedSndData(lat,
                                lon, "", timeCal, sndType, "TTCC", sndQuery)
                                .getSoundingLyLst();
                        if (ttcc.size() == 0) {
                            ttcc = ObservedSoundingQuery.getObservedSndData(
                                    lat, lon, "", timeCal, sndType, "UUCC",
                                    sndQuery).getSoundingLyLst();
                        }

                        ttdd = ObservedSoundingQuery.getObservedSndData(lat,
                                lon, "", timeCal, sndType, "TTDD", sndQuery)
                                .getSoundingLyLst();
                        if (ttdd.size() == 0) {
                            ttdd = ObservedSoundingQuery.getObservedSndData(
                                    lat, lon, "", timeCal, sndType, "UUDD",
                                    sndQuery).getSoundingLyLst();
                        }

                        ppaa = ObservedSoundingQuery.getObservedSndData(lat,
                                lon, "", timeCal, sndType, "PPAA", sndQuery)
                                .getSoundingLyLst();
                        ppbb = ObservedSoundingQuery.getObservedSndData(lat,
                                lon, "", timeCal, sndType, "PPBB", sndQuery)
                                .getSoundingLyLst();
                        ppcc = ObservedSoundingQuery.getObservedSndData(lat,
                                lon, "", timeCal, sndType, "PPCC", sndQuery)
                                .getSoundingLyLst();
                        ppdd = ObservedSoundingQuery.getObservedSndData(lat,
                                lon, "", timeCal, sndType, "PPDD", sndQuery)
                                .getSoundingLyLst();
                        wmax_a = ObservedSoundingQuery.getObservedSndData(lat,
                                lon, "", timeCal, sndType, "MAXWIND_A",
                                sndQuery).getSoundingLyLst();
                        wmax_c = ObservedSoundingQuery.getObservedSndData(lat,
                                lon, "", timeCal, sndType, "MAXWIND_C",
                                sndQuery).getSoundingLyLst();
                        trop_a = ObservedSoundingQuery.getObservedSndData(lat,
                                lon, "", timeCal, sndType, "TROPOPAUSE_A",
                                sndQuery).getSoundingLyLst();
                        trop_c = ObservedSoundingQuery.getObservedSndData(lat,
                                lon, "", timeCal, sndType, "TROPOPAUSE_C",
                                sndQuery).getSoundingLyLst();
                        pf = ObservedSoundingQuery.getObservedSndStnInfo(lat,
                                lon, "", sndType, timeCal, sndQuery);
                        long t02 = System.currentTimeMillis();
                        System.out.println("UAIR profile retreival " + timeStr
                                + "  took " + (t02 - t001));
                        sls = ms.mergeUairSounding(level, ttaa, ttbb, ttcc,
                                ttdd, ppaa, ppbb, ppcc, ppdd, trop_a, trop_c,
                                wmax_a, wmax_c, pf.getStationElevation());
                        // System.out.println("UAIR Number of Layers:"+sls.size());
                        if (level.toUpperCase().equalsIgnoreCase("MAN"))
                            pf.setSoundingLyLst(sls);
                        else if (ms.isNumber(level) >= 0) {
                            if (sls.size() == 1) {
                                pf.setSoundingLyLst(sls);
                            } else {
                                pf = null;
                            }
                        } else {
                            if (sls.isEmpty() || sls.size() <= 1)
                                pf = null;
                            else
                                pf.setSoundingLyLst(sls);
                        }

                    }

                } else if (sndType.equals(ObsSndType.BUFRUA.toString())) {
                    if (merge == 0) {
                        // ms.unMergedUairSounding
                        // *System.out.println ( " Request unmerged data");
                        if (dataType.equals(DataType.ALLDATA.toString()))
                            pf = ObservedSoundingQuery
                                    .getObservedSndBufruaAllData(lat, lon, "",
                                            refTimeCal, sndQuery);
                        else
                            pf = ObservedSoundingQuery
                                    .getObservedSndBufruaData(lat, lon, "",
                                            refTimeCal, dataType, sndQuery);

                    } else {

                        // Get TTAA. If not existent, try ship data (UUAA). If
                        // level is not null or missing,
                        // the body of code will return a sounding list with MAN
                        // data or single level data.
                        // *System.out.println ( " Request merged data at lat="+
                        // lat+" lon="+lon+ " refT="+
                        // refTimeCal.getTime().toGMTString());

                        pf = ObservedSoundingQuery.getObservedSndBufruaData(
                                lat, lon, "", refTimeCal, "TTAA", sndQuery);
                        ttaa = pf.getSoundingLyLst();
                        ttbb = ObservedSoundingQuery.getObservedSndBufruaData(
                                lat, lon, "", refTimeCal, "TTBB", sndQuery)
                                .getSoundingLyLst();
                        ttcc = ObservedSoundingQuery.getObservedSndBufruaData(
                                lat, lon, "", refTimeCal, "TTCC", sndQuery)
                                .getSoundingLyLst();
                        ttdd = ObservedSoundingQuery.getObservedSndBufruaData(
                                lat, lon, "", refTimeCal, "TTDD", sndQuery)
                                .getSoundingLyLst();
                        // ppaa =
                        // ObservedSoundingQuery.getObservedSndBufruaData(lat,
                        // lon, "", refTimeCal, "PPAA",
                        // sndQuery).getSoundingLyLst();
                        ppbb = ObservedSoundingQuery.getObservedSndBufruaData(
                                lat, lon, "", refTimeCal, "PPBB", sndQuery)
                                .getSoundingLyLst();
                        // ppcc =
                        // ObservedSoundingQuery.getObservedSndBufruaData(lat,
                        // lon, "", refTimeCal, "PPCC",
                        // sndQuery).getSoundingLyLst();
                        ppdd = ObservedSoundingQuery.getObservedSndBufruaData(
                                lat, lon, "", refTimeCal, "PPDD", sndQuery)
                                .getSoundingLyLst();
                        wmax_a = ObservedSoundingQuery
                                .getObservedSndBufruaData(lat, lon, "",
                                        refTimeCal, "MAXWIND_A", sndQuery)
                                .getSoundingLyLst();
                        wmax_c = ObservedSoundingQuery
                                .getObservedSndBufruaData(lat, lon, "",
                                        refTimeCal, "MAXWIND_C", sndQuery)
                                .getSoundingLyLst();
                        trop_a = ObservedSoundingQuery
                                .getObservedSndBufruaData(lat, lon, "",
                                        refTimeCal, "TROPOPAUSE_A", sndQuery)
                                .getSoundingLyLst();
                        trop_c = ObservedSoundingQuery
                                .getObservedSndBufruaData(lat, lon, "",
                                        refTimeCal, "TROPOPAUSE_C", sndQuery)
                                .getSoundingLyLst();
                        pf = ObservedSoundingQuery.getObservedSndStnInfo(lat,
                                lon, "", sndType, refTimeCal, sndQuery);
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
                                // System.out.println("H5UAIR get one layer using level = "+
                                // level);
                                pf.setSoundingLyLst(sls);
                            } else {
                                pf = null;
                                // System.out.println("H5UAIR get 0 layer using level = "+
                                // level);
                            }
                        } else {
                            if (sls.isEmpty() || sls.size() <= 1)
                                pf = null;
                            else
                                pf.setSoundingLyLst(sls);
                        }

                    }
                } else {

                    /*
                     * Invalid sounding type
                     */
                    pf = null;
                }
                if (pf != null && pf.getSoundingLyLst().size() > 0) {
                    soundingProfileList.add(pf);
                    // pf.setStationLatitude(lat.floatValue());
                    // pf.setStationLongitude(lon.floatValue());
                    pf = null;
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
     * This API is for getting multiple locations sounding info at one shot.
     * StnIdArray or StnNumArray is used as input stn info for each location.
     */
    public Object getSoundingDataByStnArray() throws Exception {
        Object returnedObject = new Object();
        // System.out.println ( " getSoundingData ");

        NcSoundingCube cube = new NcSoundingCube();
        List<NcSoundingProfile> soundingProfileList = new ArrayList<NcSoundingProfile>();
        SndQueryKeyType sndQuery;
        String[] stnArray;
        if (queryType.equals(SndQueryKeyType.STNID.toString())) {
            sndQuery = SndQueryKeyType.STNID;
            stnArray = stnIdArr.clone();

        } else if (queryType.equals(SndQueryKeyType.STNNUM.toString())) {
            sndQuery = SndQueryKeyType.STNNUM;
            stnArray = stnNumArr.clone();
        } else {
            returnedObject = null;
            return returnedObject;
        }
        if (stnArray.length <= 0) {
            returnedObject = null;
            return returnedObject;
        }
        List<String> timeLineStrList = new ArrayList<String>();
        ;
        List<Calendar> timeLimeCalList = new ArrayList<Calendar>();
        if (validTimeEnd != 0 && validTimeEnd > validTimeStart) {
            // range of time line request
            timeLimeCalList = ObservedSoundingQuery
                    .getObservedSndTimeRangeList(sndType, validTimeStartCal,
                            validTimeEndCal);
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
        String stn;
        for (int i = 0; i < stnArray.length; i++) {

            stn = stnArray[i];
            stn.toUpperCase(Locale.ENGLISH);
            // System.out.println ( "Request getSoundingData at " + stn);
            MergeSounding ms = new MergeSounding();
            for (int j = 0; j < timeLineStrList.size(); j++) {
                String timeStr = timeLineStrList.get(j);
                Calendar timeCal = timeLimeCalList.get(j);
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

                NcSoundingProfile pf = null;
                if (sndType.equals(ObsSndType.H5UAIR.toString())) {
                    // System.out.println("getSoundingDataByLatLonArray:H5Uair: lat="+lat+
                    // " lon="+lon);
                    if (merge == 0) {
                        // Chin...need more coding
                        pf = ObservedSoundingQuery.getObservedSndH5UairAllData(
                                0d, 0d, stn, timeStr, "TTAA", sndQuery);
                    } else {
                        /*
                         * long t001 = System.currentTimeMillis(); //get TTAA &
                         * TROPOPAUSE_A & MAXWIND_A H5UairRecord record =
                         * ObservedSoundingQuery.getObservedSndH5UairData(lat,
                         * lon, "", refTime, "TTAA", sndQuery); if(record !=
                         * null){ ttaa =
                         * getSndLayersFromH5UairRecordObsLevel(record); trop_a
                         * = getSndLayersFromH5UairRecordTrop(record); wmax_a =
                         * getSndLayersFromH5UairRecordMaxw(record); } //get
                         * TTCC & TROPOPAUSE_C & MAXWIND_C record =
                         * ObservedSoundingQuery.getObservedSndH5UairData(0d,
                         * 0d, stn, refTime, "TTCC", sndQuery); if(record !=
                         * null){ ttcc =
                         * getSndLayersFromH5UairRecordObsLevel(record); trop_c
                         * = getSndLayersFromH5UairRecordTrop(record); wmax_c =
                         * getSndLayersFromH5UairRecordMaxw(record); } //get
                         * TTBB record =
                         * ObservedSoundingQuery.getObservedSndH5UairData(0d,
                         * 0d, stn, refTime, "TTBB", sndQuery); if(record !=
                         * null){ ttbb =
                         * getSndLayersFromH5UairRecordObsLevel(record); } //get
                         * TTDD record =
                         * ObservedSoundingQuery.getObservedSndH5UairData(0d,
                         * 0d, stn, refTime, "TTDD", sndQuery); if(record !=
                         * null){ ttdd =
                         * getSndLayersFromH5UairRecordObsLevel(record); } //get
                         * PPAA record =
                         * ObservedSoundingQuery.getObservedSndH5UairData(0d,
                         * 0d, stn, refTime, "PPAA", sndQuery); if(record !=
                         * null){ ppaa =
                         * getSndLayersFromH5UairRecordObsLevel(record); } //get
                         * PPBB record =
                         * ObservedSoundingQuery.getObservedSndH5UairData(0d,
                         * 0d, stn, refTime, "PPBB", sndQuery); if(record !=
                         * null){ ppbb =
                         * getSndLayersFromH5UairRecordObsLevel(record); } //get
                         * PPCC record =
                         * ObservedSoundingQuery.getObservedSndH5UairData(0d,
                         * 0d, stn, refTime, "PPCC", sndQuery); if(record !=
                         * null){ ppcc =
                         * getSndLayersFromH5UairRecordObsLevel(record); } //get
                         * PPDD record =
                         * ObservedSoundingQuery.getObservedSndH5UairData(0d,
                         * 0d, stn, refTime, "PPDD", sndQuery); if(record !=
                         * null){ ppdd =
                         * getSndLayersFromH5UairRecordObsLevel(record); } pf =
                         * ObservedSoundingQuery.getObservedSndStnInfo(0d, 0d,
                         * stn,sndType, refTimeCal,sndQuery); long t02 =
                         * System.currentTimeMillis(); System.out.println(
                         * "H5UAIR (by stn) profile retreival  took " + (t02 -
                         * t001)); sls =
                         * ms.mergeUairSounding(level,ttaa,ttbb,ttcc
                         * ,ttdd,ppaa,ppbb
                         * ,ppcc,ppdd,trop_a,trop_c,wmax_a,wmax_c,
                         * pf.getStationElevation());
                         * //System.out.println("H5UAIR Number of Layers:"
                         * +sls.size()); //for(NcSoundingLayer ly: sls){ //
                         * System.out.println("Pre= "+ly.getPressure()+
                         * " Dew= "+ ly.getDewpoint()+ " T= "+
                         * ly.getTemperature(
                         * )+" H="+ly.getGeoHeight()+" WSp="+ly.getWindSpeed());
                         * //}
                         */
                        pf.setSoundingLyLst(sls);
                    }
                } else if (sndType.equals(ObsSndType.UAIR.toString())
                        || sndType.equals(ObsSndType.DROP.toString())
                        || sndType.equals(ObsSndType.TAMDAR.toString())) {

                    if (merge == 0) {
                        // ms.unMergedUairSounding
                        // System.out.println ( " Request unmerged data");
                        if (dataType.equals(DataType.ALLDATA.toString()))
                            pf = ObservedSoundingQuery.getObservedSndAllData(
                                    0d, 0d, stn, timeCal, sndType, sndQuery);
                        else
                            pf = ObservedSoundingQuery.getObservedSndData(0d,
                                    0d, stn, timeCal, sndType, dataType,
                                    sndQuery);

                    } else {

                        // Get TTAA. If not existent, try ship data (UUAA). If
                        // level is not null or missing,
                        // the body of code will return a sounding list with MAN
                        // data or single level data.
                        // *System.out.println ( " Request merged data at lat="+
                        // lat+" lon="+lon+ " refT="+
                        // refTimeCal.getTime().toGMTString());

                        // TO DO -----> add station ID and station number and a
                        // list of stations queries.
                        pf = ObservedSoundingQuery.getObservedSndData(0d, 0d,
                                stn, timeCal, sndType, "TTAA", sndQuery);
                        ttaa = pf.getSoundingLyLst();
                        if (ttaa.size() == 0) {
                            ttaa = ObservedSoundingQuery
                                    .getObservedSndData(0d, 0d, stn, timeCal,
                                            sndType, "UUAA", sndQuery)
                                    .getSoundingLyLst();
                        }

                        ttbb = ObservedSoundingQuery.getObservedSndData(0d, 0d,
                                stn, timeCal, sndType, "TTBB", sndQuery)
                                .getSoundingLyLst();
                        if (ttbb.size() == 0) {
                            ttbb = ObservedSoundingQuery
                                    .getObservedSndData(0d, 0d, stn, timeCal,
                                            sndType, "UUBB", sndQuery)
                                    .getSoundingLyLst();
                        }

                        ttcc = ObservedSoundingQuery.getObservedSndData(0d, 0d,
                                stn, timeCal, sndType, "TTCC", sndQuery)
                                .getSoundingLyLst();
                        if (ttcc.size() == 0) {
                            ttcc = ObservedSoundingQuery
                                    .getObservedSndData(0d, 0d, stn, timeCal,
                                            sndType, "UUCC", sndQuery)
                                    .getSoundingLyLst();
                        }

                        ttdd = ObservedSoundingQuery.getObservedSndData(0d, 0d,
                                stn, timeCal, sndType, "TTDD", sndQuery)
                                .getSoundingLyLst();
                        if (ttdd.size() == 0) {
                            ttdd = ObservedSoundingQuery
                                    .getObservedSndData(0d, 0d, stn, timeCal,
                                            sndType, "UUDD", sndQuery)
                                    .getSoundingLyLst();
                        }

                        ppaa = ObservedSoundingQuery.getObservedSndData(0d, 0d,
                                stn, timeCal, sndType, "PPAA", sndQuery)
                                .getSoundingLyLst();
                        ppbb = ObservedSoundingQuery.getObservedSndData(0d, 0d,
                                stn, timeCal, sndType, "PPBB", sndQuery)
                                .getSoundingLyLst();
                        ppcc = ObservedSoundingQuery.getObservedSndData(0d, 0d,
                                stn, timeCal, sndType, "PPCC", sndQuery)
                                .getSoundingLyLst();
                        ppdd = ObservedSoundingQuery.getObservedSndData(0d, 0d,
                                stn, timeCal, sndType, "PPDD", sndQuery)
                                .getSoundingLyLst();
                        wmax_a = ObservedSoundingQuery.getObservedSndData(0d,
                                0d, stn, timeCal, sndType, "MAXWIND_A",
                                sndQuery).getSoundingLyLst();
                        wmax_c = ObservedSoundingQuery.getObservedSndData(0d,
                                0d, stn, timeCal, sndType, "MAXWIND_C",
                                sndQuery).getSoundingLyLst();
                        trop_a = ObservedSoundingQuery.getObservedSndData(0d,
                                0d, stn, timeCal, sndType, "TROPOPAUSE_A",
                                sndQuery).getSoundingLyLst();
                        trop_c = ObservedSoundingQuery.getObservedSndData(0d,
                                0d, stn, timeCal, sndType, "TROPOPAUSE_C",
                                sndQuery).getSoundingLyLst();
                        pf = ObservedSoundingQuery.getObservedSndStnInfo(0d,
                                0d, stn, sndType, timeCal, sndQuery);
                        sls = ms.mergeUairSounding(level, ttaa, ttbb, ttcc,
                                ttdd, ppaa, ppbb, ppcc, ppdd, trop_a, trop_c,
                                wmax_a, wmax_c, pf.getStationElevation());
                        pf.setSoundingLyLst(sls);
                    }

                } else if (sndType.equals(ObsSndType.BUFRUA.toString())) {
                    if (merge == 0) {
                        // ms.unMergedUairSounding
                        // *System.out.println ( " Request unmerged data");
                        if (dataType.equals(DataType.ALLDATA.toString()))
                            pf = ObservedSoundingQuery
                                    .getObservedSndBufruaAllData(0d, 0d, stn,
                                            refTimeCal, sndQuery);
                        else
                            pf = ObservedSoundingQuery
                                    .getObservedSndBufruaData(0d, 0d, stn,
                                            refTimeCal, dataType, sndQuery);

                    } else {

                        // Get TTAA. If not existent, try ship data (UUAA). If
                        // level is not null or missing,
                        // the b0dy of c0de will return a sounding list with MAN
                        // data or single level data.
                        // *System.out.println ( " Request merged data at lat="+
                        // lat+" lon="+lon+ " refT="+
                        // refTimeCal.getTime().toGMTString());

                        pf = ObservedSoundingQuery.getObservedSndBufruaData(0d,
                                0d, stn, refTimeCal, "TTAA", sndQuery);
                        ttaa = pf.getSoundingLyLst();
                        ttbb = ObservedSoundingQuery.getObservedSndBufruaData(
                                0d, 0d, stn, refTimeCal, "TTBB", sndQuery)
                                .getSoundingLyLst();
                        ttcc = ObservedSoundingQuery.getObservedSndBufruaData(
                                0d, 0d, stn, refTimeCal, "TTCC", sndQuery)
                                .getSoundingLyLst();
                        ttdd = ObservedSoundingQuery.getObservedSndBufruaData(
                                0d, 0d, stn, refTimeCal, "TTDD", sndQuery)
                                .getSoundingLyLst();
                        // ppaa =
                        // ObservedSoundingQuery.getObservedSndBufruaData(0d,
                        // 0d, stn, refTimeCal, "PPAA",
                        // sndQuery).getSoundingLyLst();
                        ppbb = ObservedSoundingQuery.getObservedSndBufruaData(
                                0d, 0d, stn, refTimeCal, "PPBB", sndQuery)
                                .getSoundingLyLst();
                        // ppcc =
                        // ObservedSoundingQuery.getObservedSndBufruaData(0d,
                        // 0d, stn, refTimeCal, "PPCC",
                        // sndQuery).getSoundingLyLst();
                        ppdd = ObservedSoundingQuery.getObservedSndBufruaData(
                                0d, 0d, stn, refTimeCal, "PPDD", sndQuery)
                                .getSoundingLyLst();
                        wmax_a = ObservedSoundingQuery
                                .getObservedSndBufruaData(0d, 0d, stn,
                                        refTimeCal, "MAXWIND_A", sndQuery)
                                .getSoundingLyLst();
                        wmax_c = ObservedSoundingQuery
                                .getObservedSndBufruaData(0d, 0d, stn,
                                        refTimeCal, "MAXWIND_C", sndQuery)
                                .getSoundingLyLst();
                        trop_a = ObservedSoundingQuery
                                .getObservedSndBufruaData(0d, 0d, stn,
                                        refTimeCal, "TROPOPAUSE_A", sndQuery)
                                .getSoundingLyLst();
                        trop_c = ObservedSoundingQuery
                                .getObservedSndBufruaData(0d, 0d, stn,
                                        refTimeCal, "TROPOPAUSE_C", sndQuery)
                                .getSoundingLyLst();
                        pf = ObservedSoundingQuery.getObservedSndStnInfo(0d,
                                0d, stn, sndType, refTimeCal, sndQuery);
                        sls = ms.mergeUairSounding(level, ttaa, ttbb, ttcc,
                                ttdd, ppaa, ppbb, ppcc, ppdd, trop_a, trop_c,
                                wmax_a, wmax_c, pf.getStationElevation());
                        pf.setSoundingLyLst(sls);
                    }
                } else if (sndType.equals(PfcSndType.NAMSND.toString())
                        || sndType.equals(PfcSndType.GFSSND.toString())
                        || sndType.equals(PfcSndType.RUCPTYPSND.toString())
                        || sndType.equals(PfcSndType.RUC2SND.toString())) {
                    // *System.out.println ( " Processing native model data");
                    pf = PfcSoundingQuery.getPfcSndData(0d, 0d, stn,
                            refTimeCal, validTimeStartCal, sndType, sndQuery);
                    ms.nativeModelSounding(pf.getSoundingLyLst(),
                            pf.getStationElevation());

                } else if (sndType.equals(MdlSndType.ANY.toString())) {
                    // model sounding query by stn is not supported yet
                    pf = null;
                } else {

                    /*
                     * Invalid sounding type
                     */
                    pf = null;
                }
                if (pf != null && pf.getSoundingLyLst().size() > 0) {
                    soundingProfileList.add(pf);
                    pf.setStationId(stn);

                    pf = null;
                }
            }
        }
        if (soundingProfileList.size() == 0)
            cube.setRtnStatus(NcSoundingCube.QueryStatus.FAILED);
        else
            cube.setRtnStatus(NcSoundingCube.QueryStatus.OK);

        cube.setSoundingProfileList(soundingProfileList);
        returnedObject = cube;

        return returnedObject;
    }

    public Object getModels() throws Exception {
        Object returnedObject = new Object();
        NcSoundingModel mdls = MdlSoundingQuery.getMdls(tableName);
        returnedObject = mdls;
        return returnedObject;
    }

}
