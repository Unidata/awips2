package gov.noaa.nws.ncep.edex.plugin.geomag;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagRecord;
import gov.noaa.nws.ncep.common.dataplugin.geomag.dao.GeoMagDao;
import gov.noaa.nws.ncep.common.dataplugin.geomag.exception.GeoMagException;
import gov.noaa.nws.ncep.common.dataplugin.geomag.table.GeoMagSource;
import gov.noaa.nws.ncep.common.dataplugin.geomag.table.GeoMagStation;
import gov.noaa.nws.ncep.common.dataplugin.geomag.table.Group;
import gov.noaa.nws.ncep.common.dataplugin.geomag.util.GeoMagStationLookup;
import gov.noaa.nws.ncep.common.dataplugin.geomag.util.TableTimeStamp;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * This java class decodes geomagnetic data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *                   
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/27/2013   975         sgurung     Initial Creation
 * 04/26/2013   975         qzhou       Added unit checkup. Declared missingVal.
 * 06/07/2013   975         qzhou       Fixed error on conversion
 * 07/16/2013   975         qzhou       Decoder redesign:  
 * 										Changed the data entries in postgreSQL to minute(1440 entries). 
 *                                      Changed data overwrite to insert new data. Added insertion loop.
 *                                      Changed the write to from hdf5 to post. Added 5 columns: H,D,Z,F and badData.
 *                                      Removed source and sourcePreference tables.  
 *                                      Get source priority from GeoMagStaiton.xml
 *                                      Added handles for same stations but with or without header
 *                                      Fixed HAD, NGK, CNB default value
 * Aug 30, 2013 2298       rjpeter      Make getPluginName abstract
 * </pre>
 * 
 * @author sgurung, qzhou
 * @version 1
 */

public class GeoMagDecoder extends AbstractDecoder {
    private GeoMagDao dao;

    private final Log logger = LogFactory.getLog(getClass());

    private final SimpleDateFormat obsTimeDateFormat = new SimpleDateFormat(
            "yyyy-MM-dd");

    private static final String STATION_CODE = "stationCode";

    private static final String OBS_DATE = "obsDate";

    private static final String OBS_YEAR = "obsYear";

    private static final String OBS_TIME = "obsTime";

    private static final String OBS_HOUR = "obsHour";

    private static final String OBS_MINUTE = "obsMinute";

    private static final String OBS_MINUTE_NUM = "obsMinuteNum";

    private static final String OBS_DAY_OF_YEAR = "obsDayOfYear";

    private static final String SOURCE = "source";

    private static final String COMPONENT_1 = "component1";

    private static final String COMPONENT_2 = "component2";

    private static final String COMPONENT_3 = "component3";

    private static final String COMPONENT_4 = "component4";

    private static final String UNIT = "unit";

    private static final float MISSING_VAL = 99999.99f;

    public PluginDataObject[] decode(File file) throws Exception {
        List<PluginDataObject> retData = new ArrayList<PluginDataObject>();
        GeoMagRecord record = null;
        int sourceId = 101;
        String stationCode = "";
        String suffix = "";

        String format = "yyyy-MM-dd'_'HH:mm:ss.s";
        SimpleDateFormat sdf = new SimpleDateFormat(format);

        List<Date> obsTimesList = new ArrayList<Date>();
        List<Float> comp1List = new ArrayList<Float>();
        List<Float> comp2List = new ArrayList<Float>();
        List<Float> comp3List = new ArrayList<Float>();
        List<Float> comp4List = new ArrayList<Float>();

        logger.info("******** Start meganetometer decoder.");

        if ((file == null) || (file.length() < 1)) {
            return new PluginDataObject[0];
        }

        BufferedReader in = null;

        try {
            String input;
            in = new BufferedReader(new FileReader(file));

            // get station code from the file name
            String fileName = file.getName();
            stationCode = fileName.substring(0, 3).toUpperCase();
            suffix = fileName.substring(fileName.indexOf(".") + 1,
                    fileName.length());

            // for Hartland (HAD), Korea (JEJ) data, filename does not have full
            // station code
            if (stationCode.startsWith("HA")) {
                stationCode = "HAD";
            } else if (stationCode.startsWith("MEA")) {
                stationCode = "MEA";
            } else if (stationCode.startsWith("M")) {
                stationCode = "JEJ";
            }

            // get the station detail from metadata file 'geoMagStations.xml'
            // File has header & end with min. File has no header & end with
            // min. File has no header & not end with min.
            GeoMagStation station = null;
            if (!suffix.equals("min")) {
                station = getStationDetail(stationCode, false);
            } else {
                station = getStationDetail(stationCode, true);
            }

            if (station == null) {
                logger.error("Error decoding geomag file! Station code not found in geoMagStations.xml file.");
                return new PluginDataObject[0];
            }

            boolean containsHeader = (station.getRawDataFormat()
                    .getHeaderFormat() != null) ? true : false;
            boolean containsData = (station.getRawDataFormat().getDataFormat() != null) ? true
                    : false;

            Pattern HEADER_EXP = null;
            Pattern DATA_EXP = null;
            boolean conversionRequired = false;
            HashMap<String, Group> headerGroupMap = new HashMap<String, Group>();
            HashMap<String, Group> dataGroupMap = new HashMap<String, Group>();

            /*
             * Get regular expression for the header from the station metadata
             * file
             */
            if (containsHeader) {
                HEADER_EXP = Pattern.compile(station.getRawDataFormat()
                        .getHeaderFormat().getPattern());

                Group[] headerGroup = station.getRawDataFormat()
                        .getHeaderFormat().getGroup();
                if (headerGroup != null) {
                    for (Group group : headerGroup) {
                        headerGroupMap.put(group.getName(), group);
                    }
                }
            }

            /*
             * Get regular expression for the data from the station metadata
             * file
             */
            if (containsData) {
                DATA_EXP = Pattern.compile(station.getRawDataFormat()
                        .getDataFormat().getPattern());

                Group[] dataGroup = station.getRawDataFormat().getDataFormat()
                        .getGroup();
                if (dataGroup != null) {
                    for (Group group : dataGroup) {
                        dataGroupMap.put(group.getName(), group);
                    }
                }
                conversionRequired = station.getRawDataFormat().getDataFormat()
                        .getConversionRequired();
            }

            boolean firstLine = true;
            String unit = "";
            // int idx = 0;
            DataTime headTime = null;
            Calendar obsTime = null;

            while ((input = in.readLine()) != null) {
                int groupId = -1;

                /*
                 * if this is the first line and header exists, parse the header
                 * information
                 */
                if (firstLine && containsHeader) {

                    Matcher headerMatcher = HEADER_EXP.matcher(input);

                    if (headerMatcher.find()) {
                        // set the station code
                        groupId = (headerGroupMap.get(STATION_CODE) != null) ? headerGroupMap
                                .get(STATION_CODE).getId() : -1;
                        if (groupId != -1) {
                            stationCode = headerMatcher.group(groupId);
                        }

                        // set the source
                        groupId = (headerGroupMap.get(SOURCE) != null) ? headerGroupMap
                                .get(SOURCE).getId() : -1;

                        if (groupId != -1) {
                            String source = headerMatcher.group(groupId);
                            ArrayList<GeoMagSource> src = getStationDetail(
                                    stationCode, true).getSource();
                            // System.out.println("***src "+src.size() +" "+
                            // stationCode);
                            for (int i = 0; i < src.size(); i++) {
                                String name = src.get(i).getName();
                                if (name.equalsIgnoreCase(source)) {
                                    sourceId = src.get(i).getPriority();
                                }
                            }
                        }

                        // get the unit
                        groupId = (headerGroupMap.get(UNIT) != null) ? headerGroupMap
                                .get(UNIT).getId() : -1;
                        if (groupId != -1) {
                            unit = headerMatcher.group(groupId);
                        }

                        // get the time
                        headTime = getRecordDataTime(headerMatcher,
                                headerGroupMap);
                    }
                }

                if (containsData) {
                    /* if data exists, parse the data information */
                    Matcher dataMatcher = DATA_EXP.matcher(input);

                    if (dataMatcher.find()) {
                        // if (dbLastTime == 0 || (dbLastTime != 0 && count >
                        // dbLastTime)) {

                        /* if this is the first line and header does not exist */
                        if (firstLine && !containsHeader) {
                            // set the station code, if it exists in the data
                            // section
                            groupId = (dataGroupMap.get(STATION_CODE) != null) ? dataGroupMap
                                    .get(STATION_CODE).getId() : -1;

                            if (groupId != -1) {
                                stationCode = dataMatcher.group(groupId);
                            }

                            headTime = getRecordDataTime(dataMatcher,
                                    dataGroupMap);

                            // if no header, the sourceId is 101
                            if (sourceId == 0) {
                                sourceId = 101;
                            }
                        }

                        firstLine = false;

                        Float comp1Val = null;
                        Float comp2Val = null;
                        Float comp3Val = null;
                        Float comp4Val = null;

                        String comp1RefersTo = null;
                        String comp2RefersTo = null;
                        // String comp3RefersTo = null;
                        // String comp4RefersTo = null;

                        // get the observation time for the minute data
                        obsTime = getObsTime(dataMatcher, dataGroupMap,
                                headTime.getRefTimeAsCalendar());

                        // get and set the component values (h or x, d or y ,z,
                        // f)
                        groupId = (dataGroupMap.get(COMPONENT_1) != null) ? dataGroupMap
                                .get(COMPONENT_1).getId() : -1;
                        if (groupId != -1) {
                            comp1RefersTo = dataGroupMap.get(COMPONENT_1)
                                    .getRefersTo();
                            comp1Val = Float.parseFloat(dataMatcher
                                    .group(groupId));
                        }

                        groupId = (dataGroupMap.get(COMPONENT_2) != null) ? dataGroupMap
                                .get(COMPONENT_2).getId() : -1;
                        if (groupId != -1) {
                            comp2RefersTo = dataGroupMap.get(COMPONENT_2)
                                    .getRefersTo();
                            comp2Val = Float.parseFloat(dataMatcher
                                    .group(groupId));
                        }

                        groupId = (dataGroupMap.get(COMPONENT_3) != null) ? dataGroupMap
                                .get(COMPONENT_3).getId() : -1;
                        if (groupId != -1) {
                            // comp3RefersTo =
                            // dataGroupMap.get(COMPONENT_3).getRefersTo();
                            comp3Val = Float.parseFloat(dataMatcher
                                    .group(groupId));
                            if (comp3Val == null) {
                                comp3Val = MISSING_VAL;
                            }
                        }

                        groupId = (dataGroupMap.get(COMPONENT_4) != null) ? dataGroupMap
                                .get(COMPONENT_4).getId() : -1;
                        if (groupId != -1) {
                            // comp4RefersTo =
                            // dataGroupMap.get(COMPONENT_4).getRefersTo();
                            comp4Val = Float.parseFloat(dataMatcher
                                    .group(groupId)); // for BGS
                            if (comp4Val == null) {
                                comp4Val = MISSING_VAL;
                            }
                        }

                        // process "abnormal" values
                        if (unit.equalsIgnoreCase("0.01nT")) {
                            // title line defined unit, e.g. 0.01nT
                            comp1Val = comp1Val / 100;
                            comp2Val = comp2Val / 100;
                            comp3Val = comp3Val / 100;
                            comp4Val = comp4Val / 100;
                        }

                        if (stationCode.equals("HAD")) { // HAD missing are
                                                         // 99999.9 and 999.999
                            if (comp1Val == 99999.9f) {
                                comp1Val = MISSING_VAL;
                            }
                            if (comp2Val == 999.999f) {
                                comp2Val = MISSING_VAL;
                            }
                            if (comp3Val == 99999.9f) {
                                comp3Val = MISSING_VAL;
                            }
                        }

                        if (stationCode.equals("CNB")) { // HAD missing are
                                                         // 99999.9 and 999.999
                            if (comp1Val == 99999.90f) {
                                comp1Val = MISSING_VAL;
                            }
                            if (comp2Val == 99999.90f) {
                                comp2Val = MISSING_VAL;
                            }
                            if (comp3Val == 99999.90f) {
                                comp3Val = MISSING_VAL;
                            }
                            if (comp4Val == 99999.90f) {
                                comp4Val = MISSING_VAL;
                            }
                        }

                        if (stationCode.equals("NGK")
                                || stationCode.equals("WNG")
                                || stationCode.equals("MEA")) { // NGK missing
                                                                // are 99999.00
                            if (comp1Val == 99999.00f) {
                                comp1Val = MISSING_VAL;
                            }
                            if (comp2Val == 99999.00f) {
                                comp2Val = MISSING_VAL;
                            }
                            if (comp3Val == 99999.00f) {
                                comp3Val = MISSING_VAL;
                            }
                            if (comp4Val == 99999.00f) {
                                comp4Val = MISSING_VAL;
                            }
                        }

                        if ((comp1Val != null) && (comp1Val != MISSING_VAL)
                                && (comp2Val != null)
                                && (comp2Val != MISSING_VAL)) {
                            if (conversionRequired) {
                                /*
                                 * Raw data from some providers might not be
                                 * reported in the appropriate format/units.
                                 * These data needs to be converted to northward
                                 * component (X) in nT and eastward component
                                 * (Y) in nT using the general formula: X = H
                                 * Cos D; Y = H Sin D;
                                 */
                                Float h = null;
                                Float d = null;

                                if ("H".equalsIgnoreCase(comp1RefersTo)
                                        && (comp1Val != null)) {
                                    h = comp1Val;
                                } else if ("D".equalsIgnoreCase(comp1RefersTo)
                                        && (comp1Val != null)) {
                                    d = comp1Val;
                                }

                                if ("H".equalsIgnoreCase(comp2RefersTo)
                                        && (comp2Val != null)) {
                                    h = comp2Val;
                                } else if ("D".equalsIgnoreCase(comp2RefersTo)
                                        && (comp2Val != null)) {
                                    d = comp2Val;
                                }

                                if ((h != null) && (d != null)) {
                                    comp1Val = (float) (h * Math.cos(Math
                                            .toRadians(d)));
                                    comp2Val = (float) (h * Math.sin(Math
                                            .toRadians(d)));
                                }

                            }

                            if (comp1Val != null) {
                                comp1List.add(comp1Val);
                            }
                            if (comp2Val != null) {
                                comp2List.add(comp2Val);
                            }
                            if (comp3Val != null) {
                                comp3List.add(comp3Val);
                            }
                            if (comp4Val != null) {
                                comp4List.add(comp4Val);
                            }
                            obsTimesList.add(obsTime.getTime());
                        }

                    } // if (dataMatcher.find())
                } // end if containData
            } // end while
        } catch (Exception e) {
            logger.error("Failed to decode file: [" + file.getAbsolutePath()
                    + "]", e);
        } finally {
            try {
                in.close();
            } catch (IOException e) {
                throw new GeoMagException("", e);
            }
        }

        for (int i = 0; i < obsTimesList.size(); i++) {
            record = new GeoMagRecord();

            // find this time in database
            Date time = obsTimesList.get(i);
            String newUriTime = new String(sdf.format(time));
            // System.out.println("**time "+obsTimesList.get(i)+" "+stationCode
            // +" "+sourceId);
            String newUri = "/geomag/" + newUriTime + "/" + stationCode + "/"
                    + sourceId + "/GEOMAG";

            List<?> resultsList = findUriFromDb(newUri);

            // set to record
            if ((resultsList == null) || resultsList.isEmpty()) {
                if (record.getStationCode() == null) {
                    record.setStationCode(stationCode);
                }
                if (record.getSourceId() == 0) {
                    record.setSourceId(sourceId);
                }
                record.setDataURI(newUri);

                record.setComponent_1(comp1List.get(i));
                record.setComponent_2(comp2List.get(i));
                if (!comp3List.isEmpty() && (comp3List.get(i) != null)) {
                    record.setComponent_3(comp3List.get(i));
                }
                if (!comp4List.isEmpty() && (comp4List.get(i) != null)) {
                    record.setComponent_4(comp4List.get(i));
                }
                record.setDataTime(new DataTime(time));

                record.setReportType("GEOMAG");
                record.setOverwriteAllowed(false);
                record.constructDataURI();
                // System.out.println("record.getDataURI() "+record.getDataURI()+" "+record.getDataTime().getRefTime()+" "+retData.size()
                // );
                retData.add(record);
            }
        }

        if (retData.isEmpty()) {
            return new PluginDataObject[0];
        } else {

            return retData.toArray(new PluginDataObject[retData.size()]);
        }
    }

    public IDataRecord[] findRecordFromDb(String newUri) {
        // find last obs_time in hdf5.
        // /geomag/2013-05-20_00:00:00.0/HAD/101/GEOMAG
        IDataRecord[] dataRec = null;
        IDataStore dataStore = null;
        GeoMagRecord record = null;

        DatabaseQuery query = new DatabaseQuery(GeoMagRecord.class.getName());
        query.addQueryParam("dataURI", newUri);

        List<?> resultsList = null;
        try {
            resultsList = dao.queryByCriteria(query);
        } catch (DataAccessLayerException e1) {
            e1.printStackTrace();
        }

        // find dataRec
        if ((resultsList != null) && (resultsList.size() != 0)) {

            record = new GeoMagRecord(newUri);
            if (record != null) {
                dataStore = dao.getDataStore(record);
            }

            try {
                // obs_time, compx...//size 7
                dataRec = dataStore.retrieve(newUri);
            } catch (FileNotFoundException e1) {
                e1.printStackTrace();
            } catch (StorageException e1) {
                e1.printStackTrace();
            }
        }
        return dataRec;
    }

    public List<?> findUriFromDb(String newUri) {

        DatabaseQuery query = new DatabaseQuery(GeoMagRecord.class.getName());
        query.addQueryParam("dataURI", newUri);

        List<?> resultsList = null;
        try {
            resultsList = dao.queryByCriteria(query);
        } catch (DataAccessLayerException e1) {
            e1.printStackTrace();
        }

        return resultsList;
    }

    public DataTime getRecordDataTime(Matcher matcher,
            HashMap<String, Group> groupMap) throws ParseException {

        int groupId = -1;

        String obsDateStr = null;
        String obsYearStr = null;
        String obsDayOfYearStr = null;

        String format = "dd-MMM-yy";

        Calendar cal = Calendar.getInstance();
        Date obsDate = cal.getTime();
        SimpleDateFormat inputDateFormat = new SimpleDateFormat(format);

        groupId = (groupMap.get(OBS_DATE) != null) ? groupMap.get(OBS_DATE)
                .getId() : -1;
        if (groupId != -1) {
            obsDateStr = matcher.group(groupId);
            format = (groupMap.get(OBS_DATE).getFormat() != null) ? groupMap
                    .get(OBS_DATE).getFormat() : format;
        }

        groupId = (groupMap.get(OBS_YEAR) != null) ? groupMap.get(OBS_YEAR)
                .getId() : -1;
        if (groupId != -1) {
            obsYearStr = matcher.group(groupId);
        }

        groupId = (groupMap.get(OBS_DAY_OF_YEAR) != null) ? groupMap.get(
                OBS_DAY_OF_YEAR).getId() : -1;
        if (groupId != -1) {
            obsDayOfYearStr = matcher.group(groupId);
        }

        // get Observation Date using obsDate
        if (obsDateStr != null) {
            inputDateFormat = new SimpleDateFormat(format);
            obsDate = obsTimeDateFormat.parse(obsTimeDateFormat
                    .format(inputDateFormat.parse(obsDateStr)));
        }

        // get Observation Date using obsYear and obsDayOfYear
        if ((obsYearStr != null) && (obsDayOfYearStr != null)) {
            Calendar tmpCal = Calendar.getInstance();
            tmpCal.set(Calendar.YEAR, Integer.parseInt(obsYearStr));
            tmpCal.set(Calendar.DAY_OF_YEAR, Integer.parseInt(obsDayOfYearStr));

            obsDate = obsTimeDateFormat.parse(obsTimeDateFormat.format(tmpCal
                    .getTime()));
        }

        cal.setTime(obsDate);

        DataTime dataTime = new DataTime(cal);

        return dataTime;
    }

    public Calendar getObsTime(Matcher matcher,
            HashMap<String, Group> groupMap, Calendar time)
            throws ParseException {

        int groupId = -1;

        String obsDateStr = null;
        String obsTimeStr = null;
        // String obsYearStr = null;
        // String obsDayOfYearStr = null;
        String obsHourStr = null;
        String obsMinuteStr = null;
        String obsMinuteNumStr = null;

        String dateFormat = "dd-MMM-yy";
        String timeFormat = "HH:mm:ss";
        SimpleDateFormat inputDateFormat = new SimpleDateFormat(dateFormat
                + " " + timeFormat);

        Calendar obsTime = time; // record.getDataTime().getRefTimeAsCalendar();

        groupId = (groupMap.get(OBS_DATE) != null) ? groupMap.get(OBS_DATE)
                .getId() : -1;
        if (groupId != -1) {
            obsDateStr = matcher.group(groupId);
            dateFormat = (groupMap.get(OBS_DATE).getFormat() != null) ? groupMap
                    .get(OBS_DATE).getFormat() : dateFormat;

        }

        groupId = (groupMap.get(OBS_TIME) != null) ? groupMap.get(OBS_TIME)
                .getId() : -1;
        if (groupId != -1) {
            obsTimeStr = matcher.group(groupId);
            timeFormat = (groupMap.get(OBS_TIME).getFormat() != null) ? groupMap
                    .get(OBS_TIME).getFormat() : timeFormat;
            // .out.println("***obsTimeStr "+obsTimeStr +" "+timeFormat);
        }

        // groupId = (groupMap.get(OBS_YEAR) != null)?
        // groupMap.get(OBS_YEAR).getId():-1;
        // if (groupId != -1) {
        // obsYearStr = matcher.group(groupId);
        // }
        //
        // groupId = (groupMap.get(OBS_DAY_OF_YEAR) != null)?
        // groupMap.get(OBS_DAY_OF_YEAR).getId():-1;
        // if (groupId != -1) {
        // obsDayOfYearStr = matcher.group(groupId);
        // if (obsYearStr != null && obsDayOfYearStr != null) {
        // Calendar tmpCal = Calendar.getInstance();
        // tmpCal.set(Calendar.YEAR, Integer.parseInt(obsYearStr));
        // tmpCal.set(Calendar.DAY_OF_YEAR, Integer.parseInt(obsDayOfYearStr));
        //
        // obsDateStr = obsTimeDateFormat.format(tmpCal.getTime());
        // System.out.println("***obsNumStr "+obsDateStr);
        // }
        // }

        groupId = (groupMap.get(OBS_MINUTE_NUM) != null) ? groupMap.get(
                OBS_MINUTE_NUM).getId() : -1;
        if (groupId != -1) {
            obsMinuteNumStr = matcher.group(groupId);

        }

        groupId = (groupMap.get(OBS_HOUR) != null) ? groupMap.get(OBS_HOUR)
                .getId() : -1;
        if (groupId != -1) {
            obsHourStr = matcher.group(groupId);
        }

        groupId = (groupMap.get(OBS_MINUTE) != null) ? groupMap.get(OBS_MINUTE)
                .getId() : -1;
        if (groupId != -1) {
            obsMinuteStr = matcher.group(groupId);
        }

        // get obsTime using obsMinuteNum
        if (obsMinuteNumStr != null) {
            obsTime.add(
                    Calendar.MINUTE,
                    (obsMinuteNumStr != null) ? Integer
                            .parseInt(obsMinuteNumStr) : 1);
        }
        // get obsTime using obsHour and obsMinute
        else if ((obsHourStr != null) && (obsMinuteStr != null)) {
            int minutes = (Integer.parseInt(obsHourStr) * 60)
                    + Integer.parseInt(obsMinuteStr);
            obsTime.add(Calendar.MINUTE, minutes);
        }

        // get obsTime using obsDate and obsTime
        else if ((obsDateStr != null) && (obsTimeStr != null)) {
            String obsDateTimeStr = obsDateStr + " " + obsTimeStr;
            inputDateFormat = new SimpleDateFormat(dateFormat + " "
                    + timeFormat);

            Date obsDateTime = inputDateFormat.parse(obsDateTimeStr);
            obsTime.setTime(obsDateTime);
        }

        return obsTime;
    }

    public GeoMagDao getDao() {
        return dao;
    }

    public void setDao(GeoMagDao dao) {
        this.dao = dao;
    }

    public GeoMagStation getStationDetail(String stnCode, boolean hasHeader)
            throws GeoMagException {
        GeoMagStation station = null;

        if (stnCode != null) {
            TableTimeStamp.updateXmlTables();
            station = GeoMagStationLookup.getInstance().getStationByCode(
                    stnCode, hasHeader);
        }

        return station;
    }

}