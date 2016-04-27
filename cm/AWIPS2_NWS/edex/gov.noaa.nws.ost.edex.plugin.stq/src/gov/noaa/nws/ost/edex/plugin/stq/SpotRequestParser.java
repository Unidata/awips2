package gov.noaa.nws.ost.edex.plugin.stq;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;
import java.util.regex.Pattern;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.wmo.WMOHeader;

import gov.noaa.nws.ost.dataplugin.stq.SpotRequestRecord;

/**
 * Spot Forecast Request file parser
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#    Engineer    Description
 * ------------  ---------- ----------- --------------------------
 * July 29, 2015 DCS17366   pwang     	Initial creation
 * 
 * </pre>
 * 
 * @author pwang
 * @version 1.0
 */
public class SpotRequestParser {

    public static final String WMO_HEADER = "[A-Z]{3}[A-Z0-9](?:\\d{0,2}|[A-Z]{0,2})\\s+[A-Z0-9]{4}\\s+\\d{6}(?:[A-Z]{3})?";

    private static final Pattern WMO_HEADER_PATTERN = Pattern
            .compile(WMO_HEADER);

    private static final Pattern STQ_FILE_PATTERN = Pattern
            .compile("STQ[A-Z]{3}");

    private static final Pattern STQ_PROPERTY_PATTERN = Pattern
            .compile(".*:\\s+");

    /*
     * Reserved patterns
     * 
     * private static final Pattern STQ_WXCOND_PATTERN = Pattern
     * .compile("\\.*WEATHER\\s+CONDITIONS"); private static final Pattern
     * STQ_REMARK_PATTERN = Pattern .compile("\\.*REMARK"); private static final
     * Pattern STQ_WXPARMS_PATTERN = Pattern
     * .compile("\\.*WEATHER\\s+PARAMETERS");
     */
    private static final String PROPERTY_DELIMINATER = ":\\s+";

    private static final String DOT_DELIMINATER = "\\.";

    private static final String SLASH_DELIMINATER = "\\/";

    private static final Map<String, Integer> TIMEZONE = new HashMap<String, Integer>();
    static {
        TIMEZONE.put("EDT", 4);
        TIMEZONE.put("EST", 5);
        TIMEZONE.put("CDT", 5);
        TIMEZONE.put("CST", 6);
        TIMEZONE.put("MDT", 6);
        TIMEZONE.put("MST", 7);
        TIMEZONE.put("PDT", 7);
        TIMEZONE.put("PST", 8);
        TIMEZONE.put("GMT", 0);
    }

    private static final Map<String, String> PROPERTY_PATTERN_MAP = new HashMap<String, String>();
    static {
        PROPERTY_PATTERN_MAP.put("DATE_NAME", "DATE");
        PROPERTY_PATTERN_MAP.put("DATE_VALUE",
                "[0-9]{1,2}\\/[0-9]{1,2}\\/[0-9]{2}");
        PROPERTY_PATTERN_MAP.put("TIME_NAME", "TIME");
        PROPERTY_PATTERN_MAP.put("TIME_VALUE", "\\d{4}");
        PROPERTY_PATTERN_MAP.put("PROJ_NAME", "PROJECT NAME");
        PROPERTY_PATTERN_MAP.put("AGENCY_NAME", "REQUESTING\\s+AGENCY");
        PROPERTY_PATTERN_MAP.put("OFFICIAL_NAME", "REQUESTING\\s+OFFICIAL");
        PROPERTY_PATTERN_MAP.put("PHONE_NAME", "EMERGENCY\\s+PHONE");
        PROPERTY_PATTERN_MAP
                .put("PHONE_VALUE", "\\(\\d{3}\\)\\s*\\d{3}-\\d{4}");
        PROPERTY_PATTERN_MAP.put("STATE_NAME", "STATE");
        PROPERTY_PATTERN_MAP.put("DLAT_NAME", "DLAT");
        PROPERTY_PATTERN_MAP.put("DLON_NAME", "DLON");
        PROPERTY_PATTERN_MAP.put("LATLON_VALUE", "\\d+(\\.\\d+)?");
        PROPERTY_PATTERN_MAP.put("BELEV_NAME", "BOTTOM\\s+ELEVATION");
        PROPERTY_PATTERN_MAP.put("TELEV_NAME", "TOP\\s+ELEVATION");
        PROPERTY_PATTERN_MAP.put("SIZE_NAME", "SIZE\\s*\\(ACRES\\)");
        PROPERTY_PATTERN_MAP.put("SITE_NAME", "SITE");
        PROPERTY_PATTERN_MAP.put("OFILE_NAME", "OFILE");
        PROPERTY_PATTERN_MAP.put("OFILE_VALUE", "\\d{8}\\.\\w{5}\\.\\d{2}");
        PROPERTY_PATTERN_MAP.put("TIMEZONE_NAME", "TIMEZONE");
        PROPERTY_PATTERN_MAP.put("TIMEZONE_VALUE", "\\w{3}\\d{1}(\\w{3})?");
    }

    /** The logger */
    private static final IUFStatusHandler logger = UFStatus
            .getHandler(SpotRequestParser.class);

    private File stqIngestFile = null;

    private PointDataDescription pdd = null;

    private SpotRequestRecordDAO dao = null;

    private final Map<File, PointDataContainer> containerMap;

    private boolean isSTQFile = false;

    private String[] mmddyyyy;

    private WMOHeader wmoHeader;

    public SpotRequestParser(File stqFile, PointDataDescription pdd,
            SpotRequestRecordDAO dao) {
        this.stqIngestFile = stqFile;
        this.pdd = pdd;
        this.dao = dao;
        containerMap = new HashMap<File, PointDataContainer>();
    }

    /**
     * parse input stg file
     * 
     * @return
     * @throws DecoderException
     */
    public SpotRequestRecord parse() throws DecoderException {

        SpotRequestRecord srr = new SpotRequestRecord();

        SurfaceObsLocation location = new SurfaceObsLocation();
        srr.setLocation(location);

        BufferedReader br = null;
        String oneline = "";

        try {

            br = new BufferedReader(new FileReader(this.stqIngestFile));
            while ((oneline = br.readLine()) != null) {
                // Trim spaces of the line
                String line = oneline.trim();

                // Match WHMHeader
                if (WMO_HEADER_PATTERN.matcher(line).matches()) {
                    wmoHeader = new WMOHeader(line.getBytes(),
                            stqIngestFile.getName());
                    if (wmoHeader != null) {
                        srr.setWmoHeader(wmoHeader.getWmoHeader());
                    }

                } else if (STQ_FILE_PATTERN.matcher(line).matches()) {
                    /* Just check this marker and make sure it is a right ingest
                     * file
                     */
                    isSTQFile = true;
                } else if (STQ_PROPERTY_PATTERN.matcher(line).lookingAt()) {
                    // All lines with <nam string>:<space> <value string>
                    boolean status = parsePropertyLine(line, srr);
                    if (!status) {
                        // Missing required property
                        logger.error("STG Decoder error: invalid property value found");
                        throw new DecoderException(
                                "STQ Ingest file contains invalid property value");
                    }
                }
                /*
                 * Reserved else if
                 * (STQ_WXCOND_PATTERN.matcher(line).lookingAt()) { //Match
                 * reported weather conditions, reserved //do nothing continue;
                 * } else if(STQ_REMARK_PATTERN.matcher(line).lookingAt()) {
                 * //matched Remarks, reserved //do nothing continue; } else
                 * if(STQ_WXPARMS_PATTERN.matcher(line).lookingAt()) { //matched
                 * weather conditions, reserved //do nothing continue; }
                 */
            }

            // The file may not be a STQ file
            if (!isSTQFile) {
                logger.error("STG Decoder error: Ingest File "
                        + this.stqIngestFile + " is not a SRQ file!");
                throw new DecoderException("STQ Ingest file "
                        + this.stqIngestFile + " is not a STQ file");
            }

            // Build PointDataDescription

            PointDataContainer pdc = getContainer(srr);

            // Populate the point data.
            PointDataView view = pdc.append();

            view.setString("projectName", srr.getProjectName());
            view.setString("reqOfficial", srr.getReqOfficial());
            view.setString("emPhone", srr.getEmPhone());
            view.setString("timeZone", srr.getTimeZone());
            view.setString("state", srr.getState());
            view.setInt("bottomElevation", srr.getBottomElevation());
            view.setInt("topElevation", srr.getTopElevation());
            view.setInt("sizeAcres", srr.getSizeAcres());
            view.setString("stqSymbol", srr.getStqSymbol());

            srr.setPointDataView(view);

        } catch (FileNotFoundException e) {
            throw new DecoderException("STQ Ingest file " + this.stqIngestFile
                    + " is not found!", e);
        } catch (IOException e) {
            throw new DecoderException("I/O exception for reading "
                    + this.stqIngestFile, e);
        } finally {
            if (br != null) {
                try {
                    br.close();
                } catch (IOException e) {
                    throw new DecoderException("I/O exception for closing "
                            + this.stqIngestFile, e);
                }
            }
        }
        return srr;
    }

    /**
     * parsePropertyLine to property / value
     * 
     * @param line
     * @param stgPDO
     * @return false if failed to parse the line
     */
    private boolean parsePropertyLine(String line, SpotRequestRecord stgPDO) {
        boolean status = true;
        String[] property = line.split(PROPERTY_DELIMINATER);
        String propertyName = property[0].trim();
        String propertyValue;

        if (property.length > 1 && property[1] != null
                && property[1].trim().length() > 0) {
            // the property has a value
            propertyValue = property[1].trim();
        } else {
            // Invalid property line, something wrong of the ingest file
            return false;
        }

        // match and parse property pairs
        if (propertyName.matches(PROPERTY_PATTERN_MAP.get("DATE_NAME"))) {
           if (propertyValue.matches(PROPERTY_PATTERN_MAP
                    .get("DATE_VALUE"))) {
                String mmddyyyyString = "";
                try {
                    DateFormat informat = new SimpleDateFormat("MM/dd/yy");
                    DateFormat outformat = new SimpleDateFormat("MM/dd/yyyy");
                    mmddyyyyString = outformat.format(informat
                            .parse(propertyValue));
                    this.mmddyyyy = mmddyyyyString.split(SLASH_DELIMINATER);
                } catch (ParseException pe) {
                    logger.error("STQ Parser: failed to parse Date string: "
                            + pe);
                    status = false;
                }
           }
           else {
               //Date is required, return false to discontinue the parsing
               status = false;
           }
        } else if (propertyName.matches(PROPERTY_PATTERN_MAP.get("TIME_NAME"))) {
            if (propertyValue.matches(PROPERTY_PATTERN_MAP
                    .get("TIME_VALUE"))) {
                String HHs = propertyValue.substring(0, 2);
                String MMs = propertyValue.substring(2);

                int hours = Integer.parseInt(HHs);
                int minutes = Integer.parseInt(MMs);

                stgPDO.setDataTime(getRefTime(this.mmddyyyy, hours, minutes));
            }
            else {
                //Time is required, return false to discontinue the parsing
                logger.error("STQ Parser: Invalid TIME Value: " + propertyValue);
                status = false;
            }
        } else if (propertyName.matches(PROPERTY_PATTERN_MAP.get("PROJ_NAME"))) {
            //Project Name is required, but valuse is free text
            stgPDO.setProjectName(propertyValue);
        }
        else if (propertyName
                .matches(PROPERTY_PATTERN_MAP.get("OFFICIAL_NAME"))) {
            stgPDO.setReqOfficial(propertyValue);
        } else if (propertyName.matches(PROPERTY_PATTERN_MAP.get("PHONE_NAME"))) {
            if (propertyValue.matches(PROPERTY_PATTERN_MAP
                    .get("PHONE_VALUE"))) {
                stgPDO.setEmPhone(propertyValue);
            }
        } else if (propertyName.matches(PROPERTY_PATTERN_MAP.get("STATE_NAME"))) {
            stgPDO.setState(propertyValue);
        } else if (propertyName.matches(PROPERTY_PATTERN_MAP.get("DLAT_NAME"))) {
            if (propertyValue.matches(PROPERTY_PATTERN_MAP
                    .get("LATLON_VALUE"))) {
                float dlat = Float.parseFloat(propertyValue.trim());
                stgPDO.setLatitude(dlat);
            }
            else {
                //DLAT is required, return false to discontinue the parsing
                logger.error("STQ Parser: Invalid DLAT Value: " + propertyValue);
                status = false;
            }
        } else if (propertyName.matches(PROPERTY_PATTERN_MAP.get("DLON_NAME"))) {
            if (propertyValue.matches(PROPERTY_PATTERN_MAP
                    .get("LATLON_VALUE"))) {
                float dlon = Float.parseFloat(propertyValue.trim());
                //Ensure lon direction is West
                dlon = Math.abs(dlon) * -1;
                stgPDO.setLongitude(dlon);
            }
            else {
                //DLON is required, return false to discontinue the parsing
                logger.error("STQ Parser: Invalid DLON Value: " + propertyValue);
                status = false;
            }
        } else if (propertyName.matches(PROPERTY_PATTERN_MAP.get("BELEV_NAME"))) {
            int bottom_elevation = Integer.parseInt(propertyValue.trim());
            stgPDO.setBottomElevation(bottom_elevation);
        } else if (propertyName.matches(PROPERTY_PATTERN_MAP.get("TELEV_NAME"))) {
            int top_elevation = Integer.parseInt(propertyValue.trim());
            stgPDO.setTopElevation(top_elevation);
            stgPDO.getLocation().setElevation(top_elevation);
        } else if (propertyName.matches(PROPERTY_PATTERN_MAP.get("SIZE_NAME"))) {
            int size = Integer.parseInt(propertyValue.trim());
            stgPDO.setSizeAcres(size);
        } else if (propertyName.matches(PROPERTY_PATTERN_MAP.get("SITE_NAME"))) {
            stgPDO.setSite(propertyValue);
        } else if (propertyName.matches(PROPERTY_PATTERN_MAP.get("OFILE_NAME"))) {
            if (propertyValue.matches(PROPERTY_PATTERN_MAP
                    .get("OFILE_VALUE"))) {
                String[] ofileArray = propertyValue.split(DOT_DELIMINATER);
                stgPDO.setOfileKey(ofileArray[1]);
                stgPDO.setOfileVersion(ofileArray[2]);
                stgPDO.getLocation()
                        .setStationId(ofileArray[1] + ofileArray[2]);
            }
            else {
                //OFILE is required, return false to discontinue the parsing
                logger.error("STQ Parser: Invalid OFILE Value: " + propertyValue);
                status = false;
            }
        } else if (propertyName.matches(PROPERTY_PATTERN_MAP
                .get("TIMEZONE_NAME"))) {
            if (propertyValue.matches(PROPERTY_PATTERN_MAP
                    .get("TIMEZONE_VALUE"))) {
                stgPDO.setTimeZone(propertyValue);
            }
        }

        return status;
    }

    /**
     * Gets Container
     * 
     * @param obsData
     * @return
     */
    private PointDataContainer getContainer(SpotRequestRecord stqData) {

        File file = dao.getFullFilePath(stqData);
        PointDataContainer container = containerMap.get(file);
        if (container == null) {
            container = PointDataContainer.build(pdd);
            containerMap.put(file, container);
        }
        return container;
    }

    /**
     * Gets the ref time from the time field, month, and year.
     * 
     * @param time
     *            The time field.
     * @param month
     *            The month.
     * @param year
     *            The year.
     * @return The ref time, or null if the time field is null.
     */
    private DataTime getRefTime(String[] mmddyyyy, int hour, int minute) {
        if (mmddyyyy == null || mmddyyyy.length < 3) {
            return null;
        }

        int year = Integer.parseInt(mmddyyyy[2]);
        int month = Integer.parseInt(mmddyyyy[0]);
        int day = Integer.parseInt(mmddyyyy[1]);

        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.set(Calendar.YEAR, year);
        cal.set(Calendar.MONTH, month - 1);
        cal.set(Calendar.DAY_OF_MONTH, day);
        cal.set(Calendar.HOUR_OF_DAY, hour);
        cal.set(Calendar.MINUTE, minute);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        cal.setTimeZone(TimeZone.getTimeZone("GMT"));
        return new DataTime(cal);
    }

}
