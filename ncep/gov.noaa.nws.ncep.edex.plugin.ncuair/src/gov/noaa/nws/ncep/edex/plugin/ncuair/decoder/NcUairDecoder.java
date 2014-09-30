package gov.noaa.nws.ncep.edex.plugin.ncuair.decoder;

import gov.noaa.nws.ncep.common.dataplugin.ncuair.NcUairRecord;
import gov.noaa.nws.ncep.edex.plugin.ncuair.util.NcUairParser;
import gov.noaa.nws.ncep.edex.plugin.ncuair.util.NcUairShipMobile;
import gov.noaa.nws.ncep.edex.plugin.ncuair.util.NcUairTimeGroup;
import gov.noaa.nws.ncep.edex.util.UtilN;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;

//import gov.noaa.nws.ncep.edex.tools.decoder.SnsTnsLocTbl;

/**
 * NcUairDecoder
 * 
 * This java class provides the Decoder class for upper air sounding data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket# Engineer    Description
 * ------------ ------- ----------- -------------------------------------------
 * 03/2010      210     L. Lin      Initial coding
 * 05/2010      210     L. Lin      Migration to TO11DR11
 * 4/2011       210     T. Lee      Persisted to HDF5
 * 09/2011              Chin Chen   add batch decode methods for better
 *                                  performance
 * 09/2011      457     S. Gurung   Renamed H5 to Nc and h5 to nc
 * 10/2011              S. Gurung   Get stid/lat/lon/elev from database instead
 *                                  of snstns.xml file
 * 11/2011              S. Gurung   Fixed a NullPointerException bug
 * 11/2011              Q. Zhou     Handle multi-record for stationId, eliminate
 *                                  number-id if there is a non-number-id.
 * Aug 30, 2013 2298    rjpeter     Make getPluginName abstract
 * Jul 23, 2014 3410    bclement    location changed to floats
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * @author L. Lin
 * @version 1.0
 */
public class NcUairDecoder extends AbstractDecoder {
    private NcUairRecord record;

    List<NcUairRecord> recordList = new ArrayList<NcUairRecord>();

    /**
     * Constructor
     * 
     * @throws DecoderException
     */
    public NcUairDecoder() throws DecoderException {
    }

    /**
     * Retrieves an obs station with the specified wmo index
     * 
     * @param wmoIndex
     *            The wmo index
     * @return The obs station with the specified wmo index
     */
    public ObStation getStationInfo(Integer wmoIndex) {
        ObStation station = null;

        ObStationDao dao = new ObStationDao();
        if (dao != null) {
            List<?> obs = null;
            try {
                obs = dao
                        .queryBySingleCriteria("wmoIndex", wmoIndex.toString());
            } catch (DataAccessLayerException e) {
                System.out.println("Error while quering for wmoIndex: "
                        + wmoIndex);
                return null;
            }

            if ((obs != null) && (obs.size() > 0)) {

                for (int i = 0; i < obs.size(); i++) {
                    ObStation stationTemp = ((ObStation) obs.get(i));

                    try {
                        Integer.parseInt(stationTemp.getStationId());
                    } catch (Exception e) {
                        station = (ObStation) obs.get(i);
                    }
                }

                if (station == null) {
                    station = (ObStation) obs.get(0);
                }
            }

        }
        return station;
    }

    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {
        byte[] messageData = null;
        String traceId = null;
        WMOHeader hd;
        Boolean nil = false;
        Boolean ship = false;
        Boolean drop = false;
        System.out.println("Nc uair decode entered, data size= " + data.length);
        // long curTime = System.currentTimeMillis();
        if (headers != null) {
            /*
             * traceId equals to the file name
             */
            traceId = (String) headers.get("traceId");
        }

        /*
         * Check if there are more bulletins.
         */
        NcUairSeparator sep = NcUairSeparator.separate(data, headers);

        if (sep.hasNext()) {
            messageData = sep.next();
        } else {
            throw new DecoderException("uair -Out of data");
        }
        String theMessage = new String(messageData);
        record = new NcUairRecord();

        /*
         * Set issue time and bull message.
         */
        hd = new WMOHeader(messageData);
        Calendar cal = null;
        Calendar issueTime = UtilN.findDataTime(hd.getYYGGgg(), cal);
        record.setIssueTime(issueTime);
        // Replace special characters to a blank so that it may be readable.
        record.setBullMessage(UtilN.removeLeadingWhiteSpaces(theMessage
                .replace('\r', ' ').replace('\003', ' ').replace('\000', ' ')
                .replace('\001', ' ')));
        // Get and set dataType
        String dataType = NcUairParser.getDataType(theMessage);

        record.setDataType(dataType);
        record.setReportType("UAIR");
        record.setWmoHeader(hd.getWmoHeader());
        record.setCorr(NcUairParser.findCorIndicator(theMessage));

        String stationNumber = null;
        if (dataType != null) {
            String p1 = dataType.substring(0, 2);
            if (p1.equals("UU")) {
                ship = true;
            } else if (p1.equals("XX")) {
                drop = true;
            } else {
                stationNumber = NcUairParser.getStationNumber(theMessage);
            }
        }
        record.setStnum(stationNumber);

        /* Regular expression for "nil" */
        final String NIL = "(NIL)";
        Pattern nilPattern = Pattern.compile(NIL);
        Matcher nilMatcher = nilPattern.matcher(theMessage);

        if (nilMatcher.find()) {
            nil = true;
            record.setNil(true);
        }

        Calendar obsTime = null;

        // set station id, lat, lon
        if (stationNumber != null) {

            ObStation station = getStationInfo(Integer.parseInt(stationNumber));

            if ((station != null)
                    && ((station.getStationId() != null) && (station
                            .getStationId().length() > 0))) {
                SurfaceObsLocation obsLoc = new SurfaceObsLocation(
                        station.getStationId());
                if (station.getGeometry() != null) {
                    float lat = (float) station.getGeometry().getY();
                    float lon = (float) station.getGeometry().getX();
                    obsLoc.assignLocation(lat, lon);
                    Integer elev = station.getElevation();
                    if (elev == null) {
                        elev = -9999;
                    }
                    obsLoc.setElevation(elev);
                    record.setLocation(obsLoc);
                } else if (station.getUpperAirGeometry() != null) {
                    float lat = (float) station.getUpperAirGeometry().getY();
                    float lon = (float) station.getUpperAirGeometry().getX();
                    obsLoc.assignLocation(lat, lon);
                    Integer elev = station.getUpperAirElevation();
                    if (elev == null) {
                        elev = -9999;
                    }
                    obsLoc.setElevation(elev);
                    record.setLocation(obsLoc);
                } else {
                    logger.info("Latitude/Longitude (the_geom) missing for Station Number ["
                            + stationNumber + "]. Record discarded.");
                    record = null;
                }

            } else {
                // Couldn't find stnum (wmoindex) in spatial table
                logger.info("Station Number not found [" + stationNumber
                        + "]. Record discarded.");
                record = null;
            }
            if (record != null) {
                // Get time group
                NcUairTimeGroup.TimeField(theMessage);
                obsTime = NcUairTimeGroup.getObsTime();
                record.setObsTime(obsTime);
                record.setSynopticTime(NcUairTimeGroup.getSynopticTime());
                record.setUTC(NcUairTimeGroup.getIutc());
            }
        }

        if (!nil && (record != null)) {
            /* Regular expression for UAIR report */
            final String UAIR_REPORT = "(TT|PP)(AA|BB|CC|DD) ( )?(\\d{5}|\\d{4}/) (\\d{5})(.*)=";
            Pattern reportPattern = Pattern
                    .compile(UAIR_REPORT, Pattern.DOTALL);
            Matcher reportMatcher = reportPattern.matcher(theMessage);

            final String UAIR_BULL = "(TT|PP)(AA|BB|CC|DD) ( )?(\\d{5}|\\d{4}/) (\\d{5})(.*)\\x03";
            Pattern bullPattern = Pattern.compile(UAIR_BULL, Pattern.DOTALL);
            Matcher bullMatcher = bullPattern.matcher(theMessage);

            String codeMessage = null;
            if (reportMatcher.find()) {
                codeMessage = reportMatcher.group(6);
                if (codeMessage.length() > 4) {
                    codeMessage = UtilN.removeExtraWhiteSpaces(codeMessage
                            .replace('\r', ' ').replace('\n', ' '));
                }
            } else if (bullMatcher.find()) {
                codeMessage = bullMatcher.group(6);
                if (codeMessage.length() > 4) {
                    codeMessage = UtilN.removeExtraWhiteSpaces(codeMessage
                            .replace('\r', ' ').replace('\n', ' ')
                            .replace('\003', ' '));
                }
            } else {
                // Handle ship or dropsonde data
                final String SHIP = "(UU|XX)(AA|BB|CC|DD) (.*)(=|\\x03)";
                Pattern shipPattern = Pattern.compile(SHIP, Pattern.DOTALL);
                Matcher shipMatcher = shipPattern.matcher(theMessage);
                if (shipMatcher.find()) {
                    codeMessage = shipMatcher.group(3);
                    codeMessage = UtilN.removeExtraWhiteSpaces(codeMessage
                            .replace('\r', ' ').replace('\n', ' ')
                            .replace('\003', ' '));

                    NcUairShipMobile.ShipMobileField(codeMessage, dataType);
                    codeMessage = NcUairShipMobile.getCodeMessage();

                    SurfaceObsLocation obsLoc = new SurfaceObsLocation(
                            NcUairShipMobile.getStnId());
                    obsLoc.assignLocation(NcUairShipMobile.getXlat(),
                            NcUairShipMobile.getXlon());
                    Integer elev = NcUairShipMobile.getElevation();
                    if (elev == null) {
                        elev = -9999;
                    }
                    obsLoc.setElevation(elev);
                    record.setLocation(obsLoc);
                    record.setStnum(NcUairShipMobile.getStationNumber());

                    obsTime = NcUairShipMobile.getObsTime();
                    record.setObsTime(obsTime);
                    record.setSynopticTime(NcUairShipMobile.getSynopticTime());
                    record.setUTC(NcUairShipMobile.getIutc());
                }
            }

            // Parse UAIR report.
            if (codeMessage != null) {
                NcUairParser.getLevels(codeMessage, record);
            }

        }

        // set dataTime
        if (record != null) {
            if (obsTime != null) {
                DataTime dataTime = new DataTime(obsTime);
                record.setDataTime(dataTime);
            } else {
                DataTime dataTime = new DataTime(issueTime);
                record.setDataTime(dataTime);
            }
        }
        /*
         * Return the UairRecord record object.
         */
        if (record != null) {
            if (headers != null) {
                traceId = (String) headers.get("traceId");
            }
            record.setTraceId(traceId);
        }

        /*
         * Return UAIR record object.
         */
        if (record == null) {
            return new PluginDataObject[0];
        }
        // long enqueueTime = System.currentTimeMillis();
        // double latency = (enqueueTime - curTime);
        // System.out.println("Nc uair decode spend "+ latency);
        return new PluginDataObject[] { record };
    }

    /* chin: do batch decode and persistence to improve performance. */
    public PluginDataObject[] decodeBatch(byte[] data, Headers headers)
            throws DecoderException {
        byte[] messageData = null;
        PluginDataObject[] returnObjects = null;
        String traceId = null;
        WMOHeader hd;
        Boolean nil = false;
        System.out.println("Nc uair decodeBatch(), raw data size= "
                + data.length);
        // long curTime = System.currentTimeMillis();
        if (headers != null) {
            /*
             * traceId equals to the file name
             */
            traceId = (String) headers.get("traceId");
        }

        /*
         * separate data in to records strings.
         */
        NcUairSeparator sep = NcUairSeparator.separate(data, headers);

        while (sep.hasNext()) {
            nil = false;
            messageData = sep.next();
            // System.out.println("New message # "+ i);

            String theMessage = new String(messageData);

            record = new NcUairRecord();

            /*
             * Set issue time and bull message.
             */
            hd = new WMOHeader(messageData);
            Calendar cal = null;
            Calendar issueTime = UtilN.findDataTime(hd.getYYGGgg(), cal);
            record.setIssueTime(issueTime);
            // Replace special characters to a blank so that it may be readable.
            record.setBullMessage(UtilN.removeLeadingWhiteSpaces(theMessage
                    .replace('\r', ' ').replace('\003', ' ')
                    .replace('\000', ' ').replace('\001', ' ')));
            // Get and set dataType
            String dataType = NcUairParser.getDataType(theMessage);

            record.setDataType(dataType);
            record.setReportType("UAIR");
            record.setWmoHeader(hd.getWmoHeader());
            record.setCorr(NcUairParser.findCorIndicator(theMessage));

            String stationNumber = null;
            if (dataType != null) {
                String p1 = dataType.substring(0, 2);
                if (p1.equals("UU")) {
                    // ship = true;
                } else if (p1.equals("XX")) {
                    // drop = true;
                } else {
                    stationNumber = NcUairParser.getStationNumber(theMessage);
                }
            }
            record.setStnum(stationNumber);

            /* Regular expression for "nil" */
            final String NIL = "(NIL)";
            Pattern nilPattern = Pattern.compile(NIL);
            Matcher nilMatcher = nilPattern.matcher(theMessage);

            if (nilMatcher.find()) {
                nil = true;
                record.setNil(true);
            }

            Calendar obsTime = null;

            // set station id, lat, lon
            if (stationNumber != null) {

                ObStation station = getStationInfo(Integer
                        .parseInt(stationNumber));

                if ((station != null)
                        && ((station.getStationId() != null) && (station
                                .getStationId().length() > 0))) {
                    SurfaceObsLocation obsLoc = new SurfaceObsLocation(
                            station.getStationId());
                    if (station.getGeometry() != null) {
                        float lat = (float) station.getGeometry().getY();
                        float lon = (float) station.getGeometry().getX();
                        obsLoc.assignLocation(lat, lon);
                        Integer elev = station.getElevation();
                        if (elev == null) {
                            elev = -9999;
                        }
                        obsLoc.setElevation(elev);
                        record.setLocation(obsLoc);
                    } else if (station.getUpperAirGeometry() != null) {
                        float lat = (float) station.getUpperAirGeometry()
                                .getY();
                        float lon = (float) station.getUpperAirGeometry()
                                .getX();
                        obsLoc.assignLocation(lat, lon);
                        Integer elev = station.getUpperAirElevation();
                        if (elev == null) {
                            elev = -9999;
                        }
                        obsLoc.setElevation(elev);
                        record.setLocation(obsLoc);
                    } else {
                        // lat/lon (the_geom/upperairgeom) missing in spatial
                        // table
                        logger.info("Latitude/Longitude (the_geom) missing for Station Number ["
                                + stationNumber + "]. Record discarded.");
                        continue;
                    }

                } else {
                    // Couldn't find stnum (wmoindex) in spatial table
                    logger.info("Station Number not found [" + stationNumber
                            + "]. Record discarded.");
                    continue;
                }

                // Get time group
                NcUairTimeGroup.TimeField(theMessage);
                obsTime = NcUairTimeGroup.getObsTime();
                record.setObsTime(obsTime);
                record.setSynopticTime(NcUairTimeGroup.getSynopticTime());
                record.setUTC(NcUairTimeGroup.getIutc());
            }

            if (!nil && (record != null)) {
                // System.out.println(theMessage);
                /* Regular expression for UAIR report */
                final String UAIR_REPORT = "(TT|PP)(AA|BB|CC|DD) ( )?(\\d{5}|\\d{4}/) (\\d{5})(.*)=";
                Pattern reportPattern = Pattern.compile(UAIR_REPORT,
                        Pattern.DOTALL);
                Matcher reportMatcher = reportPattern.matcher(theMessage);

                final String UAIR_BULL = "(TT|PP)(AA|BB|CC|DD) ( )?(\\d{5}|\\d{4}/) (\\d{5})(.*)\\x03";
                Pattern bullPattern = Pattern
                        .compile(UAIR_BULL, Pattern.DOTALL);
                Matcher bullMatcher = bullPattern.matcher(theMessage);

                String codeMessage = null;
                if (reportMatcher.find()) {
                    // System.out.println("report found");
                    codeMessage = reportMatcher.group(6);
                    if (codeMessage.length() > 4) {
                        codeMessage = UtilN.removeExtraWhiteSpaces(codeMessage
                                .replace('\r', ' ').replace('\n', ' '));
                    }
                } else if (bullMatcher.find()) {
                    // System.out.println("bull found");
                    codeMessage = bullMatcher.group(6);
                    if (codeMessage.length() > 4) {
                        codeMessage = UtilN.removeExtraWhiteSpaces(codeMessage
                                .replace('\r', ' ').replace('\n', ' ')
                                .replace('\003', ' '));
                    }
                } else {
                    // Handle ship or dropsonde data
                    final String SHIP = "(UU|XX)(AA|BB|CC|DD) (.*)(=|\\x03)";
                    Pattern shipPattern = Pattern.compile(SHIP, Pattern.DOTALL);
                    Matcher shipMatcher = shipPattern.matcher(theMessage);
                    if (shipMatcher.find()) {
                        codeMessage = shipMatcher.group(3);
                        codeMessage = UtilN.removeExtraWhiteSpaces(codeMessage
                                .replace('\r', ' ').replace('\n', ' ')
                                .replace('\003', ' '));

                        NcUairShipMobile.ShipMobileField(codeMessage, dataType);
                        codeMessage = NcUairShipMobile.getCodeMessage();

                        SurfaceObsLocation obsLoc = new SurfaceObsLocation(
                                NcUairShipMobile.getStnId());
                        obsLoc.assignLocation(NcUairShipMobile.getXlat(),
                                NcUairShipMobile.getXlon());
                        Integer elev = NcUairShipMobile.getElevation();
                        if (elev == null) {
                            elev = -9999;
                        }
                        obsLoc.setElevation(elev);
                        record.setLocation(obsLoc);
                        record.setStnum(NcUairShipMobile.getStationNumber());

                        obsTime = NcUairShipMobile.getObsTime();
                        record.setObsTime(obsTime);
                        record.setSynopticTime(NcUairShipMobile
                                .getSynopticTime());
                        record.setUTC(NcUairShipMobile.getIutc());
                    }
                }

                // Parse UAIR report.
                if (codeMessage != null) {
                    NcUairParser.getLevels(codeMessage, record);
                }

            }

            if ((record != null) && (record.getLocation() != null)
                    && (record.getLocation().getStationId() == null)) {
                // skip the record if the stationId is null
                continue;
            }

            // set dataTime
            if (record != null) {
                if (obsTime != null) {
                    if (Math.abs(obsTime.get(Calendar.DAY_OF_MONTH)
                            - issueTime.get(Calendar.DAY_OF_MONTH)) >= 2) {
                        // Chin, not a good record, should just skip it
                        System.out.println("Nc uair record bad issue time "
                                + issueTime.getTime().toString() + " obs Time "
                                + obsTime.getTime().toString());
                        continue;
                    }

                    DataTime dataTime = new DataTime(obsTime);
                    record.setDataTime(dataTime);
                } else {
                    DataTime dataTime = new DataTime(issueTime);
                    record.setDataTime(dataTime);
                }
            }

            /*
             * Return the UairRecord record object.
             */
            if (record != null) {
                if (headers != null) {
                    traceId = (String) headers.get("traceId");
                }
                record.setTraceId(traceId);
                recordList.add(record);
            }

        }// end while loop
        /*
         * Return UAIR record object.
         */
        if (recordList.isEmpty()) {
            return new PluginDataObject[0];
        }
        returnObjects = new PluginDataObject[recordList.size()];
        recordList.toArray(returnObjects);
        recordList.clear();
        return returnObjects;
    }
}
