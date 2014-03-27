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
package com.raytheon.uf.edex.plugin.acars.decoder;

import static com.raytheon.uf.edex.decodertools.bufr.packets.DataPacketTypes.RepSubList;
import static com.raytheon.uf.edex.decodertools.bufr.packets.DataPacketTypes.SubSetList;
import static com.raytheon.uf.edex.plugin.acars.common.ACARSConstants.NO_ICING;
import static com.raytheon.uf.edex.plugin.acars.common.ACARSConstants.RESERVE_13;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.acars.ACARSRecord;
import com.raytheon.uf.common.pointdata.spatial.AircraftObsLocation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.BUFRDescriptor;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * Adapter used to decode ACARS data in BUFR format.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 22, 2009 1939       jkorman     Initial creation
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Sep 18, 2013 2339       njensen     Index safety check in getTailNumber()
 * Mar 27, 2014 2811       skorolev    Added check for empty message.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class ACARSDataAdapter {

    private IUFStatusHandler logger = UFStatus
            .getHandler(ACARSDataAdapter.class);

    private static final int MONTH_OFFSET = 1;

    private static final int DAY_OFFSET = 2;

    private static final int HOUR_OFFSET = 3;

    private static final int MINUTE_OFFSET = 4;

    private static final int SECOND_OFFSET = 5;

    // Map detailed flight phase [0-08-009] to flight phase [0-08-004]
    private static final int[] DETAIL_PHASE_MAP = { 3, 4, 2, 3, 4, 5, 6, 5, 5,
            5, 5, 6, 6, 6, 6, 7, };

    private final String pluginName;

    private String traceId = null;

    /**
     * 
     * @param name
     */
    public ACARSDataAdapter(String name) {
        pluginName = name;
    }

    /**
     * @param rawData
     * @param traceId
     * @param headers
     * @return
     */
    public PluginDataObject[] getACARSData(byte[] rawData, String traceId,
            Headers headers) {

        this.traceId = traceId;
        PluginDataObject[] returnObjects = null;
        List<ACARSRecord> records = null;

        Set<String> dataSet = new HashSet<String>();

        WMOHeader wmoHeader = new WMOHeader(rawData, headers);
        if (wmoHeader.isValid()) {

            records = new ArrayList<ACARSRecord>();

            ACARSParser parser = new ACARSParser(rawData, headers);
            while (parser.hasNext()) {

                ACARSRecord record = getDataRecord(parser.next());
                if (record != null) {
                    record.setWmoHeader(parser.getWmoHeader().getWmoHeader());
                    try {
                        record.constructDataURI();

                        String uri = record.getDataURI();
                        if (dataSet.add(uri)) {
                            records.add(record);
                            logger.debug(traceId + " -Adding " + uri);
                        } else {
                            logger.debug(traceId + " -Rejecting " + uri);
                        }

                    } catch (Exception e) {
                        logger.error(traceId + " -Unable to construct datauri",
                                e);
                    }
                }
            }
        }
        dataSet.clear();
        if (records != null) {
            returnObjects = records
                    .toArray(new PluginDataObject[records.size()]);
        }
        return returnObjects;
    }

    /**
     * 
     * @param data
     * @return
     */
    @SuppressWarnings("unchecked")
    private ACARSRecord getDataRecord(BUFRDataDocument data) {

        ACARSRecord rpt = null;
        if (data != null) {
            List<IBUFRDataPacket> subList = data.getList();

            Calendar timeObs = null;
            AircraftObsLocation loc = null;
            String tailNumber = null;
            if (!subList.isEmpty()) {
                IBUFRDataPacket packet = subList.get(0);
                int d = packet.getReferencingDescriptor().getDescriptor();
                if (d == BUFRDescriptor.createDescriptor(0, 1, 6)) {
                    packet = subList.get(1);
                    d = packet.getReferencingDescriptor().getDescriptor();
                    if (d == BUFRDescriptor.createDescriptor(0, 2, 61)) {
                        timeObs = getTimeObs(subList, 2, false);
                        if (timeObs != null) {
                            loc = getObsLocationFine(subList, 7);
                            tailNumber = getTailNumber(subList, 21);
                            if (loc != null) {
                                if (tailNumber != null) {

                                    rpt = new ACARSRecord();

                                    rpt.setTailNumber(tailNumber.trim());
                                    rpt.setLocation(loc);

                                    rpt.setTimeObs(timeObs);
                                    DataTime t = new DataTime(
                                            TimeTools.copy(timeObs));
                                    rpt.setDataTime(t);

                                    getFlightPhase(subList, rpt, 9);
                                    getWxDataA(subList, rpt);

                                    logger.debug(traceId
                                            + " -Observation time = " + timeObs);
                                } else {
                                    logger.error(traceId
                                            + " -No Aircraft tail number was found");
                                }

                            } else {
                                logger.error(traceId
                                        + " -No Aircraft location was found");
                            }
                        } else {
                            logger.error(traceId
                                    + " -No Observation time was found");
                        }
                    } else if (d == BUFRDescriptor.createDescriptor(0, 1, 8)) {
                        timeObs = getTimeObs(subList, 8, true);
                        if (timeObs != null) {
                            loc = getObsLocationCoarse(subList, 14);
                            tailNumber = getTailNumber(subList, 1);
                            if (loc != null) {
                                if (tailNumber != null) {
                                    rpt = new ACARSRecord();

                                    rpt.setTailNumber(tailNumber.trim());
                                    rpt.setLocation(loc);

                                    getReceiver(subList, rpt, 7);

                                    rpt.setTimeObs(timeObs);
                                    DataTime t = new DataTime(
                                            TimeTools.copy(timeObs));
                                    rpt.setDataTime(t);

                                    getFlightPhase(subList, rpt, 18);
                                    getWxDataC(subList, rpt, loc);
                                    getPressure(subList, rpt, 16);

                                    logger.debug(traceId
                                            + " -Observation time = " + timeObs);
                                } else {
                                    logger.error(traceId
                                            + " -No Aircraft tail number was found");
                                }

                            } else {
                                logger.error(traceId
                                        + " -No Aircraft location was found");
                            }
                        } else {
                            logger.error(traceId
                                    + " -No Observation time was found");
                        }
                    } else {
                        logger.error(traceId
                                + " - Unknown observation data following [0-01-006]");
                    }
                } else if (d == BUFRDescriptor.createDescriptor(0, 1, 8)) {

                    packet = subList.get(1);
                    d = packet.getReferencingDescriptor().getDescriptor();
                    if (d == BUFRDescriptor.createDescriptor(0, 4, 1)) {
                        timeObs = getTimeObs(subList, 1, true);
                        if (timeObs != null) {

                            loc = getObsLocationFine(subList, 7);
                            tailNumber = getTailNumber(subList, 0);

                            if (loc != null) {
                                if (tailNumber != null) {
                                    rpt = new ACARSRecord();

                                    rpt.setTailNumber(tailNumber.trim());
                                    rpt.setLocation(loc);

                                    rpt.setTimeObs(timeObs);
                                    DataTime t = new DataTime(
                                            TimeTools.copy(timeObs));
                                    rpt.setDataTime(t);

                                    getFlightPhase(subList, rpt, 9);

                                    IBUFRDataPacket wxData = subList.get(10);
                                    if (RepSubList.getPacketType().equals(
                                            wxData.getUnits())) {
                                        List<IBUFRDataPacket> dataList = (List<IBUFRDataPacket>) wxData
                                                .getValue();

                                        getWxDataB(dataList, rpt, loc);
                                    }

                                    logger.debug(traceId
                                            + " -Observation time = " + timeObs);
                                } else {
                                    logger.error(traceId
                                            + " -No Aircraft tail number was found");
                                }
                            } else {
                                logger.error(traceId
                                        + " -No Aircraft location was found");
                            }
                        } else {
                            logger.error(traceId
                                    + " -No Observation time was found");
                        }
                    } else if (d == BUFRDescriptor.createDescriptor(0, 1, 23)) {

                        timeObs = getTimeObs(subList, 4, true);
                        if (timeObs != null) {

                            loc = getObsLocationFine(subList, 2);
                            tailNumber = getTailNumber(subList, 0);

                            if (loc != null) {
                                if (tailNumber != null) {
                                    rpt = new ACARSRecord();

                                    rpt.setTailNumber(tailNumber.trim());
                                    rpt.setLocation(loc);

                                    rpt.setTimeObs(timeObs);
                                    DataTime t = new DataTime(
                                            TimeTools.copy(timeObs));
                                    rpt.setDataTime(t);

                                    getFlightPhaseD(subList, rpt, 11);

                                    getWxDataD(subList, rpt, loc, 12);

                                    logger.debug(traceId
                                            + " -Observation time = " + timeObs);
                                } else {
                                    logger.error(traceId
                                            + " -No Aircraft tail number was found");
                                }
                            } else {
                                logger.error(traceId
                                        + " -No Aircraft location was found");
                            }
                        } else {
                            logger.error(traceId
                                    + " -No Observation time was found");
                        }
                    } else {
                        logger.error(traceId
                                + " - Unknown observation data following [0-01-008]");
                    }
                }
            }
        }
        if (rpt != null) {
            if (rpt.getFlightLevel() == null) {
                logger.error(traceId + " -No aircraft flight level was found");
                rpt = null;
            }
        }

        return rpt;
    }

    /**
     * @param packets
     * @param yearPos
     * @param getSeconds
     * @return
     */
    private Calendar getTimeObs(List<IBUFRDataPacket> packets, int yearPos,
            boolean getSeconds) {

        Calendar cal = null;

        int year = -1;
        int month = -1;
        int day = -1;
        int hour = -1;
        int minute = -1;
        int second = -1;

        IBUFRDataPacket packet = packets.get(yearPos); // Year
        int d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 4, 1)) {
            if (!packet.isMissing()) {
                year = ((Double) packet.getValue()).intValue();
            }
            if (year < 100) {
                // NWS data doesn't have the century.
                // I'm making a BIG assumption here.
                if ((year >= 0) && (year < 80)) {
                    year += 2000;
                } else {
                    year += 1900;
                }
            }
        }

        packet = packets.get(yearPos + MONTH_OFFSET); // Month
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 4, 2)) {
            if (!packet.isMissing()) {
                month = ((Double) packet.getValue()).intValue();
            }
        }
        packet = packets.get(yearPos + DAY_OFFSET); // day
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 4, 3)) {
            if (!packet.isMissing()) {
                day = ((Double) packet.getValue()).intValue();
            }
        }
        if ((year >= 0) && (month >= 0) && (day >= 0)) {
            cal = TimeTools.getBaseCalendar(year, month, day);
        }

        packet = packets.get(yearPos + HOUR_OFFSET); // Hour
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 4, 4)) {
            if (!packet.isMissing()) {
                hour = ((Double) packet.getValue()).intValue();
            }
        }

        packet = packets.get(yearPos + MINUTE_OFFSET); // Minute
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 4, 5)) {
            if (!packet.isMissing()) {
                minute = ((Double) packet.getValue()).intValue();
            }
        }

        // Seconds are only available in certain observations.
        if (getSeconds) {
            packet = packets.get(yearPos + SECOND_OFFSET); // Second
            d = packet.getReferencingDescriptor().getDescriptor();
            if (d == BUFRDescriptor.createDescriptor(0, 4, 6)) {
                if (!packet.isMissing()) {
                    second = ((Double) packet.getValue()).intValue();
                }
            }
        }

        if (cal != null) {
            if (hour >= 0) {
                cal.set(Calendar.HOUR_OF_DAY, hour);
            }
            if (minute >= 0) {
                cal.set(Calendar.MINUTE, minute);
            }
            if (second >= 0) {
                cal.set(Calendar.SECOND, second);
            }
        }

        return cal;
    }

    /**
     * 
     * @param packets
     * @param locPos
     * @return
     */
    private AircraftObsLocation getObsLocationFine(
            List<IBUFRDataPacket> packets, int locPos) {

        AircraftObsLocation loc = null;

        Double lat = null;
        Double lon = null;
        Double hgt = null;

        IBUFRDataPacket packet = packets.get(locPos); // Latitude
        int d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 5, 1)) {
            if (!packet.isMissing()) {
                lat = ((Double) packet.getValue());
            }
        }

        packet = packets.get(locPos + 1); // Longitude
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 6, 1)) {
            if (!packet.isMissing()) {
                lon = ((Double) packet.getValue());
            }
        }

        if ((lat != null) && (lon != null)) {
            loc = new AircraftObsLocation();
            loc.setLatitude(lat);
            loc.setLongitude(lon);
            loc.setLocation(lat, lon);

            // We can pick up the height here for some data. Have to look
            // elsewhere i.e. RJTD ACARS
            packet = packets.get(locPos + 3); // Height
            d = packet.getReferencingDescriptor().getDescriptor();
            if (d == BUFRDescriptor.createDescriptor(0, 7, 2)) {
                if (!packet.isMissing()) {
                    hgt = ((Double) packet.getValue());
                }
            }
            if (hgt != null) {
                loc.setFlightLevel(hgt.intValue());
            }
        }
        return loc;
    }

    /**
     * 
     * @param packets
     * @param locPos
     * @return
     */
    private AircraftObsLocation getObsLocationCoarse(
            List<IBUFRDataPacket> packets, int locPos) {

        AircraftObsLocation loc = null;

        Double lat = null;
        Double lon = null;

        IBUFRDataPacket packet = packets.get(locPos); // Latitude
        int d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 5, 2)) {
            if (!packet.isMissing()) {
                lat = ((Double) packet.getValue());
            }
        }

        packet = packets.get(locPos + 1); // Longitude
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 6, 2)) {
            if (!packet.isMissing()) {
                lon = ((Double) packet.getValue());
            }
        }

        if ((lat != null) && (lon != null)) {
            loc = new AircraftObsLocation();
            loc.setLatitude(lat);
            loc.setLongitude(lon);
            loc.setLocation(lat, lon);
        }
        return loc;
    }

    /**
     * 
     * @param dataList
     * @param record
     * @return
     */
    private ACARSRecord getWxDataA(List<IBUFRDataPacket> dataList,
            ACARSRecord record) {

        IBUFRDataPacket packet = dataList.get(11); // Temperature
        int d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 12, 1)) {
            if (!packet.isMissing()) {
                record.setTemp((Double) packet.getValue());
            }
        }

        packet = dataList.get(12); // Wind direction
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 11, 1)) {
            if (!packet.isMissing()) {
                record.setWindDirection(((Double) packet.getValue()).intValue());
            }
        }

        packet = dataList.get(13); // Wind speed
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 11, 2)) {
            if (!packet.isMissing()) {
                record.setWindSpeed((Double) packet.getValue());
            }
        }

        packet = dataList.get(14); // Turbulence
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 11, 31)) {
            if (!packet.isMissing()) {
                record.setTurbulence(((Long) packet.getValue()).intValue());
            }
        }

        packet = dataList.get(15); // Turbulence base height
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 11, 32)) {
            if (!packet.isMissing()) {
                record.setTurbBaseHgt(((Long) packet.getValue()).intValue());
            }
        }

        packet = dataList.get(16); // Turbulence top height
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 11, 33)) {
            if (!packet.isMissing()) {
                record.setTurbTopHgt(((Long) packet.getValue()).intValue());
            }
        }

        packet = dataList.get(17); // Icing
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 20, 41)) {
            if (!packet.isMissing()) {
                record.setIcing(((Long) packet.getValue()).intValue());
            }
        }

        packet = dataList.get(18); // Height of icing.
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 7, 7)) {
            if (!packet.isMissing()) {
                Integer h = ((Long) packet.getValue()).intValue();
                record.setIceBaseHgt(h);
                record.setIceTopHgt(h);
            }
        }

        if (record.getTurbulence() != null) {
            if (record.getTurbBaseHgt() == null) {
                record.setTurbBaseHgt(record.getFlightLevel());
            }
            if (record.getTurbTopHgt() == null) {
                record.setTurbTopHgt(record.getFlightLevel());
            }
        }

        Integer ice = record.getIcing();
        if ((ice != null) && (ice > NO_ICING) && (ice < RESERVE_13)) {
            if (record.getIceBaseHgt() == null) {
                record.setIceBaseHgt(record.getFlightLevel());
            }
            if (record.getIceTopHgt() == null) {
                record.setIceTopHgt(record.getFlightLevel());
            }
        }

        packet = dataList.get(24); // Aircraft roll angle quality
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 2, 64)) {
            if (!packet.isMissing()) {
                record.setIcing(((Long) packet.getValue()).intValue());
            }
        }

        return record;
    }

    /**
     * @param dataList
     * @param record
     * @param loc
     * @return ACARS Record
     */
    @SuppressWarnings("unchecked")
    private ACARSRecord getWxDataB(List<IBUFRDataPacket> dataList,
            ACARSRecord record, AircraftObsLocation loc) {

        List<IBUFRDataPacket> subList = null;
        IBUFRDataPacket packet = dataList.get(0);
        if (SubSetList.getPacketType().equals(packet.getUnits())) {
            subList = (List<IBUFRDataPacket>) packet.getValue();
            if ((subList != null) && (subList.size() >= 6)) {
                packet = subList.get(0); // Height
                int d = packet.getReferencingDescriptor().getDescriptor();
                if (d == BUFRDescriptor.createDescriptor(0, 7, 10)) {
                    if (!packet.isMissing()) {
                        Double hgt = ((Double) packet.getValue());
                        if (hgt != null) {
                            loc.setFlightLevel(hgt.intValue());
                        }
                    }
                }

                packet = subList.get(1); // Wind direction
                d = packet.getReferencingDescriptor().getDescriptor();
                if (d == BUFRDescriptor.createDescriptor(0, 11, 1)) {
                    if (!packet.isMissing()) {
                        record.setWindDirection(((Double) packet.getValue())
                                .intValue());
                    }
                }

                packet = subList.get(2); // Wind speed
                d = packet.getReferencingDescriptor().getDescriptor();
                if (d == BUFRDescriptor.createDescriptor(0, 11, 2)) {
                    if (!packet.isMissing()) {
                        record.setWindSpeed((Double) packet.getValue());
                    }
                }

                packet = subList.get(3); // Roll angle quality
                d = packet.getReferencingDescriptor().getDescriptor();
                if (d == BUFRDescriptor.createDescriptor(0, 2, 64)) {
                    if (!packet.isMissing()) {
                        record.setRollAngleQuality(((Long) packet.getValue())
                                .intValue());
                    }
                }

                packet = subList.get(4); // Temperature
                d = packet.getReferencingDescriptor().getDescriptor();
                if (d == BUFRDescriptor.createDescriptor(0, 12, 101)) {
                    if (!packet.isMissing()) {
                        record.setTemp((Double) packet.getValue());
                    }
                }

                packet = subList.get(5); // Dewpoint temperature
                d = packet.getReferencingDescriptor().getDescriptor();
                if (d == BUFRDescriptor.createDescriptor(0, 12, 103)) {
                    if (!packet.isMissing()) {
                        record.setDwpt((Double) packet.getValue());
                    }
                }
            }
        }
        return record;
    }

    /**
     * @param dataList
     * @param record
     * @param loc
     * @return
     */
    private ACARSRecord getWxDataC(List<IBUFRDataPacket> dataList,
            ACARSRecord record, AircraftObsLocation loc) {

        IBUFRDataPacket packet = dataList.get(20); // Wind direction
        int d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 11, 1)) {
            if (!packet.isMissing()) {
                record.setWindDirection(((Double) packet.getValue()).intValue());
            }
        }

        packet = dataList.get(21); // Wind speed
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 11, 2)) {
            if (!packet.isMissing()) {
                record.setWindSpeed((Double) packet.getValue());
            }
        }

        packet = dataList.get(22); // Temperature
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 12, 1)) {
            if (!packet.isMissing()) {
                record.setTemp((Double) packet.getValue());
            }
        }

        packet = dataList.get(23); // Mixing ratio
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 13, 2)) {
            if (!packet.isMissing()) {
                record.setMixingRatio((Double) packet.getValue());
            }
        }

        packet = dataList.get(24); // Humidity
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 13, 3)) {
            if (!packet.isMissing()) {
                record.setHumidity((Double) packet.getValue());
            }
        }

        packet = dataList.get(17); // Roll angle quality
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 2, 64)) {
            if (!packet.isMissing()) {
                record.setRollAngleQuality(((Long) packet.getValue())
                        .intValue());
            }
        }

        packet = dataList.get(19); // Indicated aircraft altitude
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 10, 70)) {
            if (!packet.isMissing()) {
                Double hgt = ((Double) packet.getValue());
                if (hgt != null) {
                    loc.setFlightLevel(hgt.intValue());
                }
            }
        }

        return record;
    }

    /**
     * @param dataList
     * @param record
     * @param loc
     * @param pos
     * @return
     */
    private ACARSRecord getWxDataD(List<IBUFRDataPacket> dataList,
            ACARSRecord record, AircraftObsLocation loc, int pos) {

        IBUFRDataPacket packet = dataList.get(pos); // Wind direction
        int d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 11, 1)) {
            if (!packet.isMissing()) {
                record.setWindDirection(((Double) packet.getValue()).intValue());
            }
        }

        packet = dataList.get(pos + 1); // Wind speed
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 11, 2)) {
            if (!packet.isMissing()) {
                record.setWindSpeed((Double) packet.getValue());
            }
        }

        packet = dataList.get(pos + 2); // Turbulence
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 11, 31)) {
            if (!packet.isMissing()) {
                record.setTurbulence(((Long) packet.getValue()).intValue());
            }
        }

        packet = dataList.get(pos + 3); // Maximum derived equivalent vert. gust
                                        // speed
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 11, 36)) {
            if (!packet.isMissing()) {
                // record.setTurbBaseHgt(((Long) packet.getValue()).intValue());
            }
        }

        packet = dataList.get(pos + 4); // Temperature
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 12, 101)) {
            if (!packet.isMissing()) {
                record.setTemp((Double) packet.getValue());
            }
        }

        packet = dataList.get(pos + 6); // Aircraft roll angle quality
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 2, 64)) {
            if (!packet.isMissing()) {
                record.setIcing(((Long) packet.getValue()).intValue());
            }
        }

        packet = dataList.get(pos - 2); // Flight level
        d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 7, 10)) {
            if (!packet.isMissing()) {
                Double hgt = ((Double) packet.getValue());
                if (hgt != null) {
                    loc.setFlightLevel(hgt.intValue());
                }
            }
        }

        return record;
    }

    /**
     * @param dataList
     * @param record
     * @param pos
     * @return
     */
    private ACARSRecord getFlightPhaseD(List<IBUFRDataPacket> dataList,
            ACARSRecord record, int pos) {

        IBUFRDataPacket packet = dataList.get(pos); // Detailed flight phase
        int d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 8, 9)) {
            if (!packet.isMissing()) {
                int phase = DETAIL_PHASE_MAP[((Long) packet.getValue())
                        .intValue()];
                record.setFlightPhase(phase);
            }
        }

        return record;
    }

    /**
     * 
     * 
     * @param dataList
     * @param pos
     * @return
     */
    private String getTailNumber(List<IBUFRDataPacket> dataList, int pos) {

        String tailNumber = null;

        if (pos < dataList.size()) {
            IBUFRDataPacket packet = dataList.get(pos);
            int d = packet.getReferencingDescriptor().getDescriptor();
            if (d == BUFRDescriptor.createDescriptor(0, 1, 8)) {
                if (!packet.isMissing()) {
                    tailNumber = cleanString((String) packet.getValue());
                }
            }
        }

        return tailNumber;
    }

    /**
     * @param dataList
     * @param record
     * @param pos
     * @return
     */
    private ACARSRecord getFlightPhase(List<IBUFRDataPacket> dataList,
            ACARSRecord record, int pos) {

        IBUFRDataPacket packet = dataList.get(pos); // flight phase
        int d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 8, 4)) {
            if (!packet.isMissing()) {
                record.setFlightPhase(((Long) packet.getValue()).intValue());
            }
        }

        return record;
    }

    /**
     * @param dataList
     * @param record
     * @param pos
     */
    private void getReceiver(List<IBUFRDataPacket> dataList,
            ACARSRecord record, int pos) {

        IBUFRDataPacket packet = dataList.get(pos); // Receiving station.
        int d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 2, 65)) {
            if (!packet.isMissing()) {
                String s = (String) packet.getValue();
                if (s != null) {
                    record.setReceiver(cleanString((String) packet.getValue()));
                }
            }
        }
    }

    /**
     * @param dataList
     * @param record
     * @param pos
     */
    private void getPressure(List<IBUFRDataPacket> dataList,
            ACARSRecord record, int pos) {

        IBUFRDataPacket packet = dataList.get(pos); // Pressure in pascals
        int d = packet.getReferencingDescriptor().getDescriptor();
        if (d == BUFRDescriptor.createDescriptor(0, 7, 4)) {
            if (!packet.isMissing()) {
                record.setPressure((Double) packet.getValue());
            }
        }
    }

    /**
     * @param data
     * @return
     */
    private static final String cleanString(String data) {
        String retValue = null;
        if (data != null) {
            StringBuilder sb = new StringBuilder(data);
            for (int i = 0; i < sb.length();) {
                if (sb.charAt(i) >= 32) {
                    i++;
                } else {
                    sb.deleteCharAt(i);
                }
            }
            retValue = sb.toString();
        }
        return retValue;
    }

}
