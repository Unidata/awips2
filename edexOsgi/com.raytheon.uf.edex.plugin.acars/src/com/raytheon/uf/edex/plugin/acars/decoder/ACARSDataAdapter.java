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

import java.text.DecimalFormat;
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
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.bufrtools.BUFRDataDocument;
import com.raytheon.uf.edex.bufrtools.descriptors.BUFRDescriptor;
import com.raytheon.uf.edex.bufrtools.packets.DataPacketTypes;
import com.raytheon.uf.edex.bufrtools.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.plugin.acars.common.ACARSConstants;

/**
 * Adapter used to decode ACARS data in BUFR format.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 22, 2009  1939     jkorman     Initial creation
 * Aug 30, 2013  2298     rjpeter     Make getPluginName abstract
 * Sep 18, 2013  2339     njensen     Index safety check in getTailNumber()
 * Mar 27, 2014  2811     skorolev    Added check for empty message.
 * May 14, 2014  2536     bclement    moved WMO Header to common, removed TimeTools usage
 * Jun 12, 2014  2061     bsteffen    Generate unique stationid
 * Jul 22, 2014  3392     nabowle     ACARSRecord has Float fields instead of Double
 * Jul 23, 2014  3410     bclement    location changed to floats
 * Sep 16, 2014  3628     mapeters    Replaced static imports.
 * Aug 11, 2016  5757     nabowle     lower most error logs to warn. Don't hold onto traceId.
 * APR  6, 2017  19870    wkwock      Use descriptor value instead of fixed position to lookup data.
 *
 * </pre>
 *
 * @author jkorman
 */

public class ACARSDataAdapter {

    private IUFStatusHandler logger = UFStatus
            .getHandler(ACARSDataAdapter.class);

    // Map detailed flight phase [0-08-009] to flight phase [0-08-004]
    private static final int[] DETAIL_PHASE_MAP = { 3, 4, 2, 3, 4, 5, 6, 5, 5,
            5, 5, 6, 6, 6, 6, 7, };

    private static final DecimalFormat STATION_ID_FORMAT = new DecimalFormat(
            "###.###");

    /**
     *
     * @param name
     */
    @Deprecated
    public ACARSDataAdapter(String name) {
    }

    /**
     *
     */
    public ACARSDataAdapter() {
    }

    /**
     * @param rawData
     * @param traceId
     * @param headers
     * @return
     */
    public PluginDataObject[] getACARSData(byte[] rawData, String traceId,
            Headers headers) {

        PluginDataObject[] returnObjects = null;
        List<ACARSRecord> records = null;

        Set<String> dataSet = new HashSet<>();

        String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
        WMOHeader wmoHeader = new WMOHeader(rawData, fileName);
        if (wmoHeader.isValid()) {

            records = new ArrayList<>();

            ACARSParser parser = new ACARSParser(rawData, headers);
            while (parser.hasNext()) {

                ACARSRecord record = getDataRecord(parser.next(), traceId);
                if (record != null) {
                    record.setWmoHeader(parser.getWmoHeader().getWmoHeader());
                    try {
                        String uri = record.getDataURI();
                        if (dataSet.add(uri)) {
                            records.add(record);
                            logger.debug(traceId + "Adding " + uri);
                        } else {
                            logger.debug(traceId + "Rejecting " + uri);
                        }

                    } catch (Exception e) {
                        logger.error(traceId + "Unable to construct datauri",
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
    private ACARSRecord getDataRecord(BUFRDataDocument data, String traceId) {

        ACARSRecord rpt = null;
        Calendar timeObs = null;
        AircraftObsLocation loc = null;
        String tailNumber = null;
        if (data != null) {
            List<IBUFRDataPacket> subList = data.getList();
            if (!subList.isEmpty()) {
                IBUFRDataPacket packet = subList.get(0);
                int d = packet.getReferencingDescriptor().getDescriptor();
                if (d == BUFRDescriptor.createDescriptor(0, 1, 6)) {
                    packet = subList.get(1);
                    d = packet.getReferencingDescriptor().getDescriptor();
                    if (d == BUFRDescriptor.createDescriptor(0, 2, 61)) {

                        timeObs = getTimeObs(subList, false);
                        loc = getObsLocationFine(subList);
                        tailNumber = getTailNumber(subList);

                        if (isValid(traceId, timeObs, loc, tailNumber)) {
                            rpt = createAcarsRecord(timeObs, loc, tailNumber);
                            getFlightPhase(subList, rpt);
                            getWxDataA(subList, rpt);
                        }
                    } else if (d == BUFRDescriptor.createDescriptor(0, 1, 8)) {

                        timeObs = getTimeObs(subList, true);
                        loc = getObsLocationCoarse(subList);
                        tailNumber = getTailNumber(subList);

                        if (isValid(traceId, timeObs, loc, tailNumber)) {
                            rpt = createAcarsRecord(timeObs, loc, tailNumber);
                            getReceiver(subList, rpt);
                            getFlightPhase(subList, rpt);
                            getWxDataC(subList, rpt, loc);
                            getPressure(subList, rpt);
                        }
                    } else {
                        logger.warn(traceId
                                + "Unknown observation data following [0-01-006]");
                    }
                } else if (d == BUFRDescriptor.createDescriptor(0, 1, 8)) {

                    packet = subList.get(1);
                    d = packet.getReferencingDescriptor().getDescriptor();
                    if (d == BUFRDescriptor.createDescriptor(0, 4, 1)) {

                        timeObs = getTimeObs(subList, true);
                        loc = getObsLocationFine(subList);
                        tailNumber = getTailNumber(subList);

                        if (isValid(traceId, timeObs, loc, tailNumber)) {
                            rpt = createAcarsRecord(timeObs, loc, tailNumber);
                            getFlightPhase(subList, rpt);

                            IBUFRDataPacket wxData = subList.get(10);
                            if (DataPacketTypes.RepSubList.getPacketType()
                                    .equals(wxData.getUnits())) {
                                List<IBUFRDataPacket> dataList = (List<IBUFRDataPacket>) wxData
                                        .getValue();

                                getWxDataB(dataList, rpt, loc);
                            }
                        }
                    } else if (d == BUFRDescriptor.createDescriptor(0, 1, 23)) {

                        timeObs = getTimeObs(subList, true);
                        loc = getObsLocationFine(subList);
                        tailNumber = getTailNumber(subList);

                        if (isValid(traceId, timeObs, loc, tailNumber)) {
                            rpt = createAcarsRecord(timeObs, loc, tailNumber);
                            getFlightPhaseD(subList, rpt);
                            getWxDataD(subList, rpt, loc);
                        }

                    } else {
                        logger.warn(traceId
                                + "Unknown observation data following [0-01-008]");
                    }
                }
            }
        }
        if (rpt != null) {
            if (rpt.getFlightLevel() == null) {
                logger.warn(traceId + "No aircraft flight level was found");
                rpt = null;
            }
        }

        return rpt;
    }

    /**
     * Create a new report with the provided observation time, location, and
     * tailnumber.
     *
     * @return The created AcarsRecord.
     */
    private ACARSRecord createAcarsRecord(Calendar timeObs,
            AircraftObsLocation loc, String tailNumber) {
        ACARSRecord rpt = new ACARSRecord();
        rpt.setTailNumber(tailNumber.trim());
        rpt.setLocation(loc);
        rpt.setTimeObs(timeObs);
        DataTime t = new DataTime((Calendar) timeObs.clone());
        rpt.setDataTime(t);
        return rpt;
    }

    /**
     * Verifies that the observation time, location, and tail number are not
     * null. A warning will be logged for the first one that is null.
     *
     * @return True if the observation time, location, and tail number are all
     *         non-null. False otherwise.
     */
    private boolean isValid(String traceId, Calendar timeObs,
            AircraftObsLocation loc, String tailNumber) {
        boolean ret = true;
        if (timeObs == null) {
            logger.warn(traceId + "No Observation time was found");
            ret = false;
        } else if (loc == null) {
            logger.warn(traceId + "No Aircraft location was found");
            ret = false;
        } else if (tailNumber == null) {
            logger.warn(traceId + "No Aircraft tail number was found");
            ret = false;
        }

        if (ret) {
            logger.debug(traceId + "Observation time = " + timeObs);
        }

        return ret;
    }

    /**
     * @param packets
     * @param getSeconds
     * @return
     */
    private Calendar getTimeObs(List<IBUFRDataPacket> packets,
            boolean getSeconds) {

        Calendar cal = null;

        int year = -1;
        int month = -1;
        int day = -1;
        int hour = 0;
        int minute = 0;
        int second = 0;

        int yearDes = BUFRDescriptor.createDescriptor(0, 4, 1);
        int monthDes = BUFRDescriptor.createDescriptor(0, 4, 2);
        int dayDes = BUFRDescriptor.createDescriptor(0, 4, 3);
        int hourDes = BUFRDescriptor.createDescriptor(0, 4, 4);
        int minuteDes = BUFRDescriptor.createDescriptor(0, 4, 5);
        int secondDes = BUFRDescriptor.createDescriptor(0, 4, 6);

        int[] descriptors = { yearDes, monthDes, dayDes, hourDes, minuteDes,
                secondDes };
        IBUFRDataPacket[] dataPackets = searchDataPacketList(packets,
                descriptors);

        if (dataPackets[0] != null && !dataPackets[0].isMissing()) {
            year = ((Double) dataPackets[0].getValue()).intValue();

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

        if (dataPackets[1] != null && !dataPackets[1].isMissing()) {
            month = ((Double) dataPackets[1].getValue()).intValue();
        }

        if (dataPackets[2] != null && !dataPackets[2].isMissing()) {
            day = ((Double) dataPackets[2].getValue()).intValue();
        }

        if ((year >= 0) && (month >= 0) && (day >= 0)) {
            cal = TimeUtil.newGmtCalendar(year, month, day);
        }
        if (dataPackets[3] != null && !dataPackets[3].isMissing()) {
            hour = ((Double) dataPackets[3].getValue()).intValue();
        }
        if (dataPackets[4] != null && !dataPackets[4].isMissing()) {
            minute = ((Double) dataPackets[4].getValue()).intValue();
        }

        // Seconds are only available in certain observations.
        if (getSeconds) {
            if (dataPackets[5] != null && !dataPackets[5].isMissing()) {
                second = ((Double) dataPackets[5].getValue()).intValue();
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
     * @return
     */
    private AircraftObsLocation getObsLocationFine(
            List<IBUFRDataPacket> packets) {

        AircraftObsLocation loc = null;

        Double lat = null;
        Double lon = null;

        int latDes = BUFRDescriptor.createDescriptor(0, 5, 1);
        int lonDes = BUFRDescriptor.createDescriptor(0, 6, 1);
        int heightDes = BUFRDescriptor.createDescriptor(0, 7, 10);
        int[] descriptors = { latDes, lonDes, heightDes };
        IBUFRDataPacket[] dataPackets = searchDataPacketList(packets,
                descriptors);

        if (dataPackets[0] != null && !dataPackets[0].isMissing()) {
            lat = ((Double) dataPackets[0].getValue());
        }
        if (dataPackets[1] != null && !dataPackets[1].isMissing()) {
            lon = ((Double) dataPackets[1].getValue());
        }

        if ((lat != null) && (lon != null)) {
            loc = new AircraftObsLocation();
            loc.setLatitude(lat.floatValue());
            loc.setLongitude(lon.floatValue());
            loc.setLocation(lat, lon);

            // We can pick up the height here for some data. Have to look
            // elsewhere i.e. RJTD ACARS
            if (dataPackets[2] != null && !dataPackets[2].isMissing()) {
                loc.setFlightLevel(
                        ((Double) dataPackets[2].getValue()).intValue());
            }
            generateStationId(loc);
        }
        return loc;
    }

    /**
     *
     * @param packets
     * @return
     */
    private AircraftObsLocation getObsLocationCoarse(
            List<IBUFRDataPacket> packets) {

        AircraftObsLocation loc = null;

        int latDes = BUFRDescriptor.createDescriptor(0, 5, 2);
        int lonDes = BUFRDescriptor.createDescriptor(0, 6, 2);
        int[] descriptors = { latDes, lonDes };
        IBUFRDataPacket[] dataPackets = searchDataPacketList(packets,
                descriptors);

        if ((dataPackets[0] != null && !dataPackets[0].isMissing())
                && (dataPackets[1] != null && !dataPackets[1].isMissing())) {
            Double lat = (Double) dataPackets[0].getValue();
            Double lon = (Double) dataPackets[1].getValue();
            loc = new AircraftObsLocation();
            loc.setLatitude(lat.floatValue());
            loc.setLongitude(lon.floatValue());
            loc.setLocation(lat, lon);
            generateStationId(loc);
        }
        return loc;
    }

    private static void generateStationId(AircraftObsLocation loc) {
        synchronized (STATION_ID_FORMAT) {
            loc.setStationId(STATION_ID_FORMAT.format(loc.getLongitude()) + ":"
                    + STATION_ID_FORMAT.format(loc.getLatitude()));

        }
    }

    /**
     *
     * @param dataList
     * @param record
     * @return
     */
    private ACARSRecord getWxDataA(List<IBUFRDataPacket> dataList,
            ACARSRecord record) {
        int tempDes = BUFRDescriptor.createDescriptor(0, 12, 1);
        int windDirDes = BUFRDescriptor.createDescriptor(0, 11, 1);
        int windSpdDes = BUFRDescriptor.createDescriptor(0, 11, 2);
        int turbulenceDes = BUFRDescriptor.createDescriptor(0, 11, 31);
        int turbBaseHgtDes = BUFRDescriptor.createDescriptor(0, 11, 32);
        int turbTopHgtDes = BUFRDescriptor.createDescriptor(0, 11, 33);
        int icingDes = BUFRDescriptor.createDescriptor(0, 20, 41);
        int iceHgtDes = BUFRDescriptor.createDescriptor(0, 7, 7);
        int rollAngleQualityDes = BUFRDescriptor.createDescriptor(0, 2, 64);
        int[] descriptors = { tempDes, windDirDes, windSpdDes, turbulenceDes,
                turbBaseHgtDes, turbTopHgtDes, icingDes, iceHgtDes,
                rollAngleQualityDes };
        IBUFRDataPacket[] dataPackets = searchDataPacketList(dataList,
                descriptors);

        if (dataPackets[0] != null && !dataPackets[0].isMissing()) {
            record.setTemp(((Double) dataPackets[0].getValue()).floatValue());
        }

        if (dataPackets[1] != null && !dataPackets[1].isMissing()) {
            record.setWindDirection(
                    ((Double) dataPackets[1].getValue()).intValue());
        }

        if (dataPackets[2] != null && !dataPackets[2].isMissing()) {
            record.setWindSpeed(
                    ((Double) dataPackets[2].getValue()).floatValue());
        }

        if (dataPackets[3] != null && !dataPackets[3].isMissing()) {
            record.setTurbulence(((Long) dataPackets[3].getValue()).intValue());
        }

        if (dataPackets[4] != null && !dataPackets[4].isMissing()) {
            record.setTurbBaseHgt(
                    ((Long) dataPackets[4].getValue()).intValue());
        }

        if (dataPackets[5] != null && !dataPackets[5].isMissing()) {
            record.setTurbTopHgt(((Long) dataPackets[5].getValue()).intValue());
        }

        if (dataPackets[6] != null && !dataPackets[6].isMissing()) {
            record.setIcing(((Long) dataPackets[6].getValue()).intValue());
        }

        if (dataPackets[7] != null && !dataPackets[7].isMissing()) {
            Integer h = ((Long) dataPackets[7].getValue()).intValue();
            record.setIceBaseHgt(h);
            record.setIceTopHgt(h);
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
        if ((ice != null) && (ice > ACARSConstants.NO_ICING)
                && (ice < ACARSConstants.RESERVE_13)) {
            if (record.getIceBaseHgt() == null) {
                record.setIceBaseHgt(record.getFlightLevel());
            }
            if (record.getIceTopHgt() == null) {
                record.setIceTopHgt(record.getFlightLevel());
            }
        }

        if (dataPackets[8] != null && !dataPackets[8].isMissing()) {
            record.setRollAngleQuality(
                    ((Long) dataPackets[8].getValue()).intValue());
        }

        return record;
    }

    /**
     * @param dataList
     * @param record
     * @param loc
     * @return ACARS Record
     */
    private ACARSRecord getWxDataB(List<IBUFRDataPacket> dataList,
            ACARSRecord record, AircraftObsLocation loc) {

        List<IBUFRDataPacket> subList = null;
        IBUFRDataPacket packet = dataList.get(0);
        if (!DataPacketTypes.SubSetList.getPacketType()
                .equals(packet.getUnits())) {
            return record;
        }

        subList = (List<IBUFRDataPacket>) packet.getValue();
        if (subList == null) {
            return record;
        }

        int heightDes = BUFRDescriptor.createDescriptor(0, 7, 10);
        int windDirDes = BUFRDescriptor.createDescriptor(0, 11, 1);
        int windSpdDes = BUFRDescriptor.createDescriptor(0, 11, 2);
        int rollAngleQualityDes = BUFRDescriptor.createDescriptor(0, 2, 64);
        int tempDes = BUFRDescriptor.createDescriptor(0, 12, 101);
        int dewPointDes = BUFRDescriptor.createDescriptor(0, 12, 103);
        int[] descriptors = { heightDes, windDirDes, windSpdDes,
                rollAngleQualityDes, tempDes, dewPointDes };
        IBUFRDataPacket[] dataPackets = searchDataPacketList(subList,
                descriptors);

        if (dataPackets[0] != null && !dataPackets[0].isMissing()) {
            Double hgt = ((Double) dataPackets[0].getValue());
            if (hgt != null) {
                loc.setFlightLevel(hgt.intValue());
            }
        }

        if (dataPackets[1] != null && !dataPackets[1].isMissing()) {
            record.setWindDirection(
                    ((Double) dataPackets[1].getValue()).intValue());
        }

        if (dataPackets[2] != null && !dataPackets[2].isMissing()) {
            record.setWindSpeed(
                    ((Double) dataPackets[2].getValue()).floatValue());
        }

        if (dataPackets[3] != null && !dataPackets[3].isMissing()) {
            record.setRollAngleQuality(
                    ((Long) dataPackets[3].getValue()).intValue());
        }

        if (dataPackets[4] != null && !dataPackets[4].isMissing()) {
            record.setTemp(((Double) dataPackets[4].getValue()).floatValue());
        }

        if (dataPackets[5] != null && !dataPackets[5].isMissing()) {
            record.setDwpt(((Double) dataPackets[5].getValue()).floatValue());
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

        int windDirDes = BUFRDescriptor.createDescriptor(0, 11, 1);
        int windSpdDes = BUFRDescriptor.createDescriptor(0, 11, 2);
        int tempDes = BUFRDescriptor.createDescriptor(0, 12, 101);
        int mixingRatioDes = BUFRDescriptor.createDescriptor(0, 13, 2);
        int humidityDes = BUFRDescriptor.createDescriptor(0, 13, 3);
        int rollAngleQualityDes = BUFRDescriptor.createDescriptor(0, 2, 64);
        int flightLevelDes = BUFRDescriptor.createDescriptor(0, 10, 70);
        int[] descriptors = { windDirDes, windSpdDes, tempDes, mixingRatioDes,
                humidityDes, rollAngleQualityDes, flightLevelDes };
        IBUFRDataPacket[] dataPackets = searchDataPacketList(dataList,
                descriptors);

        if (dataPackets[0] != null && !dataPackets[0].isMissing()) {
            record.setWindDirection(
                    ((Double) dataPackets[0].getValue()).intValue());
        }

        if (dataPackets[1] != null && !dataPackets[1].isMissing()) {
            record.setWindSpeed(
                    ((Double) dataPackets[1].getValue()).floatValue());
        }

        if (dataPackets[2] != null && !dataPackets[2].isMissing()) {
            record.setTemp(((Double) dataPackets[2].getValue()).floatValue());
        }

        if (dataPackets[3] != null && !dataPackets[3].isMissing()) {
            record.setMixingRatio(
                    ((Double) dataPackets[3].getValue()).floatValue());
        }

        if (dataPackets[4] != null && !dataPackets[4].isMissing()) {
            record.setHumidity(
                    ((Double) dataPackets[4].getValue()).floatValue());
        }

        if (dataPackets[5] != null && !dataPackets[5].isMissing()) {
            record.setRollAngleQuality(
                    ((Long) dataPackets[5].getValue()).intValue());
        }

        if (dataPackets[6] != null && !dataPackets[6].isMissing()) {
            Double hgt = ((Double) dataPackets[6].getValue());
            if (hgt != null) {
                loc.setFlightLevel(hgt.intValue());
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
    private ACARSRecord getWxDataD(List<IBUFRDataPacket> dataList,
            ACARSRecord record, AircraftObsLocation loc) {

        int windDirDes = BUFRDescriptor.createDescriptor(0, 11, 1);
        int windSpdDes = BUFRDescriptor.createDescriptor(0, 11, 2);
        int tempDes = BUFRDescriptor.createDescriptor(0, 12, 101);
        int turbulenceDes = BUFRDescriptor.createDescriptor(0, 11, 31);
        int rollAngleQualityDes = BUFRDescriptor.createDescriptor(0, 2, 64);
        int flightLevelDes = BUFRDescriptor.createDescriptor(0, 10, 70);
        int[] descriptors = { windDirDes, windSpdDes, tempDes, turbulenceDes,
                rollAngleQualityDes, flightLevelDes };
        IBUFRDataPacket[] dataPackets = searchDataPacketList(dataList,
                descriptors);

        if (dataPackets[0] != null && !dataPackets[0].isMissing()) {
            record.setWindDirection(
                    ((Double) dataPackets[0].getValue()).intValue());
        }

        if (dataPackets[1] != null && !dataPackets[1].isMissing()) {
            record.setWindSpeed(
                    ((Double) dataPackets[1].getValue()).floatValue());
        }

        if (dataPackets[2] != null && !dataPackets[2].isMissing()) {
            record.setTemp(((Double) dataPackets[2].getValue()).floatValue());
        }

        if (dataPackets[3] != null && !dataPackets[3].isMissing()) {
            record.setTurbulence(((Long) dataPackets[3].getValue()).intValue());
        }

        if (dataPackets[4] != null && !dataPackets[4].isMissing()) {
            record.setIcing(((Long) dataPackets[4].getValue()).intValue());
        }

        if (dataPackets[5] != null && !dataPackets[5].isMissing()) {
            Double hgt = ((Double) dataPackets[5].getValue());
            if (hgt != null) {
                loc.setFlightLevel(hgt.intValue());
            }
        }

        return record;
    }

    /**
     * @param dataList
     * @param record
     * @return
     */
    private ACARSRecord getFlightPhaseD(List<IBUFRDataPacket> dataList,
            ACARSRecord record) {

        int flightPhaseDes = BUFRDescriptor.createDescriptor(0, 8, 9);
        int[] descriptors = { flightPhaseDes };
        IBUFRDataPacket[] dataPackets = searchDataPacketList(dataList,
                descriptors);

        if (dataPackets[0] != null && !dataPackets[0].isMissing()) {
            int phase = DETAIL_PHASE_MAP[((Long) dataPackets[0].getValue())
                    .intValue()];
            record.setFlightPhase(phase);
        }

        return record;
    }

    /**
     *
     *
     * @param dataList
     * @return
     */
    private String getTailNumber(List<IBUFRDataPacket> dataList) {

        String tailNumber = null;
        int tailNumberDes = BUFRDescriptor.createDescriptor(0, 1, 8);
        int[] descriptors = { tailNumberDes };
        IBUFRDataPacket[] dataPackets = searchDataPacketList(dataList,
                descriptors);

        if (dataPackets[0] != null && !dataPackets[0].isMissing()) {
            tailNumber = cleanString((String) dataPackets[0].getValue());
        }

        return tailNumber;
    }

    /**
     * @param dataList
     * @param record
     * @return
     */
    private ACARSRecord getFlightPhase(List<IBUFRDataPacket> dataList,
            ACARSRecord record) {

        int flightPhaseDes = BUFRDescriptor.createDescriptor(0, 8, 4);
        int[] descriptors = { flightPhaseDes };
        IBUFRDataPacket[] dataPackets = searchDataPacketList(dataList,
                descriptors);

        if (dataPackets[0] != null && !dataPackets[0].isMissing()) {
            record.setFlightPhase(
                    ((Long) dataPackets[0].getValue()).intValue());
        }

        return record;
    }

    /**
     * @param dataList
     * @param record
     */
    private void getReceiver(List<IBUFRDataPacket> dataList,
            ACARSRecord record) {
        int receiverDes = BUFRDescriptor.createDescriptor(0, 2, 65);
        int[] descriptors = { receiverDes };
        IBUFRDataPacket[] dataPackets = searchDataPacketList(dataList,
                descriptors);
        if (dataPackets[0] != null && !dataPackets[0].isMissing()) {
            String s = (String) dataPackets[0].getValue();
            if (s != null) {
                record.setReceiver(
                        cleanString((String) dataPackets[0].getValue()));
            }
        }
    }

    /**
     * @param dataList
     * @param record
     */
    private void getPressure(List<IBUFRDataPacket> dataList,
            ACARSRecord record) {
        int pressureDes = BUFRDescriptor.createDescriptor(0, 7, 4);
        int[] descriptors = { pressureDes };
        IBUFRDataPacket[] dataPackets = searchDataPacketList(dataList,
                descriptors);
        if (dataPackets[0] != null && !dataPackets[0].isMissing()) {
            record.setPressure(
                    ((Double) dataPackets[0].getValue()).floatValue());
        }
    }

    /**
     * Search for the dataPacket(s) that match the descriptors
     * 
     * @param dataList
     * @param descriptors
     * @return dataPacket list
     */
    private IBUFRDataPacket[] searchDataPacketList(
            List<IBUFRDataPacket> dataList, int descriptors[]) {
        IBUFRDataPacket[] dataPackets = new IBUFRDataPacket[descriptors.length];
        for (IBUFRDataPacket dataPacket : dataList) {
            for (int i = 0; i < descriptors.length; i++) {
                if (dataPacket.getReferencingDescriptor()
                        .getDescriptor() == descriptors[i]) {
                    dataPackets[i] = dataPacket;
                    break; // from this inner for loop
                }
            }
        }

        return dataPackets;
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
