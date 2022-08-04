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
package com.raytheon.viz.radar;

import java.io.File;
import java.io.FileNotFoundException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.CorrelatedShearPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.HdaHailPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.LinkedContourVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.LinkedVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.MesocyclonePacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SCITDataPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SpecialGraphicSymbolPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.StormIDPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.TVSPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.UnlinkedContourVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.UnlinkedVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.WindBarbPacket;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest.OrderMode;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.awipstools.common.StormTrackData;

/**
 * Gets information from classes and processes these values to figure values for
 * SCAN and other
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#   Engineer     Description
 * ------------- --------- ------------ ----------------------------------------
 * Feb 16, 2009            mnash        Initial creation
 * Mar 07, 2012  14660     D. Friedman  Added time-based getSTIData* functions.
 * Mar 01, 2013  15496     zwang        Handle the expanded GSM Correct some
 *                                      status according to B14 ICD
 * Aug 20, 2014  17214     zwang        Report more status for VCP supplemental
 *                                      Info according to RPG B16 ICD
 * Mar 30, 2016  18798     jdynina      Added Backup Comms RPG Alarm
 * Jan 05, 2017  DCS19663  jdynina      Added MRLE
 * Mar 26, 2018  6711      randerso     Removed methods/fields that were
 *                                      duplicated in RadarUtil and
 *                                      RadarConstants
 *
 * </pre>
 *
 * @author mnash
 */

public class RadarHelper {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarHelper.class);

    public static final RGB LIGHT_GREEN = new RGB(0, 251, 144);

    public static final RGB MED_GREEN = new RGB(0, 187, 0);

    public static final RGB GREEN = new RGB(0, 255, 0);

    public static final RGB LIGHT_YELLOW = new RGB(255, 255, 112);

    public static final RGB YELLOW = new RGB(255, 255, 0);

    public static final RGB DARK_YELLOW = new RGB(208, 208, 96);

    public static final RGB LIGHT_RED = new RGB(255, 96, 96);

    public static final RGB BRIGHT_RED = new RGB(255, 0, 0);

    public static final RGB MED_RED = new RGB(218, 0, 0);

    public static final RGB DARK_RED = new RGB(174, 0, 0);

    public static final RGB LIGHT_BLUE = new RGB(0, 224, 255);

    public static final RGB PURPLE = new RGB(231, 0, 255);

    public static final RGB MED_PURPLE = new RGB(255, 112, 255);

    public static final RGB BLACK = new RGB(0, 0, 0);

    public static final RGB MED_GRAY = new RGB(119, 119, 144);

    public static final RGB DARK_GRAY = new RGB(118, 118, 118);

    public static final RGB LIGHT_PINK = new RGB(255, 170, 170);

    public static final RGB MED_PINK = new RGB(238, 140, 140);

    public static final RGB DARK_PINK = new RGB(201, 112, 112);

    public static final RGB BLUE = new RGB(0, 0, 255);

    public static final RGB WHITE = new RGB(255, 255, 255);

    public static final int VWP_CODE = 48;

    public static final int VAD_CODE = 84;

    public static final int GSM_CODE = 2;

    public static final RGB TVS_COLOR = BRIGHT_RED;

    public static final RGB MESO_COLOR = YELLOW;

    public static final int DMD_MIN_RADIUS = 4;

    /**
     * The default maximum difference in time used when retrieving STI data (15
     * minutes.)
     */
    public static final long DEFAULT_MAX_STI_TIME_DIFFERENCE = 15
            * TimeUtil.MILLIS_PER_MINUTE;

    /**
     * Returns the information in the packet to be processed
     *
     * @param sp
     * @return
     */
    public static List<?> getItems(SymbologyPacket sp) {
        List<?> items = null;
        if (sp instanceof UnlinkedVectorPacket) {
            UnlinkedVectorPacket uvp = (UnlinkedVectorPacket) sp;
            items = uvp.getVectors();
        } else if (sp instanceof UnlinkedContourVectorPacket) {
            UnlinkedContourVectorPacket uvp = (UnlinkedContourVectorPacket) sp;
            items = uvp.getVectors();
        } else if (sp instanceof LinkedVectorPacket) {
            LinkedVectorPacket lvp = (LinkedVectorPacket) sp;
            items = lvp.getVectors();
        } else if (sp instanceof LinkedContourVectorPacket) {
            LinkedContourVectorPacket lvp = (LinkedContourVectorPacket) sp;
            items = lvp.getVectors();
        } else if (sp instanceof HdaHailPacket) {
            HdaHailPacket hdp = (HdaHailPacket) sp;
            items = hdp.getPoints();
        } else if (sp instanceof TVSPacket) {
            TVSPacket tvp = (TVSPacket) sp;
            items = Arrays.asList(tvp.getPoints());
        } else if (sp instanceof WindBarbPacket) {
            WindBarbPacket wbp = (WindBarbPacket) sp;
            items = Arrays.asList(wbp.getPoints());
        } else if (sp instanceof SCITDataPacket) {
            SCITDataPacket sdp = (SCITDataPacket) sp;
            items = sdp.getPoints();
        } else if (sp instanceof StormIDPacket) {
            StormIDPacket id = (StormIDPacket) sp;
            items = Arrays.asList(id.getPoints());
        } else if (sp instanceof MesocyclonePacket) {
            MesocyclonePacket mp = (MesocyclonePacket) sp;
            items = Arrays.asList(mp.getPoints());
        } else if (sp instanceof CorrelatedShearPacket) {
            CorrelatedShearPacket csp = (CorrelatedShearPacket) sp;
            items = Arrays.asList(csp.getPoints());
        } else if (sp instanceof SpecialGraphicSymbolPacket) {
            SpecialGraphicSymbolPacket sgsp = (SpecialGraphicSymbolPacket) sp;
            items = sgsp.getPoints();
        }
        return items;
    }

    /**
     * Returns any other parameters needed from the packet, depending upon the
     * packet type
     *
     * @param sp
     * @return
     */
    public static Object getOther(SymbologyPacket sp) {
        return null;
    }

    /*
     * Following functions contain nothing right now, but will in the future.
     */
    public void getVil() {

    }

    public void getEchoTops() {

    }

    public void getDigitalEchoTops() {

    }

    /**
     * Get the latest STI data for use with Warngen and SRM8
     *
     * @param icao
     *            The ICAO whose STI data is to be returned
     * @return The STI data or null if there isn't any STI data in the database
     *         for the specified ICAO
     * @throws VizException
     */
    public static StormTrackData getSTIData(String icao) throws VizException {
        StormTrackData rval = null;
        try {
            RadarRecord record = null;
            DbQueryRequest request = new DbQueryRequest();
            request.setEntityClass(RadarRecord.class.getName());
            request.setLimit(1);
            request.setOrderByField("dataTime.refTime");
            request.addConstraint("productCode", new RequestConstraint("58"));
            request.addConstraint("icao", new RequestConstraint(icao));
            request.addConstraint("pluginName", new RequestConstraint("radar"));

            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);
            for (Map<String, Object> result : response.getResults()) {
                record = (RadarRecord) result.get(null);
                break;
            }

            if (record != null) {
                // Populate the radar record with its data
                File loc = HDF5Util.findHDF5Location(record);
                IDataStore dataStore = DataStoreFactory.getDataStore(loc);
                RadarDataRetriever.populateRadarRecord(dataStore, record);

                // Get the Tabular data from the record
                Map<RadarConstants.MapValues, Map<RadarConstants.MapValues, String>> stiMap = record
                        .getMapRecordVals();

                String stiDir = "";
                String stiSpeed = "";
                double direction = 0;
                double speed = 0;

                // Get the speed and dir from the record
                if (stiMap != null) {
                    stiDir = stiMap.get(RadarConstants.MapValues.STI_TYPE)
                            .get(RadarConstants.MapValues.STI_AVG_DIRECTION);
                    stiSpeed = stiMap.get(RadarConstants.MapValues.STI_TYPE)
                            .get(RadarConstants.MapValues.STI_AVG_SPEED);
                }

                if (stiDir.isEmpty() || stiSpeed.isEmpty()) {
                    direction = 0;
                    speed = 0;
                } else {
                    direction = Double.parseDouble(stiDir);
                    speed = Double.parseDouble(stiSpeed);
                }

                // Populate the return data with the STI data
                rval = new StormTrackData();
                rval.setDate(record.getDataTime().getRefTime());
                rval.setMotionDirection(direction);
                rval.setMotionSpeed(speed);
            }
        } catch (FileNotFoundException | StorageException e) {
            statusHandler.error("Error popluating RadarRecord", e);
        }

        return rval;
    }

    /**
     * Get the STI data for the radar and time specified by the given
     * RadarRecord.
     *
     * @param referenceRecord
     *            Specifies the radar and time to search for.
     * @return The STI data record for the specified radar and closest in time
     *         to the specified time within
     *         {@link #DEFAULT_MAX_STI_TIME_DIFFERENCE} or null if there isn't
     *         any STI data that matches.
     * @throws VizException
     */
    public static StormTrackData getSTIDataForRadarRecord(
            RadarRecord referenceRecord) throws VizException {
        return getSTIData(referenceRecord.getIcao(),
                referenceRecord.getDataTime().getRefTime(),
                DEFAULT_MAX_STI_TIME_DIFFERENCE);
    }

    private static final SimpleDateFormat DATE_FORMAT;
    static {
        DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        DATE_FORMAT.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * Retrieves the specified STI data.
     *
     * @param icao
     *            The ICAO of there radar whose STI data is to be returned
     * @param targetTime
     *            If not null, specifies the center or a time range to search.
     *            If null, the latest STI product is retrieved
     *            (<strong>TODO</strong>: DOES NOT WORK because "order by" is
     *            ignored by DbQueryHandler.)
     * @param maxTimeDifference
     *            The maximum acceptable difference in time from targetTime, in
     *            milliseconds.
     * @return The STI data or null if there isn't any STI data in the database
     *         that matches the given criteria
     * @throws VizException
     */
    public static StormTrackData getSTIData(String icao, Date targetTime,
            long maxTimeDifference) throws VizException {
        StormTrackData rval = null;
        try {
            RadarRecord record = null;
            DbQueryRequest request = new DbQueryRequest();
            request.setEntityClass(RadarRecord.class.getName());
            request.setOrderByField("dataTime.refTime", OrderMode.DESC);
            if (targetTime != null) {
                if (maxTimeDifference != 0) {
                    maxTimeDifference = Math.abs(maxTimeDifference);
                    request.addConstraint("dataTime.refTime",
                            new RequestConstraint(
                                    DATE_FORMAT.format(
                                            new Date(targetTime.getTime()
                                                    - maxTimeDifference)),
                                    ConstraintType.GREATER_THAN_EQUALS));
                    request.addConstraint("dataTime.refTime",
                            new RequestConstraint(
                                    DATE_FORMAT.format(
                                            new Date(targetTime.getTime()
                                                    + maxTimeDifference)),
                                    ConstraintType.LESS_THAN_EQUALS));
                    RequestConstraint timeConstraint = new RequestConstraint(
                            null, ConstraintType.BETWEEN);
                    timeConstraint.setBetweenValueList(new String[] {
                            DATE_FORMAT.format(new Date(
                                    targetTime.getTime() - maxTimeDifference)),
                            DATE_FORMAT.format(new Date(targetTime.getTime()
                                    + maxTimeDifference)) });
                    request.addConstraint("dataTime.refTime", timeConstraint);
                } else {
                    request.addConstraint("dataTime.refTime",
                            new RequestConstraint(
                                    DATE_FORMAT.format(targetTime)));
                }
            } else {
                request.setLimit(1);
            }
            request.addConstraint("productCode", new RequestConstraint("58"));
            request.addConstraint("icao", new RequestConstraint(icao));
            request.addConstraint("pluginName", new RequestConstraint("radar"));

            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);
            if (targetTime != null && maxTimeDifference != 0) {
                // Find the record closest in time to targetTime.
                long bestDifference = Long.MAX_VALUE;
                for (Map<String, Object> result : response.getResults()) {
                    RadarRecord aRecord = (RadarRecord) result.get(null);
                    long difference = Math
                            .abs(aRecord.getDataTime().getRefTime().getTime()
                                    - targetTime.getTime());
                    if (difference < bestDifference) {
                        bestDifference = difference;
                        record = aRecord;
                    }
                }
            } else {
                for (Map<String, Object> result : response.getResults()) {
                    record = (RadarRecord) result.get(null);
                    break;
                }
            }

            if (record != null) {
                // Populate the radar record with its data
                File loc = HDF5Util.findHDF5Location(record);
                IDataStore dataStore = DataStoreFactory.getDataStore(loc);
                RadarDataRetriever.populateRadarRecord(dataStore, record);

                // Get the Tabular data from the record
                Map<RadarConstants.MapValues, Map<RadarConstants.MapValues, String>> stiMap = record
                        .getMapRecordVals();

                String stiDir = null;
                String stiSpeed = null;
                double direction = 0;
                double speed = 0;

                // Get the speed and dir from the record
                if (stiMap != null) {
                    stiDir = stiMap.get(RadarConstants.MapValues.STI_TYPE)
                            .get(RadarConstants.MapValues.STI_AVG_DIRECTION);
                    stiSpeed = stiMap.get(RadarConstants.MapValues.STI_TYPE)
                            .get(RadarConstants.MapValues.STI_AVG_SPEED);
                }

                if (stiDir == null || stiSpeed == null || stiDir.isEmpty()
                        || stiSpeed.isEmpty()) {
                    direction = 0;
                    speed = 0;
                } else {
                    direction = Double.parseDouble(stiDir);
                    speed = Double.parseDouble(stiSpeed);
                }

                // Populate the return data with the STI data
                rval = new StormTrackData();
                rval.setDate(record.getDataTime().getRefTime());
                rval.setMotionDirection(direction);
                rval.setMotionSpeed(speed);
            }
        } catch (FileNotFoundException | StorageException e) {
            throw new VizException(e);
        }

        return rval;
    }

}
