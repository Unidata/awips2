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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe;

import java.awt.Rectangle;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.shef.tables.Datalimits;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypc;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypp;
import com.raytheon.uf.common.dataplugin.shef.tables.Locdatalimits;
import com.raytheon.uf.common.dataplugin.shef.tables.Pseudogageval;
import com.raytheon.uf.common.hydro.CommonHydroConstants;
import com.raytheon.uf.common.hydro.data.PrecipTotal;
import com.raytheon.uf.common.mpe.util.PrecipUtil;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.mpe.CommonMPEUtils;
import com.raytheon.uf.edex.plugin.mpe.HrapGridFactor;
import com.raytheon.uf.common.mpe.constants.MpeConstants;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.DatalimitsDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.LocdatalimitsDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.PseudogagevalDao;
import com.raytheon.uf.edex.plugin.mpe.geo.GageLocation;
import com.raytheon.uf.edex.plugin.mpe.geo.MpeGageLocationsGeoDataAsciiFile;
import com.raytheon.uf.edex.plugin.mpe.precip.GageData;
import com.raytheon.uf.edex.plugin.mpe.precip.HourlyPrecipLoaderUtil;
import com.raytheon.uf.edex.plugin.mpe.precip.PrecipDataRecord;
import org.locationtech.jts.geom.Coordinate;

/**
 * Reads gage data from the pseudo gage, HourlyPC, and HourlyPP data sources.
 * The data that is read is arranged for use by the HPE Field Gen application.
 * (TODO: ideally Gage Data retrieval and arrangement will be common across both
 * HPE and MPE Field Gen). Based on: hpe_fieldgen/TEXT/read_gage_data.c.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 6, 2016  5631       bkowal      Initial creation
 * Sep 19, 2016 5631       bkowal      Updated gage retrieval to return a {@link Map}
 *                                     of retrieved gages.
 *
 * </pre>
 *
 * @author bkowal
 */

public class GageDataManager {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private static final double TOTAL_PRECIP_VALUE_MULTIPLIER = 25.4;

    private final PseudogagevalDao pseudogagevalDao = new PseudogagevalDao();

    private final Calendar runDateTime;

    /*
     * maximum of 24 hours. Ideally verified before this point.
     */
    private final int numHoursRun;

    private final HrapGridFactor hrapGridFactor;

    private final Rectangle geoGridData;

    private final MpeGageLocationsGeoDataAsciiFile gageLocationsGeoData;

    private final List<String> ts = Arrays.asList(new String[] { "PM" });

    public GageDataManager(final HPERunConfiguration runConfig) {
        numHoursRun = runConfig.getRunHours();
        if (numHoursRun < 1 || numHoursRun > 24) {
            throw new IllegalArgumentException(
                    "Required argument 'numHoursRun' must be >= 1 and <= 24.");
        }
        this.runDateTime = runConfig.getRunDateTime();
        this.gageLocationsGeoData = runConfig.getGeoFileData()
                .getGageLocationsGeoData();
        hrapGridFactor = runConfig.getConfig().getHrapGridFactor();
        geoGridData = runConfig.getGeoGridData();
    }

    public Map<Calendar, GageData> retrieveGageData()
            throws HPEFieldGenConfigurationException {
        /*
         * The assumption is that minutes and seconds will be 0 in each of the
         * time intervals that are used. Milliseconds were already previously
         * 0'ed out when the run time was determined.
         */
        final Calendar endDateTime = TimeUtil.newCalendar(runDateTime);
        TimeUtil.minCalendarFields(endDateTime, Calendar.MINUTE,
                Calendar.SECOND, Calendar.MILLISECOND);

        /*
         * The data start date/time is based on the run date/time less the
         * number of run hours.
         */
        final Calendar startDateTime = TimeUtil.newCalendar(endDateTime);
        startDateTime.add(Calendar.HOUR_OF_DAY, -numHoursRun);

        /*
         * Initialize the {@link GageData} {@link Map} structure.
         */
        Map<Calendar, GageData> gageDataMap = initializeGageData(startDateTime,
                endDateTime);

        /*
         * Rather than making individual queries for each set of hours, all data
         * between the start data date/time and the run date/time will be
         * retrieved and separated.
         */
        retrievePseudoGages(startDateTime, endDateTime, gageDataMap);
        retrieveHourlyGages(startDateTime, endDateTime, gageDataMap);
        return gageDataMap;
    }

    private Map<Calendar, GageData> initializeGageData(
            final Calendar startDateTime, final Calendar endDateTime) {
        if (startDateTime == null) {
            throw new IllegalArgumentException(
                    "Required argument 'startDateTime' cannot be NULL.");
        }
        if (endDateTime == null) {
            throw new IllegalArgumentException(
                    "Required argument 'endDateTime' cannot be NULL.");
        }

        final Calendar sequenceDateTime = TimeUtil.newCalendar(endDateTime);

        Map<Calendar, GageData> gageDataMap = new HashMap<>(numHoursRun, 1.0f);

        while (sequenceDateTime.after(startDateTime)) {
            final Calendar dateTime = TimeUtil.newCalendar(sequenceDateTime);
            logger.info("Added Gage Data storage for date/time: {}.",
                    sequenceDateTime.getTime().toString());
            gageDataMap.put(dateTime, new GageData(dateTime));
            sequenceDateTime.add(Calendar.HOUR_OF_DAY, -1);
        }

        return gageDataMap;
    }

    /**
     * Retrieves the pseudo gage information for the specified start date/time
     * to the specified end date/time. All data that is retrieved is placed in
     * the specified gage data {@link Map} structure. Based on:
     * hpe_fieldgen/TEXT/read_pseudo_precip.c
     * 
     * @param startTime
     *            the specified start date/time
     * @param endTime
     *            the specified end date/time
     * @param gageDataMap
     *            the specified gage data {@link Map} structure
     */
    private void retrievePseudoGages(final Calendar startTime,
            final Calendar endTime, final Map<Calendar, GageData> gageDataMap) {

        /*
         * Read the pseudo gage values for the data time span. Note: none of the
         * tables on our servers contained any rows and neither did the tables
         * at a few of the operational sites. Is this the result of old, unused
         * code that was never cleaned up?
         */
        List<Pseudogageval> pseudoGages = pseudogagevalDao
                .retrieveGagesWithinTimeSpan(startTime.getTime(),
                        endTime.getTime());
        if (pseudoGages.isEmpty()) {
            return;
        }

        for (Pseudogageval pseudogageval : pseudoGages) {
            if ((pseudogageval.getGageValue() >= 0) && (pseudogageval
                    .getGageValue() != CommonHydroConstants.MISSING_PRECIP)) {
                final Coordinate hrapCoord = HPEFieldgenUtils
                        .convertLatLonToScaledHrap(pseudogageval.getLat(),
                                pseudogageval.getLon(), hrapGridFactor);
                /*
                 * Truncate to find integer HRAP coordinates which gage is in.
                 * Translate the origin to the lowerleft corner of the MPE
                 * estimation domain, that is, convert from the global to the
                 * local HRAP grid.
                 */

                int irow = (int) hrapCoord.x;
                int icol = (int) hrapCoord.y;

                irow -= geoGridData.x;
                icol -= geoGridData.y;

                for (int i = 0; i < numHoursRun; i++) {
                    PrecipDataRecord precipP3 = new PrecipDataRecord();
                    precipP3.setValue(pseudogageval.getGageValue());
                    precipP3.setX(icol);
                    precipP3.setY(irow);
                    precipP3.setId(pseudogageval.getId().getPseudoGageId());
                    precipP3.setLatitude(pseudogageval.getLat());
                    precipP3.setLongitude(pseudogageval.getLon());
                    final Calendar obsDateTime = TimeUtil
                            .newCalendar(pseudogageval.getId().getObstime());
                    gageDataMap.get(obsDateTime).addPseudoPrecipP3(precipP3);

                    if ((irow >= 0) && (irow < geoGridData.height)
                            && ((icol >= 0)
                                    && (icol < geoGridData.getWidth()))) {
                        PrecipDataRecord precip = new PrecipDataRecord();
                        precip.setValue(pseudogageval.getGageValue());
                        precip.setX(icol);
                        precip.setY(irow);
                        precip.setId(pseudogageval.getId().getPseudoGageId());
                        precip.setLatitude(pseudogageval.getLat());
                        precip.setLongitude(pseudogageval.getLon());
                        gageDataMap.get(obsDateTime).addPseudoPrecip(precip);

                        logger.info(
                                "Loaded pseudo precip for: id = {}, x = {}, y = {}, value(mm) = {} ({})",
                                pseudogageval.getId().getPseudoGageId(), irow,
                                icol, pseudogageval.getGageValue(),
                                pseudogageval.getId().getObstime().toString());
                    }
                }
            }
        }
    }

    /**
     * Retrieves the hourly PC and PP data for the specified start date/time to
     * the specified end date/time. All data that is retrieved is placed in the
     * specified gage data {@link Map} structure. Based on:
     * hpe_fieldgen/TEXT/read_gage_precip.c.
     * 
     * @param startTime
     *            the specified start date/time
     * @param endTime
     *            the specified end date/time
     * @param gageDataMap
     *            the specified gage data {@link Map} structure
     * @throws HPEFieldGenConfigurationException
     */
    private void retrieveHourlyGages(final Calendar startTime,
            final Calendar endTime, final Map<Calendar, GageData> gageDataMap)
                    throws HPEFieldGenConfigurationException {
        logger.info("Retrieving gage data spanning time period: {} to {} ...",
                startTime.getTime().toString(), endTime.getTime().toString());
        List<Hourlypc> hourlyPCList = HourlyPrecipLoaderUtil
                .loadPCForTimePeriod(startTime, endTime, null, ts, true);
        List<Hourlypp> hourlyPPList = HourlyPrecipLoaderUtil
                .loadPPForTimePeriod(startTime, endTime, null, ts, true);
        if (hourlyPCList.isEmpty() && hourlyPPList.isEmpty()) {
            /*
             * No precipitation data.
             */
            if (numHoursRun > 1) {
                logger.info(
                        "There is no precipitation data between: {} and {}.",
                        startTime.getTime().toString(),
                        endTime.getTime().toString());
            }
        }

        /*
         * "pointer" to an index in the hourlyPCList list.
         */
        final int[] pHourlyPCIdx = new int[] { 0 };
        /*
         * "pointer" to an index in the hourlyPPList list.
         */
        final int[] pHourlyPPIdx = new int[] { 0 };
        final int[] pcRecordsCount = new int[] { hourlyPCList.size() };
        final int[] ppRecordsCount = new int[] { hourlyPPList.size() };
        /*
         * hard-coded to 0.0 on line 178 of hpe_fieldgen/TEXT/read_gage_precip.c
         */
        final float minPercent = 0.0f;
        /*
         * hard-coded to 1 (true) on line 188 of
         * hpe_fieldgen/TEXT/read_gage_precip.c
         */
        final boolean advance = true;
        /*
         * hard-coded to 1 on line 189 of hpe_fieldgen/TEXT/read_gage_precip.c
         */
        final int duration = 1;

        final Calendar totalEndTime = TimeUtil.newCalendar(startTime);
        for (int i = 0; i < numHoursRun; i++) {

            /* Compute the ending data retrieval time. */
            totalEndTime.add(Calendar.HOUR_OF_DAY, 1);

            while ((pHourlyPPIdx[0] < hourlyPPList.size())
                    || (pHourlyPCIdx[0] < hourlyPCList.size())) {
                final PrecipTotal precipTotal = PrecipUtil.getInstance()
                        .getTotalHourlyPrecip(hourlyPCList, pHourlyPCIdx,
                                hourlyPPList, pHourlyPPIdx,
                                totalEndTime.getTime(), duration, minPercent,
                                CommonHydroConstants.PRECIP_TS_RANK
                                        | CommonHydroConstants.PRECIP_PP,
                                advance, pcRecordsCount, ppRecordsCount);
                if (precipTotal.value >= 0.0
                        && precipTotal.value != CommonHydroConstants.MISSING_PRECIP) {
                    if (CommonHydroConstants.PC.equals(precipTotal.getPe())) {
                        if (!rangeCheck(precipTotal.value, precipTotal.lid,
                                totalEndTime)) {
                            continue;
                        }
                    }

                    /*
                     * Retrieve the stations latitude and longitude and convert
                     * to obtain the station's HRAP coordinates.
                     */
                    GageLocation gageLocation = gageLocationsGeoData
                            .getGageLocationMap().get(precipTotal.lid);
                    if (gageLocation == null) {
                        logger.warn(
                                "Could not successfully retrieve lat/lon for lid: {}.",
                                precipTotal.lid);
                    } else {
                        /*
                         * Convert the retrieved lat/lon to HRAP coordinates.
                         */
                        final Coordinate latLonCoord = gageLocation
                                .getLatLonCoord();
                        final Coordinate hrapCoord = HPEFieldgenUtils
                                .convertLatLonToScaledHrap(latLonCoord.x,
                                        latLonCoord.y, hrapGridFactor);

                        /*
                         * Truncate to find integer HRAP coordinates which gage
                         * is in. Translate the origin to the lowerleft corner
                         * of the MPE estimation domain, that is, convert from
                         * the global to the local HRAP grid.
                         */

                        int irow = (int) hrapCoord.x;
                        int icol = (int) hrapCoord.y;

                        irow -= geoGridData.x;
                        icol -= geoGridData.y;

                        precipTotal.setValue((float) (precipTotal.getValue()
                                * TOTAL_PRECIP_VALUE_MULTIPLIER));

                        PrecipDataRecord precipP3 = new PrecipDataRecord();
                        precipP3.setValue(precipTotal.getValue());
                        precipP3.setX(icol);
                        precipP3.setY(irow);
                        precipP3.setId(precipTotal.getLid());
                        precipP3.setTs(precipTotal.getTs());
                        precipP3.setLatitude(latLonCoord.x);
                        precipP3.setLongitude(latLonCoord.y);
                        gageDataMap.get(totalEndTime).addGagePrecipP3(precipP3);

                        if ((irow >= 0) && (irow < geoGridData.height)
                                && ((icol >= 0)
                                        && (icol < geoGridData.getWidth()))) {
                            /*
                             * TODO: consider (based on data usage) adding some
                             * type of 'within hrap' flag to the Precip Data
                             * Record object and just set it when within the
                             * hrap instead of duplicating the information in
                             * another data structure that is tracked
                             * separately.
                             */
                            PrecipDataRecord precip = new PrecipDataRecord();
                            precip.setValue(precipTotal.getValue());
                            precip.setX(icol);
                            precip.setY(irow);
                            precip.setId(precipTotal.getLid());
                            precip.setTs(precipTotal.getTs());
                            precip.setLatitude(latLonCoord.x);
                            precip.setLongitude(latLonCoord.y);
                            gageDataMap.get(totalEndTime).addGagePrecip(precip);

                            logger.info(
                                    "Loaded gage precip for: lid = {}, x = {}, y = {}, value(mm) = {} ({})",
                                    gageLocation.getLid(), irow, icol,
                                    precipTotal.getValue(),
                                    totalEndTime.getTime().toString());
                        }
                    }
                }
            }
        }
    }

    /**
     * Verifies that the specified precipitation value associated with the
     * specified lid is within the expected range.
     * 
     * @param precipValue
     *            the specified precipitation value
     * @param lid
     *            the specified lid
     * @param endTime
     *            date used to lookup the data limits
     * @return {@code true} if the precipitation value is within range;
     *         {@code false}, otherwise
     * @throws HPEFieldGenConfigurationException
     */
    private boolean rangeCheck(final float precipValue, final String lid,
            final Calendar endTime) throws HPEFieldGenConfigurationException {
        double maxPrecip = MpeConstants.RANGE_CHECK_DEFAULT;
        /*
         * Load the max hourly precip data for range check.
         */
        try {
            maxPrecip = retrievePrecipLimit(lid, endTime);
        } catch (Exception e) {
            throw new HPEFieldGenConfigurationException(
                    "Failed to determine the max hourly precip value for lid: "
                            + lid + " and date/time: "
                            + endTime.getTime().toString() + ".",
                    e);
        }

        if (maxPrecip != MpeConstants.RANGE_CHECK_DEFAULT) {
            if (precipValue > maxPrecip) {
                logger.warn(
                        "Gage value: {} is greater than max precip value: {} for: {}.",
                        precipValue, maxPrecip, lid);
                return false;
            }
        }

        return true;
    }

    /**
     * Retrieves the precipitation limit for the specified lid and end date.
     * Based on: hpe_fieldgen/TEXT/read_gage_precip.c.
     * 
     * @param lid
     *            the specified lid
     * @param endTime
     *            the specified end date
     * @return the precipitation limit that was retrieved
     */
    private double retrievePrecipLimit(final String lid, final Calendar endTime)
            throws Exception {
        final LocdatalimitsDao locdatalimitsDao = new LocdatalimitsDao();
        List<Locdatalimits> locdatalimitsList = locdatalimitsDao
                .getMaxPrecipForLid(lid);
        if (!locdatalimitsList.isEmpty()) {
            /*
             * Attempt to find the precip limit for the location associated with
             * the specified lid.
             */
            for (Locdatalimits ldl : locdatalimitsList) {
                if (CommonMPEUtils.withinDataLimitTimeRange(endTime.getTime(),
                        ldl.getId().getMonthdaystart(), ldl.getMonthdayend())) {
                    if (ldl.getGrossRangeMax() != null) {
                        return ldl.getGrossRangeMax();
                    }
                }
            }
        }

        /*
         * No valid value was found in locdatalimits. Check datalimits.
         */
        final DatalimitsDao datalimitsDao = new DatalimitsDao();
        List<Datalimits> datalimitsList = datalimitsDao.getMaxPrecip();
        if (!datalimitsList.isEmpty()) {
            for (Datalimits dl : datalimitsList) {
                if (CommonMPEUtils.withinDataLimitTimeRange(endTime.getTime(),
                        dl.getId().getMonthdaystart(), dl.getMonthdayend())) {
                    if (dl.getGrossRangeMax() != null) {
                        return dl.getGrossRangeMax();
                    }
                }
            }
        }

        return MpeConstants.RANGE_CHECK_DEFAULT;
    }
}