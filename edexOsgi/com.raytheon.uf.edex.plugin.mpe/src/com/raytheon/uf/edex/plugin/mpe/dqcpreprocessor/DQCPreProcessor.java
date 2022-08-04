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
package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.Range;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.shef.tables.Dailypp;
import com.raytheon.uf.common.dataplugin.shef.tables.Datalimits;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypp;
import com.raytheon.uf.common.dataplugin.shef.tables.IHourlyTS;
import com.raytheon.uf.common.dataplugin.shef.tables.Locdatalimits;
import com.raytheon.uf.common.dataplugin.shef.tables.Temperature;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode;
import com.raytheon.uf.common.mpe.constants.MpeConstants;
import com.raytheon.uf.common.mpe.dqcpreprocessor.DQCPreProcRunConfiguration;
import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output.DataDateKey;
import com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output.PointFileException;
import com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output.PrecipPointFile;
import com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output.OutputStationKey;
import com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output.PrecipStationOutput;
import com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output.SynopticHour;
import com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output.TemperatureBasetime;
import com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output.TemperatureOutputValue;
import com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output.TemperaturePointFile;
import com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output.TemperatureStationOutput;
import com.raytheon.uf.edex.plugin.mpe.util.DataLimitUtil;

/**
 * Java port of the MPE DQC PreProcessor. Based on:
 * ohd/pproc/src/dqc_preprocessor/TEXT. Reads data from the DailyPP, HourlyPP,
 * and HourlyPC tables. Creates a summary of the data and writes it to Point
 * Precipitation files for areas specified in Apps Defaults.
 * 
 * TODO: there are significant improvements that could be made to this DQC
 * PreProcessor implementation if it were fully integrated into the shef
 * processing pipeline instead of being triggered by a cron timer. However, the
 * improvements would be better made in a more recent baseline.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 24, 2018 7184       bkowal      Initial creation
 * Feb 19, 2018 7184       bkowal      Always overwrite any previously calculated 12z-12z total for
 *                                     precipitation. Ensure precipitation output is only based on
 *                                     one source of data.
 * Mar 05, 2018 7232       bkowal      No longer calculate totals for partial precip synoptic days.
 * Mar 12, 2018 7184       bkowal      Correctly fill in the precipitation output based on hourlypp 6-hour fields.
 *                                     Use hourly PC data before hourly PP data.
 * Mar 26, 2018 7184       bkowal      Do not use negative 6-hour totals. Adjust the hourly slotted data that is used
 *                                     when calculating the total for a 6-hour period.
 * Apr 06, 2018 7184       bkowal      Updated to fully utilize externally provided run configuration when specified.
 *
 * Nov 21, 2019 21684      jkelmer     Removed check for completePrecipDay when setting daily values
 * </pre>
 *
 * @author bkowal
 */

public class DQCPreProcessor {

    private static final String STATION_LIST_NAME_FILE_PATTERN = "%s_station_list";

    private static final String OUTPUT_DATE_FORMAT = "yyyyMMdd";

    private static final ThreadLocal<SimpleDateFormat> outputSDF = TimeUtil
            .buildThreadLocalSimpleDateFormat(OUTPUT_DATE_FORMAT,
                    TimeUtil.GMT_TIME_ZONE);

    private static final List<Short> LIMIT6_HOUR_DURATIONS = Arrays
            .asList(new Short[] { 1006 });

    private static final List<Short> LIMIT24_HOUR_DURATIONS = Arrays
            .asList(new Short[] { 2001, 5004 });

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private DQCPreProcConfig config;

    private DQCPreProcRunData runData;

    public void execute(final DQCPreProcRunConfiguration runConfiguration)
            throws DqcPreProcFailedException {
        final ITimer timer = TimeUtil.getTimer();
        timer.start();
        logger.info("Starting DQC PreProcessor ...");

        configure(runConfiguration);
        runData = new DQCPreProcRunData(config);
        for (String areaName : config.getAreaNames()) {
            processArea(areaName);
        }

        timer.stop();
        logger.info("DQC PreProcessor has finished in {}.",
                TimeUtil.prettyDuration(timer.getElapsedTime()));
    }

    private void configure(final DQCPreProcRunConfiguration runConfiguration)
            throws DqcPreProcFailedException {
        final AppsDefaults appsDefaults = AppsDefaults.getInstance();

        /*
         * TODO: in a later version of the AWIPS II baseline, there are
         * annotations that can be applied to the configuration POJO that would
         * handle reading of Apps Defaults properties automatically. And, it
         * would no longer be necessary to read each property out individually
         * as is currently being done here.
         */
        /*
         * The following property identifies the location of the Station List
         * files.
         */
        final String mpeStationListDir = appsDefaults
                .getToken(MpeConstants.AppsDefaults.MPE_STATION_LIST_DIR, null);
        if (mpeStationListDir == null || mpeStationListDir.isEmpty()) {
            throw new DqcPreProcFailedException(
                    "Unable to configure DQC PreProcessor. The "
                            + MpeConstants.AppsDefaults.MPE_STATION_LIST_DIR
                            + " token is not defined in Apps Defaults.");
        }

        /*
         * The following properties determine the locations that the
         * Precipitation and Temperature data should be written out to
         * respectively.
         */
        final String mpePointPrecipDir = appsDefaults
                .getToken(MpeConstants.AppsDefaults.MPE_POINT_PRECIP_DIR, null);
        if (mpePointPrecipDir == null || mpePointPrecipDir.isEmpty()) {
            throw new DqcPreProcFailedException(
                    "Unable to configure DQC PreProcessor. The "
                            + MpeConstants.AppsDefaults.MPE_POINT_PRECIP_DIR
                            + " token is not defined in Apps Defaults.");
        }
        final String mpePointTemperatureDir = appsDefaults.getToken(
                MpeConstants.AppsDefaults.MPE_POINT_TEMPERATURE_DIR, null);
        if (mpePointTemperatureDir == null
                || mpePointTemperatureDir.isEmpty()) {
            throw new DqcPreProcFailedException(
                    "Unable to configure DQC PreProcessor. The "
                            + MpeConstants.AppsDefaults.MPE_POINT_TEMPERATURE_DIR
                            + " token is not defined in Apps Defaults.");
        }

        /*
         * This property is specific to processing of precipitation data.
         */
        final Boolean loadHourlyPC = AppsDefaultsConversionWrapper
                .getPropertyAsBoolean(
                        MpeConstants.AppsDefaults.MPE_LOAD_HOURLYPC);

        /*
         * The following properties are specific to processing of temperature
         * data.
         */
        final int temperatureWindow = appsDefaults.getInt(
                MpeConstants.AppsDefaults.MPE_TEMPERATURE_WINDOW,
                DQCPreProcessorConstants.DEFAULT_TEMPERATURE_WINDOW);
        final String temperatureBasetimeString = appsDefaults.getToken(
                MpeConstants.AppsDefaults.DQC_PREPROCESSOR_BASETIME, null);
        TemperatureBasetime temperatureBasetime = TemperatureBasetime
                .lookupByKey(temperatureBasetimeString);
        if (temperatureBasetime == null) {
            temperatureBasetime = TemperatureBasetime.BASETIME_12Z;
            logger.info(
                    "The " + MpeConstants.AppsDefaults.DQC_PREPROCESSOR_BASETIME
                            + " token is not defined in Apps Defaults. Using the default temperature basetime: "
                            + temperatureBasetime.name() + ".");
        }

        /*
         * The following properties determine which areas data will need to be
         * processed/generated for.
         */
        String mpeSiteId = appsDefaults
                .getToken(MpeConstants.AppsDefaults.MPE_SITE_ID, null);
        if (mpeSiteId == null || mpeSiteId.isEmpty()) {
            throw new DqcPreProcFailedException(
                    "Unable to configure DQC PreProcessor. The "
                            + MpeConstants.AppsDefaults.MPE_SITE_ID
                            + " token is not defined in Apps Defaults.");
        }
        mpeSiteId = mpeSiteId.trim();

        Set<String> areaNames = null;
        if (CollectionUtils.isEmpty(runConfiguration.getAreas())) {
            final String mpeAreaNames = appsDefaults
                    .getToken(MpeConstants.AppsDefaults.MPE_AREA_NAMES, null);
            if (mpeAreaNames == null || mpeAreaNames.isEmpty()) {
                throw new DqcPreProcFailedException(
                        "Unable to configure DQC PreProcessor. The "
                                + MpeConstants.AppsDefaults.MPE_AREA_NAMES
                                + " token is not defined in Apps Defaults.");
            }

            /*
             * Tokenize the mpe area names.
             */
            areaNames = new HashSet<>();
            for (String areaName : mpeAreaNames.split(",")) {
                areaNames.add(areaName.trim());
            }
            areaNames.add(mpeSiteId);
        } else {
            /*
             * Use the areas defined in the run configuration.
             */
            areaNames = new HashSet<>(runConfiguration.getAreas().size(), 1.0f);
            for (String areaName : runConfiguration.getAreas()) {
                areaNames.add(areaName.trim());
            }
        }

        final Calendar startDate = TimeUtil
                .newGmtCalendar(runConfiguration.getRunDate().getTime());
        startDate.add(Calendar.DATE, -runConfiguration.getNumDays());
        final Calendar endDate = TimeUtil.newGmtCalendar(startDate.getTime());
        /*
         * An extra day only needs to be added when it is after 12z to
         * generate/update the file for the following 12z-12z period.
         */
        int numDays = runConfiguration.getNumDays();
        if (runConfiguration.getRunDate().get(Calendar.HOUR_OF_DAY) >= 12) {
            ++numDays;
        }
        endDate.add(Calendar.DATE, numDays);
        this.config = new DQCPreProcConfig(runConfiguration,
                Paths.get(mpeStationListDir), Paths.get(mpePointPrecipDir),
                Paths.get(mpePointTemperatureDir), mpeSiteId, areaNames,
                loadHourlyPC, temperatureWindow, temperatureBasetime, startDate,
                endDate);
    }

    private void processArea(final String areaName)
            throws DqcPreProcFailedException {
        /*
         * Read the station file for the area.
         */
        final Path stationFilePath = config.getStationListPath().resolve(
                String.format(STATION_LIST_NAME_FILE_PATTERN, areaName));
        StationFile stationFile = null;
        try {
            logger.info("Reading station file: {} ...",
                    stationFilePath.toString());
            stationFile = StationFile.loadStationFile(stationFilePath);
            logger.info("Successfully read station file: {}.",
                    stationFilePath.toString());
        } catch (StationFileException e) {
            logger.error("Failed to read the station file for area: " + areaName
                    + ".", e);
            return;
        }

        processPrecip(areaName, stationFile.getPrecipStations());
        processTemperature(areaName, stationFile.getTempStations());
    }

    private void processPrecip(final String areaName,
            final Set<StationFileEntry> precipStations) {
        if (CollectionUtil.isNullOrEmpty(precipStations)) {
            return;
        }

        Calendar iterationDate = TimeUtil.newCalendar(config.getStartDate());
        final Map<DataDateKey, Map<OutputStationKey, PrecipStationOutput>> dataMap = new HashMap<>(
                config.getRunConfig().getNumDays() + 1, 1.0f);
        final boolean setZero = config.getRunConfig().isSetZero();
        while (iterationDate.before(config.getEndDate())) {
            final String currentDay = outputSDF.get()
                    .format(iterationDate.getTime());
            iterationDate.add(Calendar.DATE, 1);
            final String nextDay = outputSDF.get()
                    .format(iterationDate.getTime());

            final Map<OutputStationKey, PrecipStationOutput> stationOutputMap = new TreeMap<>();
            for (StationFileEntry stationFileEntry : precipStations) {
                OutputStationKey outputStationKey = new OutputStationKey(
                        stationFileEntry.getLid(),
                        stationFileEntry.getSource());
                stationOutputMap.put(outputStationKey,
                        new PrecipStationOutput(setZero));
            }
            dataMap.put(new DataDateKey(currentDay, nextDay), stationOutputMap);
        }

        if (!setZero) {
            processDailyPPData(dataMap);
            processHourlyPCAndPPData(dataMap);
        }
        writePrecip(areaName, dataMap);
    }

    private void processTemperature(final String areaName,
            final Set<StationFileEntry> temperatureStations) {
        if (CollectionUtil.isNullOrEmpty(temperatureStations)) {
            return;
        }

        Calendar iterationDate = TimeUtil.newCalendar(config.getStartDate());
        final Map<DataDateKey, Map<OutputStationKey, TemperatureStationOutput>> dataMap = new HashMap<>(
                config.getRunConfig().getNumDays() + 1, 1.0f);
        final Map<String, List<Map<Range<Calendar>, SynopticIdentifier>>> dateSlotRangeLookupMap = new HashMap<>(
                config.getRunConfig().getNumDays() + 1, 1.0f);
        final boolean setZero = config.getRunConfig().isSetZero();
        while (iterationDate.before(config.getEndDate())) {
            /*
             * Determine how the data will be arranged into slots based on the
             * configuration.
             */
            Calendar slotIterationDate = TimeUtil.newCalendar(iterationDate);

            /*
             * The outer edges of the slots.
             */
            final String currentDay = outputSDF.get()
                    .format(iterationDate.getTime());
            iterationDate.add(Calendar.DATE, 1);
            final String nextDay = outputSDF.get()
                    .format(iterationDate.getTime());
            final DataDateKey dataDateKey = new DataDateKey(currentDay,
                    nextDay);

            slotIterationDate = TimeUtil.minCalendarFields(slotIterationDate,
                    Calendar.MINUTE, Calendar.SECOND, Calendar.MILLISECOND);
            /*
             * Set the end of the first slot date/time based on the temperature
             * basetime configuration.
             */
            slotIterationDate.set(Calendar.HOUR_OF_DAY,
                    config.getTemperatureBasetime().getBaseTime());

            /*
             * Determine the ranges of all four slots.
             */
            final Collection<SynopticHour> synopticHourOrder = SynopticHour
                    .determineSynopticHourOrder(
                            config.getTemperatureBasetime());
            final Map<Range<Calendar>, SynopticIdentifier> rangeSynopticLookupMap = new HashMap<>(
                    synopticHourOrder.size(), 1.0f);
            for (SynopticHour synopticHour : synopticHourOrder) {
                final Range<Calendar> slotRange = getSlotRange(
                        slotIterationDate);
                rangeSynopticLookupMap.put(slotRange,
                        new SynopticIdentifier(synopticHour,
                                TimeUtil.newCalendar(slotIterationDate),
                                dataDateKey));
                slotIterationDate.add(Calendar.HOUR_OF_DAY,
                        DQCPreProcessorConstants.TEMPERATURE_RANGE_DURATION);
            }

            final Map<OutputStationKey, TemperatureStationOutput> stationOutputMap = new TreeMap<>();
            for (StationFileEntry stationFileEntry : temperatureStations) {
                OutputStationKey outputStationKey = new OutputStationKey(
                        stationFileEntry.getLid(),
                        stationFileEntry.getSource());
                stationOutputMap.put(outputStationKey,
                        new TemperatureStationOutput(synopticHourOrder,
                                setZero));
            }
            dataMap.put(dataDateKey, stationOutputMap);
            List<Map<Range<Calendar>, SynopticIdentifier>> possibleRangesForCurrentDay = dateSlotRangeLookupMap
                    .get(currentDay);
            if (possibleRangesForCurrentDay == null) {
                possibleRangesForCurrentDay = new ArrayList<>();
                dateSlotRangeLookupMap.put(currentDay,
                        possibleRangesForCurrentDay);
            }
            possibleRangesForCurrentDay.add(rangeSynopticLookupMap);
            List<Map<Range<Calendar>, SynopticIdentifier>> possibleRangesForNextDay = dateSlotRangeLookupMap
                    .get(nextDay);
            if (possibleRangesForNextDay == null) {
                possibleRangesForNextDay = new ArrayList<>();
                dateSlotRangeLookupMap.put(nextDay, possibleRangesForNextDay);
            }
            possibleRangesForNextDay.add(rangeSynopticLookupMap);
        }

        if (!setZero) {
            processTemperatureData(dataMap, dateSlotRangeLookupMap);
        }
        writeTemperature(areaName, dataMap);
    }

    private Range<Calendar> getSlotRange(final Calendar slotIterationDate) {
        final Calendar beginSlot = TimeUtil.newCalendar(slotIterationDate);
        beginSlot.add(Calendar.HOUR_OF_DAY, -3);
        final Calendar endSlot = TimeUtil.newCalendar(slotIterationDate);
        endSlot.add(Calendar.HOUR_OF_DAY, 3);
        return Range.between(beginSlot, endSlot);
    }

    private void processDailyPPData(
            final Map<DataDateKey, Map<OutputStationKey, PrecipStationOutput>> dataMap) {
        List<Dailypp> dailyPPResults = runData.getDailyPPData();
        if (dailyPPResults.isEmpty()) {
            return;
        }

        for (Dailypp dailypp : dailyPPResults) {
            final Calendar outputObsDate = TimeUtil
                    .newGmtCalendar(dailypp.getId().getObstime());
            /*
             * Subtract one day for lookup purposes, the day in the dailypp
             * record is the end day of the 12z-12z synoptic period. Records are
             * keyed by the start day of the 12z-12z synoptic period.
             */
            outputObsDate.add(Calendar.DATE, -1);
            final String obsDate = outputSDF.get()
                    .format(outputObsDate.getTime());
            final DataDateKey dataDateKey = new DataDateKey(obsDate);
            if (!dataMap.containsKey(dataDateKey)) {
                /*
                 * This should never happen. The start date/end date are used to
                 * constrain the data that is returned.
                 */
                logger.warn("Encountered data for unexpected obs date: "
                        + obsDate + ".");
                continue;
            }

            if (dailypp.getId().getTs().length() < 2) {
                logger.error("Skipping DailyPP record: " + dailypp.toString()
                        + " with invalid ts.");
                continue;
            }
            final String source = String
                    .valueOf(dailypp.getId().getTs().charAt(1));
            final OutputStationKey outputStationKey = new OutputStationKey(
                    dailypp.getId().getLid(), source);
            final PrecipStationOutput precipStationOutput = dataMap
                    .get(dataDateKey).get(outputStationKey);
            if (precipStationOutput == null) {
                continue;
            }
                precipStationOutput.setTotalValue12to12(dailypp.getValue());
        }
    }

    private void processHourlyPCAndPPData(
            final Map<DataDateKey, Map<OutputStationKey, PrecipStationOutput>> dataMap) {
        /*
         * Use the hourlypp data. Any 6 hour values will be used to fill in the
         * output data structure.
         */
        final Map<LidTSKey, Set<IHourlyTS>> lidTSHourlyPPMap = runData
                .getLidTSHourlyPPMap();
        fill6HourSlotsWithHourlyPP(dataMap, lidTSHourlyPPMap);

        /*
         * Use the hourlypc data to attempt to fill any empty slots provided
         * that this has been enabled via Apps Defaults.
         */
        calculateTotalPrecip(dataMap, lidTSHourlyPPMap,
                runData.getLidTSHourlyPCMap());
    }

    private void processTemperatureData(
            final Map<DataDateKey, Map<OutputStationKey, TemperatureStationOutput>> dataMap,
            final Map<String, List<Map<Range<Calendar>, SynopticIdentifier>>> dateSlotRangeLookupMap) {
        if (runData.getTemperatureData().isEmpty()) {
            return;
        }

        for (Temperature temperature : runData.getTemperatureData()) {
            final String obsDate = outputSDF.get()
                    .format(temperature.getId().getObstime());

            /*
             * Determine if there are data slots associated with the obs time.
             */
            List<Map<Range<Calendar>, SynopticIdentifier>> possibleRangesForDay = dateSlotRangeLookupMap
                    .get(obsDate);
            if (possibleRangesForDay == null) {
                /*
                 * This record does not fall within the possible range of
                 * synoptic times. Skip it.
                 */
                continue;
            }

            /*
             * Attempt to determine the synoptic hour and date.
             */
            final Calendar obsTimeCal = TimeUtil
                    .newCalendar(temperature.getId().getObstime());
            SynopticIdentifier synopticIdentifier = null;
            long diffFromSynoptic = 0;
            for (Map<Range<Calendar>, SynopticIdentifier> rangeSynopticLookupMap : possibleRangesForDay) {
                for (Entry<Range<Calendar>, SynopticIdentifier> entry : rangeSynopticLookupMap
                        .entrySet()) {
                    final Range<Calendar> checkRange = entry.getKey();
                    if (obsTimeCal.after(checkRange.getMinimum()) && (obsTimeCal
                            .before(checkRange.getMaximum())
                            || obsTimeCal.equals(checkRange.getMaximum()))) {
                        synopticIdentifier = entry.getValue();
                        diffFromSynoptic = (Math.abs(synopticIdentifier
                                .getSynopticCalendar().getTimeInMillis()
                                - obsTimeCal.getTimeInMillis()))
                                / TimeUtil.MILLIS_PER_MINUTE;
                        break;
                    }
                }
            }
            if (synopticIdentifier == null) {
                continue;
            }

            /*
             * Verify that there is output that would be written for the lid and
             * source associated with this temperature record.
             */
            if (temperature.getId().getTs().length() < 2) {
                logger.error("Skipping Temperature record: "
                        + temperature.toString() + " with invalid ts.");
                continue;
            }
            final String source = String
                    .valueOf(temperature.getId().getTs().charAt(1));
            final OutputStationKey outputStationKey = new OutputStationKey(
                    temperature.getId().getLid(), source);
            final TemperatureStationOutput temperatureStationOutput = dataMap
                    .get(synopticIdentifier.getDataDateKey())
                    .get(outputStationKey);
            if (temperatureStationOutput == null) {
                /*
                 * No output associated with this lid and ts. Skip this record.
                 */
                continue;
            }

            final Range<Double> limit = determineTemperatureLimit(
                    temperature.getId().getLid(),
                    temperature.getId().getObstime());
            final Double value = temperature.getValue();
            if (limit == null || (limit.getMinimum() <= value
                    && limit.getMaximum() >= value)) {
                final String extremum = temperature.getId().getExtremum();
                if ("X".equals(extremum)) {
                    /*
                     * process the max temperature value.
                     */
                    if (temperatureStationOutput.getMax() == null || temperature
                            .getValue() > temperatureStationOutput.getMax()) {
                        temperatureStationOutput.setMax(value);
                    }
                } else if ("N".equals(extremum)) {
                    /*
                     * process the min temperature value.
                     */
                    if (temperatureStationOutput.getMin() == null || temperature
                            .getValue() < temperatureStationOutput.getMin()) {
                        temperatureStationOutput.setMin(value);
                    }
                } else {
                    /* Process and store this temperature observation. */

                    /*
                     * determine if the min/max can be updated.
                     */
                    if (temperatureStationOutput.getMax() == null || temperature
                            .getValue() > temperatureStationOutput.getMax()) {
                        temperatureStationOutput.setMax(value);
                    }
                    if (temperatureStationOutput.getMin() == null || temperature
                            .getValue() < temperatureStationOutput.getMin()) {
                        temperatureStationOutput.setMin(value);
                    }

                    if (diffFromSynoptic <= config.getTemperatureWindow()) {
                        /*
                         * store the 00z, 06z, 12z, 18z temperature value
                         */
                        final TemperatureOutputValue temperatureOutputValue = temperatureStationOutput
                                .getSynopticHourDataMap()
                                .get(synopticIdentifier.getSynopticHour());
                        if (temperatureOutputValue.getValue() == null
                                || temperatureOutputValue
                                        .getDiffFromSynoptic() > diffFromSynoptic) {
                            temperatureOutputValue.setValue(value);
                            temperatureOutputValue
                                    .setDiffFromSynoptic(diffFromSynoptic);
                        }
                    }
                }
            }
        }
    }

    private void writePrecip(final String areaName,
            final Map<DataDateKey, Map<OutputStationKey, PrecipStationOutput>> dataMap) {
        for (Entry<DataDateKey, Map<OutputStationKey, PrecipStationOutput>> dataEntry : dataMap
                .entrySet()) {
            final PrecipPointFile precipPointFile = new PrecipPointFile(
                    config.getPointPrecipPath(), areaName, dataEntry.getKey(),
                    dataEntry.getValue());
            try {
                precipPointFile.write();
            } catch (PointFileException e) {
                logger.error(
                        "Failed to write the Point Precipitation File for area: "
                                + areaName + ".",
                        e);
            }
        }
    }

    private void writeTemperature(final String areaName,
            final Map<DataDateKey, Map<OutputStationKey, TemperatureStationOutput>> dataMap) {
        for (Entry<DataDateKey, Map<OutputStationKey, TemperatureStationOutput>> dataEntry : dataMap
                .entrySet()) {
            final TemperaturePointFile temperaturePointFile = new TemperaturePointFile(
                    config.getPointTemperaturePath(), areaName,
                    dataEntry.getKey(), dataEntry.getValue(),
                    config.getTemperatureBasetime());
            try {
                temperaturePointFile.write();
            } catch (PointFileException e) {
                logger.error(
                        "Failed to write the Point Temperature File for area: "
                                + areaName + ".",
                        e);
            }
        }
    }

    private void fill6HourSlotsWithHourlyPP(
            final Map<DataDateKey, Map<OutputStationKey, PrecipStationOutput>> dataMap,
            final Map<LidTSKey, Set<IHourlyTS>> lidTSHourlyPPMap) {
        if (lidTSHourlyPPMap.isEmpty()) {
            return;
        }

        for (Entry<LidTSKey, Set<IHourlyTS>> entry : lidTSHourlyPPMap
                .entrySet()) {
            final LidTSKey lidTSKey = entry.getKey();
            final Set<IHourlyTS> hourlyData = entry.getValue();

            /*
             * The data period is 12z-12z. So, records must be processed in
             * pairs to generate/calculate the needed values.
             */
            Iterator<IHourlyTS> hourlyIterator = hourlyData.iterator();
            Hourlypp first = (Hourlypp) hourlyIterator.next();
            while (true) {
                Hourlypp second = null;
                if (hourlyIterator.hasNext()) {
                    second = (Hourlypp) hourlyIterator.next();
                }
                final String obsDate = outputSDF.get()
                        .format(first.getObsdate());
                final DataDateKey dataDateKey = new DataDateKey(obsDate);
                if (!dataMap.containsKey(dataDateKey)) {
                    /*
                     * This may happen if an extra day of data was retrieved to
                     * have access to the hour 24 value from a previous day.
                     */
                    if (second == null) {
                        break;
                    }
                    first = second;
                    continue;
                }

                /*
                 * Get the output container that the information will be placed
                 * in.
                 */
                if (lidTSKey.getTs().length() < 2) {
                    logger.error("Skipping HourlyPP record: " + first.toString()
                            + " with invalid ts.");
                    first = second;
                    continue;
                }
                final String source = String
                        .valueOf(lidTSKey.getTs().charAt(1));
                final OutputStationKey outputStationKey = new OutputStationKey(
                        lidTSKey.getLid(), source);
                final PrecipStationOutput precipStationOutput = dataMap
                        .get(dataDateKey).get(outputStationKey);
                if (precipStationOutput == null) {
                    if (second == null) {
                        break;
                    }
                    first = second;
                    continue;
                }

                final Short sixHr18 = first.getSixhr18();
                final Short sixHr24 = first.getSixhr24();
                Short sixHr06 = null;
                Short sixHr12 = null;
                if (second != null) {
                    sixHr06 = second.getSixhr06();
                    sixHr12 = second.getSixhr12();
                }

                if (sixHr18 != null && sixHr18 >= 0) {
                    precipStationOutput
                            .setValue12to18((double) sixHr18 / 100.0);
                }
                if (sixHr24 != null && sixHr24 >= 0) {
                    precipStationOutput
                            .setValue18to00((double) sixHr24 / 100.0);
                }
                if (sixHr06 != null && sixHr06 >= 0) {
                    precipStationOutput
                            .setValue00to06((double) sixHr06 / 100.0);
                }
                if (sixHr12 != null && sixHr12 >= 0) {
                    precipStationOutput
                            .setValue06to12((double) sixHr12 / 100.0);
                }

                /*
                 * Only calculate the 12z-12z total for a complete day.
                 */
                    final Double totalValue12to12 = sumTotal(sixHr18, sixHr24,
                            sixHr06, sixHr12);
                    if (totalValue12to12 != null) {
                        precipStationOutput
                                .setTotalValue12to12(totalValue12to12 / 100.0);
                    }
                
                precipStationOutput.setSource(source);

                if (second == null) {
                    break;
                }
                first = second;
            }
        }
    }

    private void calculateTotalPrecip(
            final Map<DataDateKey, Map<OutputStationKey, PrecipStationOutput>> dataMap,
            final Map<LidTSKey, Set<IHourlyTS>> lidTSHourlyPPMap,
            final Map<LidTSKey, Set<IHourlyTS>> lidTSHourlyPCMap) {
        final Set<LidTSKey> uniqueLidTSKeys = new HashSet<>(
                lidTSHourlyPPMap.size() + lidTSHourlyPCMap.size(), 1.0f);
        uniqueLidTSKeys.addAll(lidTSHourlyPPMap.keySet());
        uniqueLidTSKeys.addAll(lidTSHourlyPCMap.keySet());

        // unsigned char precip_settings = PRECIP_TS_SINGLE | PRECIP_PP;
        for (LidTSKey lidTSKey : uniqueLidTSKeys) {
            /*
             * Determine what data is available.
             */
            Set<IHourlyTS> ppData = lidTSHourlyPPMap.get(lidTSKey);
            Set<IHourlyTS> pcData = lidTSHourlyPCMap.get(lidTSKey);

            if (CollectionUtils.isNotEmpty(pcData)) {
                /*
                 * Use any available PC data to fill in empty slots.
                 */
                calculateTotalPrecipPC(dataMap, lidTSKey, pcData);
            }
            if (CollectionUtils.isNotEmpty(ppData)) {
                /*
                 * Use any available PP data to fill in empty slots. The C++ DQC
                 * PreProcessor never previously used PP data causing a large
                 * number of values reported as missing.
                 */
                calculateTotalPrecipPP(dataMap, lidTSKey, ppData);
            }
        }
    }

    private void calculateTotalPrecipPP(
            final Map<DataDateKey, Map<OutputStationKey, PrecipStationOutput>> dataMap,
            final LidTSKey lidTSKey, Set<IHourlyTS> hourlyData) {
        /*
         * The data period is 12z-12z. So, records must be processed in pairs to
         * generate/calculate the needed values.
         */
        Iterator<IHourlyTS> hourlyIterator = hourlyData.iterator();
        IHourlyTS first = hourlyIterator.next();
        while (true) {
            IHourlyTS second = null;
            if (hourlyIterator.hasNext()) {
                second = hourlyIterator.next();
            }
            final String obsDate = outputSDF.get().format(first.getObsdate());
            final DataDateKey dataDateKey = new DataDateKey(obsDate);
            if (!dataMap.containsKey(dataDateKey)) {
                /*
                 * This may happen if an extra day of data was retrieved to have
                 * access to the hour 24 value from a previous day.
                 */
                if (second == null) {
                    break;
                }
                first = second;
                continue;
            }

            /*
             * Get the output container that the information will be placed in.
             */
            final String source = String.valueOf(lidTSKey.getTs().charAt(1));
            final OutputStationKey outputStationKey = new OutputStationKey(
                    lidTSKey.getLid(), source);
            final PrecipStationOutput precipStationOutput = dataMap
                    .get(dataDateKey).get(outputStationKey);
            if (precipStationOutput == null
                    || precipStationOutput.contains6HourData()) {
                if (second == null) {
                    break;
                }
                first = second;
                continue;
            }

            Double value12to18 = sumTotal(first.getHour13(), first.getHour14(),
                    first.getHour15(), first.getHour16(), first.getHour17(),
                    first.getHour18());
            Double value18to00 = sumTotal(first.getHour19(), first.getHour20(),
                    first.getHour21(), first.getHour22(), first.getHour23(),
                    first.getHour24());
            Double value00to06 = null;
            Double value06to12 = null;
            if (second != null) {
                value00to06 = sumTotal(second.getHour1(), second.getHour2(),
                        second.getHour3(), second.getHour4(), second.getHour5(),
                        second.getHour6());
                value06to12 = sumTotal(second.getHour7(), second.getHour8(),
                        second.getHour9(), second.getHour10(),
                        second.getHour11(), second.getHour12());
            }
            Double totalValue12to12 = null;
            if (value12to18 != null) {
                precipStationOutput.setValue12to18(value12to18 / 100.0);
                totalValue12to12 = value12to18;
            }
            if (value18to00 != null) {
                precipStationOutput.setValue18to00(value18to00 / 100.0);
                if (totalValue12to12 == null) {
                    totalValue12to12 = value18to00;
                } else {
                    totalValue12to12 += value18to00;
                }
            }
            if (value00to06 != null) {
                precipStationOutput.setValue00to06(value00to06 / 100.0);
                if (totalValue12to12 == null) {
                    totalValue12to12 = value00to06;
                } else {
                    totalValue12to12 += value00to06;
                }
            }
            if (value06to12 != null) {
                precipStationOutput.setValue06to12(value06to12 / 100.0);
                if (totalValue12to12 == null) {
                    totalValue12to12 = value06to12;
                } else {
                    totalValue12to12 += value06to12;
                }
            }

            if (totalValue12to12 != null) {
                precipStationOutput
                        .setTotalValue12to12(totalValue12to12 / 100.0);
            }
            precipStationOutput.setSource(source);

            if (second == null) {
                break;
            }
            first = second;
        }
    }

    private void calculateTotalPrecipPC(
            final Map<DataDateKey, Map<OutputStationKey, PrecipStationOutput>> dataMap,
            final LidTSKey lidTSKey, Set<IHourlyTS> hourlyData) {
        /*
         * The data period is 12z-12z. So, records must be processed in pairs to
         * generate/calculate the needed values.
         */
        Iterator<IHourlyTS> hourlyIterator = hourlyData.iterator();
        IHourlyTS first = hourlyIterator.next();
        while (true) {
            IHourlyTS second = null;
            if (hourlyIterator.hasNext()) {
                second = hourlyIterator.next();
            }
            final String obsDate = outputSDF.get().format(first.getObsdate());
            final DataDateKey dataDateKey = new DataDateKey(obsDate);
            if (!dataMap.containsKey(dataDateKey)) {
                /*
                 * This may happen if an extra day of data was retrieved to have
                 * access to the hour 24 value from a previous day.
                 */
                if (second == null) {
                    break;
                }
                first = second;
                continue;
            }

            /*
             * Get the output container that the information will be placed in.
             */
            final String source = String.valueOf(lidTSKey.getTs().charAt(1));
            final OutputStationKey outputStationKey = new OutputStationKey(
                    lidTSKey.getLid(), source);
            final PrecipStationOutput precipStationOutput = dataMap
                    .get(dataDateKey).get(outputStationKey);
            if (precipStationOutput == null
                    || precipStationOutput.contains6HourData()) {
                if (second == null) {
                    break;
                }
                first = second;
                continue;
            }

            if (first.getHourlyQc().length() < 24) {
                logger.error("Skipping HourlyPC record: " + first.toString()
                        + " with invalid qc.");
                first = second;
                continue;
            }
            Deque<HourlyPCValue> values12To18Queue = new LinkedList<>();
            /*
             * 12Z is the starting point; followed by hours 13Z-18Z of
             * accumulation.
             */
            values12To18Queue.add(new HourlyPCValue(first.getHour12(),
                    first.getHourlyQc().charAt(11)));
            values12To18Queue.add(new HourlyPCValue(first.getHour13(),
                    first.getHourlyQc().charAt(12)));
            values12To18Queue.add(new HourlyPCValue(first.getHour14(),
                    first.getHourlyQc().charAt(13)));
            values12To18Queue.add(new HourlyPCValue(first.getHour15(),
                    first.getHourlyQc().charAt(14)));
            values12To18Queue.add(new HourlyPCValue(first.getHour16(),
                    first.getHourlyQc().charAt(15)));
            values12To18Queue.add(new HourlyPCValue(first.getHour17(),
                    first.getHourlyQc().charAt(16)));
            values12To18Queue.add(new HourlyPCValue(first.getHour18(),
                    first.getHourlyQc().charAt(17)));
            Double value12to18 = diffTotal(lidTSKey, first.getObsdate(),
                    values12To18Queue, LIMIT6_HOUR_DURATIONS, "12z-18z value");

            Deque<HourlyPCValue> values18To00Queue = new LinkedList<>();
            values18To00Queue.add(new HourlyPCValue(first.getHour18(),
                    first.getHourlyQc().charAt(17)));
            values18To00Queue.add(new HourlyPCValue(first.getHour19(),
                    first.getHourlyQc().charAt(18)));
            values18To00Queue.add(new HourlyPCValue(first.getHour20(),
                    first.getHourlyQc().charAt(19)));
            values18To00Queue.add(new HourlyPCValue(first.getHour21(),
                    first.getHourlyQc().charAt(20)));
            values18To00Queue.add(new HourlyPCValue(first.getHour22(),
                    first.getHourlyQc().charAt(21)));
            values18To00Queue.add(new HourlyPCValue(first.getHour23(),
                    first.getHourlyQc().charAt(22)));
            values18To00Queue.add(new HourlyPCValue(first.getHour24(),
                    first.getHourlyQc().charAt(23)));
            Double value18to00 = diffTotal(lidTSKey, first.getObsdate(),
                    values18To00Queue, LIMIT6_HOUR_DURATIONS, "18z-00z value");

            Deque<HourlyPCValue> values00To06Queue = new LinkedList<>();
            Deque<HourlyPCValue> values06To12Queue = new LinkedList<>();
            Double value00to06 = null;
            Double value06to12 = null;
            if (second != null) {
                if (second.getHourlyQc().length() < 24) {
                    logger.error("Skipping HourlyPC record: "
                            + second.toString() + " with invalid qc.");
                    first = second;
                    continue;
                }
                values00To06Queue.add(new HourlyPCValue(first.getHour24(),
                        first.getHourlyQc().charAt(23)));
                values00To06Queue.add(new HourlyPCValue(second.getHour1(),
                        second.getHourlyQc().charAt(0)));
                values00To06Queue.add(new HourlyPCValue(second.getHour2(),
                        second.getHourlyQc().charAt(1)));
                values00To06Queue.add(new HourlyPCValue(second.getHour3(),
                        second.getHourlyQc().charAt(2)));
                values00To06Queue.add(new HourlyPCValue(second.getHour4(),
                        second.getHourlyQc().charAt(3)));
                values00To06Queue.add(new HourlyPCValue(second.getHour5(),
                        second.getHourlyQc().charAt(4)));
                values00To06Queue.add(new HourlyPCValue(second.getHour6(),
                        second.getHourlyQc().charAt(5)));
                value00to06 = diffTotal(lidTSKey, second.getObsdate(),
                        values00To06Queue, LIMIT6_HOUR_DURATIONS,
                        "00z-06z value");

                values06To12Queue.add(new HourlyPCValue(second.getHour6(),
                        second.getHourlyQc().charAt(5)));
                values06To12Queue.add(new HourlyPCValue(second.getHour7(),
                        second.getHourlyQc().charAt(6)));
                values06To12Queue.add(new HourlyPCValue(second.getHour8(),
                        second.getHourlyQc().charAt(7)));
                values06To12Queue.add(new HourlyPCValue(second.getHour9(),
                        second.getHourlyQc().charAt(8)));
                values06To12Queue.add(new HourlyPCValue(second.getHour10(),
                        second.getHourlyQc().charAt(9)));
                values06To12Queue.add(new HourlyPCValue(second.getHour11(),
                        second.getHourlyQc().charAt(10)));
                values06To12Queue.add(new HourlyPCValue(second.getHour12(),
                        first.getHourlyQc().charAt(11)));
                value06to12 = diffTotal(lidTSKey, second.getObsdate(),
                        values06To12Queue, LIMIT6_HOUR_DURATIONS,
                        "06z-12z value");
            }

            /*
             * Just add the previously calculated values together to determine
             * the total for the current synoptic day.
             */
            Double totalValue12to12 = null;

            if (value12to18 != null
                    && precipStationOutput.getValue12to18() == null) {
                precipStationOutput.setValue12to18(value12to18);
                totalValue12to12 = value12to18;
            }
            if (value18to00 != null
                    && precipStationOutput.getValue18to00() == null) {
                precipStationOutput.setValue18to00(value18to00);
                if (totalValue12to12 == null) {
                    totalValue12to12 = value18to00;
                } else {
                    totalValue12to12 += value18to00;
                }
            }
            if (value00to06 != null
                    && precipStationOutput.getValue00to06() == null) {
                precipStationOutput.setValue00to06(value00to06);
                if (totalValue12to12 == null) {
                    totalValue12to12 = value00to06;
                } else {
                    totalValue12to12 += value00to06;
                }
            }
            if (value06to12 != null
                    && precipStationOutput.getValue06to12() == null) {
                precipStationOutput.setValue06to12(value06to12);
                if (totalValue12to12 == null) {
                    totalValue12to12 = value06to12;
                } else {
                    totalValue12to12 += value06to12;
                }
            }
            if (totalValue12to12 != null) {
                /*
                 * Verify that the calculated value is within limits.
                 */
                Double limit = determinePrecipLimit(lidTSKey.getLid(),
                        first.getObsdate(), LIMIT24_HOUR_DURATIONS);
                if (limit != null && totalValue12to12.doubleValue() > limit
                        .doubleValue()) {
                    logger.info(
                            "Calculated 12z-12z total: {} for {} exceeds the allowed limit: {}. Discarding the calculated value.",
                            totalValue12to12, lidTSKey, limit);
                    totalValue12to12 = null;
                }
                precipStationOutput.setTotalValue12to12(totalValue12to12);
            }
            precipStationOutput.setSource(source);
            precipStationOutput.setPhysicalElement(
                    ParameterCode.PhysicalElement.PRECIPITATION_ACCUMULATOR
                            .getCode());

            if (second == null) {
                break;
            }
            first = second;
        }
    }

    private Double sumTotal(final Short... values) {
        Double total = null;
        for (Short value : values) {
            if (!isPresent(value)) {
                continue;
            }

            if (total == null) {
                total = (double) value;
            } else {
                total += (double) value;
            }
        }
        return total;
    }

    private Double diffTotal(final LidTSKey lidTSKey, final Date obsDate,
            final Deque<HourlyPCValue> values,
            final Collection<Short> requiredDurations, final String label) {
        if (CollectionUtils.isEmpty(values)) {
            return null;
        }
        /*
         * Find a usuable value at the beginning and end of the list of values.
         */
        HourlyPCValue begin = null;
        while (true) {
            begin = values.peekFirst();
            if (begin == null) {
                /*
                 * No additional values are present in the list.
                 */
                return null;
            }
            if (isPresent(begin.getValue()) && begin.getQc() != 'B'
                    && begin.getQc() != 'R') {
                break;
            }
            /*
             * Value is not usable. Remove it.
             */
            values.removeFirst();
        }
        HourlyPCValue end = null;
        while (true) {
            end = values.peekLast();
            if (end == null) {
                /*
                 * No additional values are present in the list.
                 */
                return null;
            }
            if (isPresent(end.getValue()) && end.getQc() != 'B'
                    && end.getQc() != 'R') {
                break;
            }
            /*
             * Value is not usable. Remove it.
             */
            values.removeLast();
        }
        if (begin == null || end == null) {
            return null;
        }
        Double calculatedValue = ((double) end.getValue()
                - (double) begin.getValue()) / 100.0;
        if (calculatedValue < 0) {
            /*
             * Do not allow negative values.
             */
            calculatedValue = 0.0;
        }
        /*
         * Verify that the calculated value is within the expected data limits
         * (only for PC data).
         */
        Double limit = determinePrecipLimit(lidTSKey.getLid(), obsDate,
                requiredDurations);
        if (limit != null
                && calculatedValue.doubleValue() > limit.doubleValue()) {
            logger.info(
                    "Calculated {}: {} for {} exceeds the allowed limit: {}. Discarding the calculated value.",
                    label, calculatedValue, lidTSKey, limit);
            return null;
        }

        return calculatedValue;
    }

    private Double determinePrecipLimit(final String lid, final Date obsDate,
            final Collection<Short> requiredDurations) {
        /*
         * Determine if there is a specific data limit that applies for the lid.
         */
        List<Locdatalimits> locdatalimits = runData
                .getPrecipLidLocdatalimitMap().get(lid);
        if (CollectionUtils.isNotEmpty(locdatalimits)) {
            for (Locdatalimits ldl : locdatalimits) {
                if (!requiredDurations.contains(ldl.getId().getDur())) {
                    continue;
                }
                try {
                    if (DataLimitUtil.withinDataLimitTimeRange(obsDate,
                            ldl.getId().getMonthdaystart(),
                            ldl.getMonthdayend())) {
                        return ldl.getGrossRangeMax();
                    }
                } catch (Exception e) {
                    /*
                     * Ignore for now. Indicates an invalid data limit exists in
                     * the database.
                     */
                    logger.warn("Encountered invalid data limit: "
                            + ldl.toString() + ".");
                }
            }
        }

        /*
         * A data limit specifically associated with the lid does not exist.
         * Determine if one of the general data limits applies.
         */
        if (CollectionUtils.isNotEmpty(runData.getPrecipDatalimits())) {
            for (Datalimits dl : runData.getPrecipDatalimits()) {
                try {
                    if (DataLimitUtil.withinDataLimitTimeRange(obsDate,
                            dl.getId().getMonthdaystart(),
                            dl.getMonthdayend())) {
                        return dl.getGrossRangeMax();
                    }
                } catch (Exception e) {
                    /*
                     * Ignore for now. Indicates an invalid data limit exists in
                     * the database.
                     */
                    logger.warn("Encountered invalid data limit: "
                            + dl.toString() + ".");
                }
            }
        }

        /*
         * No data limits are applicable.
         */
        return null;
    }

    private Range<Double> determineTemperatureLimit(final String lid,
            final Date obsDate) {
        /*
         * Determine if there is a specific data limit that applies for the lid.
         */
        List<Locdatalimits> locdatalimits = runData.getTempLidLocdatalimitMap()
                .get(lid);
        if (CollectionUtils.isNotEmpty(locdatalimits)) {
            for (Locdatalimits ldl : locdatalimits) {
                if (ldl.getGrossRangeMin() == null) {
                    continue;
                }
                try {
                    if (DataLimitUtil.withinDataLimitTimeRange(obsDate,
                            ldl.getId().getMonthdaystart(),
                            ldl.getMonthdayend())) {
                        return Range.between(ldl.getGrossRangeMin(),
                                ldl.getGrossRangeMax());
                    }
                } catch (Exception e) {
                    /*
                     * Ignore for now. Indicates an invalid data limit exists in
                     * the database.
                     */
                    logger.warn("Encountered invalid data limit: "
                            + ldl.toString() + ".");
                }
            }
        }

        /*
         * A data limit specifically associated with the lid does not exist.
         * Determine if one of the general data limits applies.
         */
        if (CollectionUtils.isNotEmpty(runData.getPrecipDatalimits())) {
            for (Datalimits dl : runData.getPrecipDatalimits()) {
                if (dl.getGrossRangeMin() == null) {
                    continue;
                }
                try {
                    if (DataLimitUtil.withinDataLimitTimeRange(obsDate,
                            dl.getId().getMonthdaystart(),
                            dl.getMonthdayend())) {
                        return Range.between(dl.getGrossRangeMin(),
                                dl.getGrossRangeMax());
                    }
                } catch (Exception e) {
                    /*
                     * Ignore for now. Indicates an invalid data limit exists in
                     * the database.
                     */
                    logger.warn("Encountered invalid data limit: "
                            + dl.toString() + ".");
                }
            }
        }

        /*
         * No data limits are applicable.
         */
        return null;
    }

    private boolean isPresent(Short value) {
        return (value != null) && (value >= 0);
    }

}