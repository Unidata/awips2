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
package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

/**
 * Representative of a Point Temperature file that will be written by DQC
 * PreProcessor after temperature processing has concluded.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 15, 2018 7184       bkowal      Initial creation
 * Aug 15, 2019 7912       smanoj      Station IDs can be up to 8 characters for some sites,
 *                                     so increasing the field length to 8.
 *
 * </pre>
 *
 * @author bkowal
 */

public class TemperaturePointFile {

    private static final String POINT_TEMP_NAME_FILE_PATTERN = "temperature_1_%s_point_%8.8s";

    private static final String MAX_DATA_LINE_FMT = ".A %-8.8s %8.8s %4.4s/TAI1%1.1sXZ %s";

    private static final String MIN_DATA_LINE_FMT = ".A %-8.8s %8.8s %4.4s/TAI1%1.1sNZ %s";

    private static final String SIX_HR_LINE_FMT = ".E %-8.8s %8.8s %4.4s/TAI1%1.1sZZ/DIH+6/ %s / %s / %s / %s";

    private static final String DATA_FMT = "%5dS";

    private static final String MISSING = "m";

    private final Path pointTemperatureFilePath;

    private final DataDateKey dataDateKey;

    private final Map<OutputStationKey, TemperatureStationOutput> temperatureStationOutputMap;

    private final TemperatureBasetime temperatureBasetime;

    public TemperaturePointFile(final Path pointTemperaturePath,
            final String areaName, final DataDateKey dataDateKey,
            final Map<OutputStationKey, TemperatureStationOutput> temperatureStationOutputMap,
            final TemperatureBasetime temperatureBasetime) {
        this.pointTemperatureFilePath = pointTemperaturePath
                .resolve(String.format(POINT_TEMP_NAME_FILE_PATTERN, areaName,
                        dataDateKey.getNextDay()));
        this.dataDateKey = dataDateKey;
        this.temperatureStationOutputMap = temperatureStationOutputMap;
        this.temperatureBasetime = temperatureBasetime;
    }

    public void write() throws PointFileException {
        if (temperatureStationOutputMap.isEmpty()) {
            /*
             * Nothing to write.
             */
            return;
        }
        OpenOption openOption = StandardOpenOption.CREATE_NEW;
        if (Files.exists(pointTemperatureFilePath)) {
            openOption = StandardOpenOption.TRUNCATE_EXISTING;
        }
        try (BufferedWriter bw = Files
                .newBufferedWriter(pointTemperatureFilePath, openOption)) {
            for (Entry<OutputStationKey, TemperatureStationOutput> outputEntry : temperatureStationOutputMap
                    .entrySet()) {
                final OutputStationKey outputStationKey = outputEntry.getKey();
                final TemperatureStationOutput temperatureStationOutput = outputEntry
                        .getValue();
                final String lid = outputStationKey.getLid();
                final String source = outputStationKey.getSource();

                bw.write(
                        String.format(MAX_DATA_LINE_FMT, lid,
                                dataDateKey.getNextDay(),
                                temperatureBasetime.getOutput(), source,
                                determineWrittenValue(
                                        temperatureStationOutput.getMax()))
                        + "\n");
                bw.write(
                        String.format(MIN_DATA_LINE_FMT, lid,
                                dataDateKey.getNextDay(),
                                temperatureBasetime.getOutput(), source,
                                determineWrittenValue(
                                        temperatureStationOutput.getMin()))
                        + "\n");
                Iterator<TemperatureOutputValue> iterator = temperatureStationOutput
                        .getSynopticHourDataMap().values().iterator();
                bw.write(
                        String.format(SIX_HR_LINE_FMT, lid,
                                dataDateKey.getCurrentDay(),
                                temperatureBasetime.getOutput(), source,
                                determineWrittenValue(
                                        iterator.next().getValue()),
                        determineWrittenValue(iterator.next().getValue()),
                        determineWrittenValue(iterator.next().getValue()),
                        determineWrittenValue(iterator.next().getValue()))
                        + "\n");
            }
        } catch (IOException e) {
            throw new PointFileException("Temperature",
                    pointTemperatureFilePath, e);
        }
    }

    private String determineWrittenValue(final Double dataValue) {
        if (dataValue == null) {
            return MISSING;
        }
        return String.format(DATA_FMT, (int) (dataValue + 0.5));
    }
}