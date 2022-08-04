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
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.collections.MapUtils;

/**
 * Representative of a Point Precipitation file that will be written by DQC
 * PreProcessor after precipitation processing has concluded.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 7, 2018  7184       bkowal      Initial creation
 * Aug 15, 2019 7912       smanoj      Station IDs can be up to 8 characters for some sites,
 *                                     so increasing the field length to 8.
 *
 * </pre>
 *
 * @author bkowal
 */

public class PrecipPointFile {

    private static final String POINT_PRECIP_NAME_FILE_PATTERN = "precip_1_%s_point_%8.8s";

    private static final String TOTAL_12Z_DATA_LINE_FMT = ".A %-8.8s %8.8s DH12/PPD1%1.1s/ %5.2fS";

    private static final String TOTAL_12Z_MISSING_LINE_FMT = ".A %-8.8s %8.8s DH12/PPD1%1.1s/ m";

    private static final String SIX_HR_LINE_FMT = ".E %-8.8s %8.8s DH18/%2.2sQ1%1.1s/DIH+6/ %s / %s / %s / %s";

    private static final String DATA_FMT = "%5.2fS";

    private static final String MISSING = "m";

    private final Path pointPrecipFilePath;

    private final DataDateKey dataDateKey;

    private final Map<OutputStationKey, PrecipStationOutput> precipStationOutputMap;

    public PrecipPointFile(final Path pointPrecipPath, final String areaName,
            final DataDateKey dataDateKey,
            final Map<OutputStationKey, PrecipStationOutput> precipStationOutputMap) {
        this.pointPrecipFilePath = pointPrecipPath
                .resolve(String.format(POINT_PRECIP_NAME_FILE_PATTERN, areaName,
                        dataDateKey.getNextDay()));
        this.dataDateKey = dataDateKey;
        this.precipStationOutputMap = precipStationOutputMap;
    }

    public void write() throws PointFileException {
        if (MapUtils.isEmpty(precipStationOutputMap)) {
            /*
             * Nothing to write.
             */
            return;
        }
        OpenOption openOption = StandardOpenOption.CREATE_NEW;
        if (Files.exists(pointPrecipFilePath)) {
            openOption = StandardOpenOption.TRUNCATE_EXISTING;
        }
        try (BufferedWriter bw = Files.newBufferedWriter(pointPrecipFilePath,
                openOption)) {
            for (Entry<OutputStationKey, PrecipStationOutput> outputEntry : precipStationOutputMap
                    .entrySet()) {
                final OutputStationKey outputStationKey = outputEntry.getKey();
                final PrecipStationOutput precipStationOutput = outputEntry
                        .getValue();
                final String lid = outputStationKey.getLid();
                final String source = outputStationKey.getSource();

                String totalLine = null;
                if (precipStationOutput.getTotalValue12to12() == null) {
                    totalLine = String.format(TOTAL_12Z_MISSING_LINE_FMT, lid,
                            dataDateKey.getNextDay(), source);
                } else {
                    totalLine = String.format(TOTAL_12Z_DATA_LINE_FMT, lid,
                            dataDateKey.getNextDay(), source,
                            precipStationOutput.getTotalValue12to12());
                }
                bw.write(totalLine + "\n");

                String sixHrLine = String.format(SIX_HR_LINE_FMT, lid,
                        dataDateKey.getCurrentDay(),
                        precipStationOutput.getPhysicalElement(), source,
                        determineWrittenValue(
                                precipStationOutput.getValue12to18()),
                        determineWrittenValue(
                                precipStationOutput.getValue18to00()),
                        determineWrittenValue(
                                precipStationOutput.getValue00to06()),
                        determineWrittenValue(
                                precipStationOutput.getValue06to12()));
                bw.write(sixHrLine + "\n");
            }
        } catch (IOException e) {
            throw new PointFileException("Precipitation", pointPrecipFilePath, e);
        }
    }

    private String determineWrittenValue(final Double dataValue) {
        if (dataValue == null) {
            return MISSING;
        }
        return String.format(DATA_FMT, dataValue);
    }
}