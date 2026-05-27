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

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Scanner;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Reads the mpe station files and segregates the station entries into
 * temperature stations or precipitation stations. All other stations are
 * discarded.
 * 
 * Based on: ohd/pproc/src/dqc_preprocessor/TEXT/read_station_file.c
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 26, 2018 7184       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class StationFile {

    public static final String STATION_LIST_NAME_FILE_PATTERN = "%s_station_list";

    private static final String SHEF_REGEX = "(.{3})(.(.))..";

    private static final Pattern SHEF_PATTERN = Pattern.compile(SHEF_REGEX);

    private static final int PE_GROUP = 1;

    private static final int TS_GROUP = 2;

    private static final int SOURCE_GROUP = 3;

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private final Path stationFilePath;

    private Set<StationFileEntry> precipStations = new HashSet<>();

    private Set<StationFileEntry> tempStations = new HashSet<>();

    public static StationFile loadStationFile(final Path stationFilePath)
            throws StationFileException {
        if (!Files.exists(stationFilePath)) {
            throw new StationFileException(stationFilePath,
                    "The expected file does not exist.");
        }

        return new StationFile(stationFilePath);
    }

    private StationFile(final Path stationFilePath)
            throws StationFileException {
        this.stationFilePath = stationFilePath;
        readStationFile();
    }

    private void readStationFile() throws StationFileException {
        try (BufferedReader br = Files.newBufferedReader(stationFilePath)) {
            while (true) {
                String line = br.readLine();
                if (line == null) {
                    break;
                }

                try {
                    final int recordCount = Integer.parseInt(line.trim());
                    readRecords(br, recordCount);
                } catch (StationFileEarlyTerminationException e) {
                    logger.warn("Reached unexpected end of station file: "
                            + stationFilePath.toString() + ".", e);
                    break;
                } catch (NumberFormatException e) {
                    logger.warn("Line: " + line
                            + " did not contain the upcoming record count in station file: "
                            + stationFilePath.toString() + ".");
                    break;
                }
            }
        } catch (IOException e) {
            throw new StationFileException(stationFilePath,
                    "Failed to read file.", e);
        }
    }

    private void readRecords(final BufferedReader br, final int recordCount)
            throws StationFileException, StationFileEarlyTerminationException,
            IOException {
        for (int i = 0; i < recordCount; i++) {
            String line = br.readLine();
            if (line == null) {
                /*
                 * TODO: should be ERROR. But, so many of the station files has
                 * this issue. Will update once the station file generation
                 * logic has also been transitioned to Java. Alternatively, it
                 * is possible that from the perspective of EDEX the station
                 * files will no longer be needed - a capability will exist to
                 * generate them as needed for local apps.
                 */
                throw new StationFileEarlyTerminationException(recordCount, i);
            }

            StationFileEntry stationFileEntry = new StationFileEntry();
            try (final Scanner scanner = new Scanner(line)) {
                final String lid = scanner.next();
                final String shef = scanner.next();
                final float lat = scanner.nextFloat();
                final float lon = scanner.nextFloat();
                final int elevation = scanner.nextInt();
                final int tipWeighFlag = scanner.nextInt();
                String name = scanner.nextLine();
                if (name != null) {
                    name = name.trim();
                }

                stationFileEntry.setLid(lid);
                stationFileEntry.setShef(shef);
                final Matcher matcher = SHEF_PATTERN.matcher(shef);
                if (!matcher.matches()) {
                    logger.warn(
                            "Encountered improperly formed shef String in station line: "
                                    + line + " in file: "
                                    + stationFilePath.toString() + ".");
                }
                stationFileEntry.setPe(matcher.group(PE_GROUP));
                stationFileEntry.setTs(matcher.group(TS_GROUP));
                stationFileEntry.setSource(matcher.group(SOURCE_GROUP));
                stationFileEntry.setLat(lat);
                stationFileEntry.setLon(lon);
                stationFileEntry.setElevation(elevation);
                stationFileEntry.setTipWeighFlag(tipWeighFlag);
                stationFileEntry.setName(name);
            } catch (Exception e) {
                logger.warn("Encountered improperly formatted station line: "
                        + line + " in file: " + stationFilePath.toString()
                        + ".");
            }

            if ("PPD".equals(stationFileEntry.getPe())) {
                precipStations.add(stationFileEntry);
            } else if ("TAI".equals(stationFileEntry.getPe())) {
                tempStations.add(stationFileEntry);
            }
        }
    }

    /**
     * @return the stationFilePath
     */
    public Path getStationFilePath() {
        return stationFilePath;
    }

    /**
     * @return the precipStations
     */
    public Set<StationFileEntry> getPrecipStations() {
        return precipStations;
    }

    /**
     * @param precipStations
     *            the precipStations to set
     */
    public void setPrecipStations(Set<StationFileEntry> precipStations) {
        this.precipStations = precipStations;
    }

    /**
     * @return the tempStations
     */
    public Set<StationFileEntry> getTempStations() {
        return tempStations;
    }

    /**
     * @param tempStations
     *            the tempStations to set
     */
    public void setTempStations(Set<StationFileEntry> tempStations) {
        this.tempStations = tempStations;
    }
}