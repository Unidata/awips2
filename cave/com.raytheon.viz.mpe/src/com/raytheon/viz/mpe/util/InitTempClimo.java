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
package com.raytheon.viz.mpe.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

import com.raytheon.viz.mpe.util.DailyQcUtils.Station;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 23, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class InitTempClimo {

    public InitTempClimo() {
        // empty constructor
    }

    public boolean initTemp_climo(String station_climo_file,
            ArrayList<Station> temperature_stations, int max_tstations) {
        BufferedReader in = null;
        String pathName = station_climo_file;
        int stationCount = 0;
        int rec_num = 1;
        DailyQcUtils dc = new DailyQcUtils();
        System.out.println("Starting init temp climo: ");
        try {
            File infile = new File(pathName);
            if (infile.exists() != true) {
                return false;
            }
            in = new BufferedReader(new FileReader(pathName));
            stationCount = Integer.parseInt(in.readLine());
            if (stationCount == 0) {
                System.out.println("There are no Stations. ->" + pathName);
                return false;
            }
            // read in the PPD station records
            int index = 0;
            int record_count = 0;
            int status = 0;
            rec_num++;
            // Skipping over the PPD stations
            for (int i = 0; i < stationCount; ++i) {
                String line = in.readLine();
                if (line == null) {
                    System.out
                            .println("Reached EOF searching for station record at record: "
                                    + rec_num);
                    return false;
                }
                ++rec_num;
            }

            stationCount = Integer.parseInt(in.readLine());
            record_count = 0;
            String line = in.readLine();
            int fp = rec_num - 1;
            while ((line != null) && (index < max_tstations)
                    && (record_count < stationCount)) {
                String tokens[] = new String[14];
                if (line != null) {
                    tokens = line.split("\\s+", 14);
                }
                if ((tokens == null)) {
                    // invalid record
                    ++record_count;
                    line = in.readLine();
                    continue;
                }
                if (tokens.length != 14) {
                    rec_num++;
                    status = tokens[0].compareTo(temperature_stations
                            .get(index).hb5);

                    if (status < 0) {
                        /* Read the next record from the climo file. */
                        line = in.readLine();
                        ++record_count;
                    } else if (status > 0) {
                        /* Increment the station array index. */
                        ++index;
                    } else {
                        Station nstation = new Station();
                        nstation = temperature_stations.get(index);

                        if ((tokens[1].charAt(5) == 'X')
                                || (tokens[1].charAt(5) == 'N')) {
                            if (tokens[1].charAt(5) == 'X') {
                                /*
                                 * Copy the max temperature information into the
                                 * temperature structure in the temperature
                                 * station array.
                                 */

                                for (int m = 0; m < 12; m++) {
                                    nstation.isoh[m] = -99.0f;
                                }
                            } else if (tokens[1].charAt(5) == 'N') {
                                /*
                                 * Does this have an extremum code of 'Minimum'?
                                 * If so, store its information in the
                                 * temperature structure.
                                 */
                                for (int m = 0; m < 12; m++) {
                                    nstation.isoh[m] = -99.0f;
                                }
                            }

                            nstation.cparm = tokens[1];
                            temperature_stations.set(index, nstation);
                            nstation = null;
                            // }
                            line = in.readLine();
                        }
                    }
                    continue;
                }
                rec_num++;
                status = tokens[0]
                        .compareTo(temperature_stations.get(index).hb5);

                if (status < 0) {
                    /* Read the next record from the climo file. */
                    line = in.readLine();
                    ++record_count;
                } else if (status > 0) {
                    /* Increment the station array index. */
                    ++index;
                } else {
                    Station nstation = new Station();
                    nstation = temperature_stations.get(index);
                    /*
                     * Is the extremum code 'Maximum'? Of temperature,
                     * precipitation and freezing level, temperature is the only
                     * one described with an extremum code of maximum.
                     */
                    if ((tokens[1].charAt(5) == 'X')
                            || (tokens[1].charAt(5) == 'N')) {
                        if (tokens[1].charAt(5) == 'X') {
                            /*
                             * Copy the max temperature information into the
                             * temperature structure in the temperature station
                             * array.
                             */

                            for (int m = 0; m < 12; m++) {
                                nstation.isoh[m] = Float
                                        .parseFloat(tokens[m + 2]);
                            }
                        } else if (tokens[1].charAt(5) == 'N') {
                            /*
                             * Does this have an extremum code of 'Minimum'? If
                             * so, store its information in the temperature
                             * structure.
                             */
                            for (int m = 0; m < 12; m++) {
                                nstation.isoh[m] = Float
                                        .parseFloat(tokens[m + 2]);
                            }
                        }

                        nstation.cparm = tokens[1];
                        temperature_stations.set(index, nstation);
                        nstation = null;
                        // }
                        line = in.readLine();
                    } else {
                        /* Increment the station array index. */
                        ++index;
                    }
                }
            }
            in.close();
            record_count = 0;
            index = 0;

            in = new BufferedReader(new FileReader(pathName));
            for (int i = 0; i < fp; i++) {
                in.readLine();
            }
            stationCount = Integer.parseInt(in.readLine());
            line = in.readLine();

            while ((line != null) && (index < max_tstations)) {
                String tokens[] = new String[14];
                {
                    tokens = line.split("\\s+", 14);
                }
                if ((tokens == null)) {
                    // invalid record
                    ++record_count;
                    line = in.readLine();
                    continue;
                }

                if (tokens[1].substring(0, 5).equalsIgnoreCase("TAIPB")
                        && tokens[1].substring(0, 5).equalsIgnoreCase("TAIRZ")) {
                    line = in.readLine();
                    ++record_count;
                    continue;
                }
                if (tokens.length != 14) {
                    rec_num++;
                    status = tokens[0].compareTo(temperature_stations
                            .get(index).hb5);

                    if (status < 0) {
                        /* Read the next record from the climo file. */
                        line = in.readLine();
                        ++record_count;
                    } else if (status > 0) {
                        /* Increment the station array index. */
                        ++index;
                    } else {
                        Station nstation = new Station();
                        nstation = temperature_stations.get(index);

                        if ((tokens[1].charAt(5) == 'X')
                                || (tokens[1].charAt(5) == 'N')) {
                            if (tokens[1].charAt(5) == 'X') {
                                /*
                                 * Copy the max temperature information into the
                                 * temperature structure in the temperature
                                 * station array.
                                 */
                                System.out
                                        .println("Setting temp station values to missing: "
                                                + tokens[0] + " " + tokens[1]);
                                System.out.println("Station is: "
                                        + nstation.hb5);
                                for (int m = 0; m < 12; m++) {
                                    nstation.isoh[m] = -99.0f;
                                }
                            } else if (tokens[1].charAt(5) == 'N') {
                                /*
                                 * Does this have an extremum code of 'Minimum'?
                                 * If so, store its information in the
                                 * temperature structure.
                                 */
                                System.out
                                        .println("Setting temp station values to missing: "
                                                + tokens[0] + " " + tokens[1]);
                                System.out.println("Station is: "
                                        + nstation.hb5);
                                for (int m = 0; m < 12; m++) {
                                    nstation.isoh[m] = -99.0f;
                                }
                            }

                            nstation.cparm = tokens[1];
                            temperature_stations.set(index, nstation);
                            nstation = null;
                            // }
                            line = in.readLine();
                        }
                    }
                    continue;
                }

                rec_num++;
                status = tokens[0]
                        .compareTo(temperature_stations.get(index).hb5);

                if (status < 0) {
                    /* Read the next record from the climo file. */
                    line = in.readLine();
                    ++record_count;
                } else if (status > 0) {
                    /* Increment the station array index. */
                    ++index;
                } else {
                    if (((tokens[1].charAt(5) == 'X') && (temperature_stations
                            .get(index).max[0] < 0))
                            || ((tokens[1].charAt(5) == 'N') && (temperature_stations
                                    .get(index).min[0] < 0))
                            || temperature_stations.get(index).cparm.substring(
                                    0, 5).equalsIgnoreCase("TAIPB")) {
                        Station nstation = new Station();
                        nstation = temperature_stations.get(index);
                        /*
                         * Is the extremum code 'Maximum'? Of temperature,
                         * precipitation and freezing level, temperature is the
                         * only one described with an extremum code of maximum.
                         */
                        if (tokens[1].charAt(5) == 'X') {
                            /*
                             * Copy the max temperature information into the
                             * temperature structure in the temperature station
                             * array.
                             */
                            for (int m = 0; m < 12; m++) {
                                nstation.max[m] = Float
                                        .parseFloat(tokens[m + 2]);
                            }
                        } else if (tokens[1].charAt(5) == 'N') {
                            /*
                             * Does this have an extremum code of 'Minimum'? If
                             * so, store its information in the temperature
                             * structure.
                             */
                            for (int m = 0; m < 12; m++) {
                                nstation.min[m] = Float
                                        .parseFloat(tokens[m + 2]);
                            }
                        }
                        nstation.cparm = tokens[1];
                        temperature_stations.set(index, nstation);
                        nstation = null;
                        line = in.readLine();
                    } else {
                        ++index;
                    }
                }
            }
            in.close();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        System.out.println("Finished init temp climo: ");
        return true;
    }
}
