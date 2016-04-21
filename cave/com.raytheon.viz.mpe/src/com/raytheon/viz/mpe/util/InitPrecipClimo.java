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
 * Feb 3, 2015  16993     snaples     Fixed if condition on cparm.
 * Mar 2, 2015  15660      snaples     Fixed issue with if statement testing CPARM and checking for both values to be true, broken logic.
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class InitPrecipClimo {

    public InitPrecipClimo() {
        // empty constructor
    }

    public boolean initPrecip_climo(String station_climo_file,
            ArrayList<Station> precip_stations, int max_pstations) {
        BufferedReader in = null;
        String pathName = station_climo_file;
        int stationCount = 0;
        int rec_num = 1;
        DailyQcUtils dc = new DailyQcUtils();
        System.out.println("Starting init precip climo:");
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

            String line = in.readLine();
            while ((line != null) && (index < max_pstations)) {
                String tokens[] = new String[14];
                {
                    tokens = line.split("\\s+", 14);
                }
                if ((tokens == null) || (tokens.length != 14)) {
                    // invalid record
                    ++record_count;
                    line = in.readLine();
                    continue;
                }
                rec_num++;
                status = tokens[0].compareTo(precip_stations.get(index).hb5);

                if (status < 0) {
                    /* Read the next record from the climo file. */
                    line = in.readLine();
                    ++record_count;
                } else if (status > 0) {
                    /* Increment the station array index. */
                    ++index;
                } else {
                    // if (precip_stations.get(index).parm.charAt(4) ==
                    // tokens[1]
                    // .charAt(4)) {
                    Station nstation = dc.new Station();
                    nstation = precip_stations.get(index);
                    for (int m = 0; m < 12; m++) {
                        nstation.isoh[m] = Float.parseFloat(tokens[m + 2]);
                    }
                    nstation.cparm = tokens[1];
                    precip_stations.set(index, nstation);
                    nstation = null;
                }
                ++index;
                // }

            }
            in.close();
            record_count = 0;
            index = 0;

            in = new BufferedReader(new FileReader(pathName));
            stationCount = Integer.parseInt(in.readLine());
            line = in.readLine();

            while ((line != null) && (index < max_pstations)) {
                String tokens[] = new String[14];
                {
                    tokens = line.split("\\s+", 14);
                }
                if ((tokens[0] == null)) {
                    // invalid record
                    ++record_count;
                    line = in.readLine();
                    continue;
                }

                if (!(tokens[1].equalsIgnoreCase("PPMPBCM"))
                        && !(tokens[1].equalsIgnoreCase("PPMRZCM"))) {
                    line = in.readLine();
                    ++record_count;
                    continue;
                }

                if (tokens.length != 14) {
                    rec_num++;
                    if ((precip_stations.get(index).isoh[0] < 0)
                            || (precip_stations.get(index).cparm
                                    .equalsIgnoreCase("PPMPBCM"))) {
                        Station nstation = dc.new Station();
                        nstation = precip_stations.get(index);

                        for (int m = 0; m < 12; m++) {
                            nstation.isoh[m] = Float.parseFloat(tokens[m + 2]);
                        }
                        nstation.cparm = tokens[1];
                        precip_stations.set(index, nstation);
                        nstation = null;
                    }
                    ++index;
                    line = in.readLine();
                    continue;
                }

                rec_num++;
                status = tokens[0].compareTo(precip_stations.get(index).hb5);

                if (status < 0) {
                    /* Read the next record from the climo file. */
                    line = in.readLine();
                    ++record_count;
                } else if (status > 0) {
                    /* Increment the station array index. */
                    ++index;
                } else {
                    if ((precip_stations.get(index).isoh[0] < 0)
                            || (precip_stations.get(index).cparm
                                    .equalsIgnoreCase("PPMPBCM"))) {
                        Station nstation = dc.new Station();
                        nstation = precip_stations.get(index);

                        for (int m = 0; m < 12; m++) {
                            nstation.isoh[m] = Float.parseFloat(tokens[m + 2]);
                        }
                        nstation.cparm = tokens[1];
                        precip_stations.set(index, nstation);
                        nstation = null;
                    }
                    ++index;
                }
            }

            in.close();

            /* finally sum up for seasonal totals */
            for (int i = 0; i < max_pstations; i++) {

                int oldk = -1;
                int k;
                Station astation = dc.new Station();

                for (int kk = 0; kk < 12; kk++) {
                    if (kk < 3) {
                        k = 9 + kk;
                    } else {
                        k = kk - 3;
                    }

                    if (oldk == -1) {
                        astation = precip_stations.get(i);
                        astation.isoh[k + 12] = precip_stations.get(i).isoh[k];
                        precip_stations.set(i, astation);
                        astation = null;
                    } else {
                        astation = precip_stations.get(i);
                        astation.isoh[k + 12] = precip_stations.get(i).isoh[k]
                                + precip_stations.get(i).isoh[oldk];
                        precip_stations.set(i, astation);
                        astation = null;
                    }
                    oldk = k + 12;
                }
            }
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
        System.out.println("Finished init precip climo: ");
        return true;
    }
}
