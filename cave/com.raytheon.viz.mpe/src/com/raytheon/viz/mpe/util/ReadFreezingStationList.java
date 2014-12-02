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
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.hydro.spatial.HRAP;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 18, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class ReadFreezingStationList {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReadFreezingStationList.class);

    public static int max_zstations = 0;

    private AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private String station_dir = appsDefaults.getToken("mpe_station_list_dir");

    private String sitename = appsDefaults.getToken("mpe_site_id");

    private String pathName = "";

    private DailyQcUtils dqc = DailyQcUtils.getInstance();
    
    private StationListManager stationListManager = StationListManager.getInstance();

    public ArrayList<Station> read_freezing_station_list(String qcArea,
            boolean master_file_flag) {


        try {
            stationListManager.getStationInfo(qcArea, master_file_flag,
                    dqc.freezing_stations,
                    dqc.temperature_stations,
                    dqc.precip_stations);

        } catch (Exception e) {
            e.printStackTrace();
        }

        ReadFreezingStationList.max_zstations = dqc.freezing_stations
                .size();

        return dqc.freezing_stations;
    }

    public ArrayList<Station> old_read_freezing_station_list(String qcArea,
            boolean master_file_flag) {

        pathName = getStationListPath(qcArea);
        int stationCount = 0;
        int rec_num = 1;
        ArrayList<Station> stations = new ArrayList<Station>();

        if (pathName == null) {
            return null;
        }
        BufferedReader in = null;

        if (master_file_flag) {
            try {
                in = new BufferedReader(new FileReader(pathName));
                stationCount = Integer.parseInt(in.readLine().trim());

                // Skipping over the (1 hour) PPH stations
                for (int i = 0; i < stationCount; ++i) {
                    in.readLine().trim();
                    ++rec_num;
                }
                String linein = in.readLine().trim();
                stationCount = Integer.parseInt(linein.trim()); // Integer.parseInt(in.readLine());
                max_zstations = stationCount;
                // Skipping over the PPD stations
                for (int i = 0; i < stationCount; ++i) {
                    in.readLine().trim();
                    ++rec_num;
                }
                linein = in.readLine().trim();
                stationCount = Integer.parseInt(linein.trim()); // Integer.parseInt(in.readLine());
                max_zstations = stationCount;
                // Skipping over the TAI stations
                for (int i = 0; i < stationCount; ++i) {
                    in.readLine().trim();
                    ++rec_num;
                }
                linein = in.readLine().trim();
                stationCount = Integer.parseInt(linein.trim()); // Integer.parseInt(in.readLine());
                max_zstations = stationCount;
                if (stationCount == 0) {
                    System.out.println("There are no Stations. ->" + linein);
                    return null;
                }
                // read in the HZI station records
                for (int i = 0; i < stationCount; ++i) {
                    String[] tokens;
                    String line = in.readLine().trim();
                    if (line != null) {
                        tokens = line.split("\\s+", 7);
                    } else {
                        tokens = null;
                    }
                    if ((tokens != null) && tokens.length == 7) {
                        Station astation = dqc.new Station();
                        astation.hb5 = tokens[0].toString().trim();
                        astation.parm = tokens[1].toString().trim();
                        double lat = Double.parseDouble(tokens[2].trim());
                        double lon = -Double.parseDouble(tokens[3].trim());
                        astation.lat = (float) lat;
                        astation.lon = (float) lon;
                        float elev;
                        if (Integer.parseInt(tokens[4].trim()) == 0) {
                            elev = 1;
                        } else {
                            elev = Integer.parseInt(tokens[4].trim());
                        }
                        astation.elev = (int) elev;
                        astation.tip = Integer.parseInt(tokens[5].trim());
                        Coordinate hrap_point;
                        Coordinate latlon = new Coordinate();
                        latlon.x = lon;
                        latlon.y = lat;
                        hrap_point = getLatLontoHrap(latlon);
                        astation.hrap_x = (float) hrap_point.x;
                        astation.hrap_y = (float) hrap_point.y;
                        astation.name = tokens[6].toString().trim();
                        astation.isoh = new float[12];
                        for (int m = 0; m < astation.isoh.length; m++) {
                            astation.isoh[m] = -99.0f;
                        }
                        stations.add(astation);
                    } else {
                        // invalid record
                        continue;
                    }
                }
            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            } catch (NumberFormatException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            } finally {
                try {
                    if (in != null) {
                        in.close();
                    }
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }

        }

        in = null;
        try {
            rec_num = 0;
            // opening custom station list
            String pathNamen = pathName + ".custom";
            in = new BufferedReader(new FileReader(pathNamen));
            String line;
            String[] record;
            int status = 0;
            if (in != null) {
                int i = 0;
                line = in.readLine().trim();
                while (line != null && i < stationCount) {
                    ++rec_num;
                    record = line.split("\\s+", 4);
                    if (record.length != 4) {
                        System.out.println("In file " + pathNamen + " record "
                                + rec_num + " is invalid, skipping.");
                        line = in.readLine().trim();
                        continue;
                    }
                    status = record[0].compareTo(stations.get(i).hb5);
                    if (status < 0) {
                        /* Read the next record from the custom climo file. */
                        line = in.readLine().trim();
                    } else if (status > 0) {
                        /* increment the index in the freezing station array. */
                        ++i;
                    } else {
                        if ((record[1].substring(0, 2)
                                .equalsIgnoreCase(stations.get(i).parm
                                        .substring(0, 2)))
                                && (record[1]
                                        .equalsIgnoreCase(stations.get(i).parm))) {
                            Station statn = dqc.new Station();
                            statn = stations.get(i);
                            statn.xadd = Integer.parseInt(record[2].trim());
                            statn.yadd = Integer.parseInt(record[3].trim());
                            stations.set(i, statn);
                        }
                        ++i;
                    }

                }
            }
        } catch (FileNotFoundException e) {
            System.out.println("Could not open custom freezing station list.");

        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (NumberFormatException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        return stations;
    }

    private String getStationListPath(String qcArea) {
        String dir;

        if (qcArea != null) {
            if (station_dir.length() > 0) {
                dir = station_dir + "/" + qcArea + "_station_list";
            } else {
                dir = qcArea;
            }
        } else {
            if (station_dir.length() > 0) {
                dir = station_dir + "/" + sitename + "_station_list";
            } else {
                dir = sitename;
            }
        }
        return dir;
    }

    private Coordinate getLatLontoHrap(Coordinate latLon) {
        /* calculate HRAP coordinates from lat,lon */
        Coordinate hrap_point = new Coordinate(0, 0);
        try {
            hrap_point = HRAP.getInstance().latLonToGridCoordinate(latLon,
                    PixelOrientation.LOWER_LEFT);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "error computing hrap coordinate", e);
        }
        return hrap_point;
    }

    public int getNumZstations() {
        return max_zstations;
    }

}
