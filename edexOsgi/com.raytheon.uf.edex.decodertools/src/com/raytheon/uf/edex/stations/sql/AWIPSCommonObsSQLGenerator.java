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
package com.raytheon.uf.edex.stations.sql;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.text.DecimalFormat;
import java.util.HashMap;
import java.util.Iterator;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;

/**
 * Give this program a path containing the files you wish to read. It will then
 * give you back SQL files you can then insert into the AWIPS DB
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial release.
 * Mar 06, 2014   2876     mpduff      New NDM plugin.
 * 
 * </pre>
 * 
 * @author dhladky
 * 
 */
public class AWIPSCommonObsSQLGenerator implements INationalDatasetSubscriber {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AWIPSCommonObsSQLGenerator.class);

    private static final String FILE_NAME_PREFIX = "common_obs_spatial_";

    private static final String FILE_NAME_SUFFIX = ".sql";

    public static final int FILE_SIZE_LIMIT = 4000;

    public static final String RAOB = "RAOB";

    public static final String SYNOPTIC = "SYNOPTIC";

    public static final String SYNOPTIC_BLANK_POS = "9999";

    public static final String SYNOPTIC_BLANK_NEG = "-9999";

    public static final String NULL = "NULL";

    public static final String DELIMITER = "\\|";

    public static final String SKIP = "#";

    public static final String INSERT_PREFIX = "INSERT INTO common_obs_spatial (gid, country, elevation, icao, the_geom, name, rbsnindicator, state, upperairelevation, upperairgeom, wmoindex, wmoregion) VALUES (";

    public static final DecimalFormat FORMAT = new DecimalFormat();

    private HashMap<String, SpatialLine> stationMap = null;

    private String path = null;

    public int count = 0;

    public int read = 0;

    public AWIPSCommonObsSQLGenerator(String path) {

        FORMAT.setMaximumFractionDigits(2);
        FORMAT.setMinimumIntegerDigits(1);
        this.path = path;
        this.stationMap = new HashMap<String, SpatialLine>();

        readFile(path + "metarStationInfo.txt");
        readFile(path + "maritimeStationInfo.txt");
        readFile(path + "raobStationInfo.txt");
        readFile(path + "synopticStationTable1.txt");

        try {
            writeFiles();
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }
    }

    /**
     * 
     * @param value
     * @param first
     * @param last
     * @param number
     * @return
     */
    private String addParam(String value, boolean first, boolean last,
            boolean number) {
        String retValue = NULL;
        if (!value.equals(NULL) && !number) {
            retValue = "'" + value + "'";
        }
        if (!value.equals(NULL) && number) {
            retValue = value;
        }

        if (first) {
            retValue = retValue + ",";
        } else if (last) {
            retValue = retValue + ");";
        } else if (!first && !last) {
            retValue = retValue + ",";
        }

        return retValue;
    }

    /**
     * Write out the files to use for your SQL
     * 
     * @throws IOException
     */
    private void writeFiles() throws IOException {

        Iterator<String> it = stationMap.keySet().iterator();
        count = 0;
        int fileCount = 1;

        FileWriter fstream = null;
        BufferedWriter bw = null;
        try {
            while (it.hasNext()) {

                if (fstream == null) {
                    fstream = new FileWriter(path + FILE_NAME_PREFIX
                            + (fileCount) + FILE_NAME_SUFFIX);
                }
                if (bw == null) {
                    bw = new BufferedWriter(fstream);
                }

                SpatialLine sline = stationMap.get(it.next());

                String gid = sline.getGid();
                String state = sline.getState();
                String elevation = sline.getElevation();
                String icao = sline.getICAO();
                String the_geom = sline.getGeom();
                String name = sline.getName();
                String rbsnindicator = NULL;
                String country = sline.getCountry();
                // TODO: these are hard coded to the same as surface, must fix
                String upperairelevation = sline.getElevation();
                String upperairgeom = sline.getGeom();
                String wmoindex = sline.getWmo();
                String wmoregion = NULL;

                StringBuffer line = new StringBuffer();
                line.append(INSERT_PREFIX);
                line.append(addParam(gid, true, false, false));
                line.append(addParam(country, false, false, false));
                line.append(addParam(elevation, false, false, true));
                line.append(addParam(icao, false, false, false));
                line.append(addParam(the_geom, false, false, false));
                line.append(addParam(name, false, false, false));
                line.append(addParam(rbsnindicator, false, false, false));
                line.append(addParam(state, false, false, false));
                line.append(addParam(upperairelevation, false, false, true));
                line.append(addParam(upperairgeom, false, false, false));
                line.append(addParam(wmoindex, false, false, false));
                line.append(addParam(wmoregion, false, true, false));

                bw.write(line.toString() + "\n");
                count++;

                if (count == (FILE_SIZE_LIMIT * fileCount) - 1) {
                    bw.close();
                    fstream.close();
                    bw = null;
                    fstream = null;
                    fileCount++;

                    fstream = new FileWriter(path + FILE_NAME_PREFIX
                            + (fileCount) + FILE_NAME_SUFFIX);
                    bw = new BufferedWriter(fstream);
                }
            }
        } finally {
            if (bw != null) {
                bw.close();
            }
            if (fstream != null) {
                fstream.close();
            }
        }
    }

    @Override
    public void notify(String fileName, File file) {
    }

    /**
     * Read the pipe delimited file, 8 columns
     * 
     * @param filename
     */
    private void readFile(String filename) {
        FileInputStream fis = null;
        DataInputStream dis = null;
        BufferedReader br = null;

        try {
            fis = new FileInputStream(filename);
            dis = new DataInputStream(fis);
            br = new BufferedReader(new InputStreamReader(dis));
            String strLine;
            while ((strLine = br.readLine()) != null) {
                if (!strLine.startsWith(SKIP)) {
                    String[] readLine = strLine.split(DELIMITER);
                    SpatialLine line = new SpatialLine(readLine);
                    stationMap.put(line.getGid(), line);
                    read++;
                }
            }
        } catch (IOException ioe) {
            statusHandler.error("Error processing file: " + filename, ioe);
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                } catch (IOException e) {
                    // ignore
                }
            }
            if (dis != null) {
                try {
                    dis.close();
                } catch (IOException e) {
                    // ignore
                }
            }
            if (br != null) {
                try {
                    br.close();
                } catch (IOException e) {
                    // ignore
                }
            }
        }
    }

    /**
     * Give this the path to your files It will then spit back a number of SQL
     * files all less than 4000 lines long.
     * 
     * @param args
     */
    public static void main(String[] args) {
        System.out.println("Starting: ......");
        System.out.println("\n");
        AWIPSCommonObsSQLGenerator gen = new AWIPSCommonObsSQLGenerator(args[0]);
        System.out.println("\n");
        System.out.println("Ending: ......wrote: " + gen.count + " read: "
                + gen.read);
    }

}
