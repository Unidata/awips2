package com.raytheon.uf.edex.ohd.pproc;

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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map.Entry;

import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.monitor.xml.FreezingLevelEntry;
import com.raytheon.uf.common.monitor.xml.FreezingLevelXML;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.props.PropertiesFactory;
import com.raytheon.uf.edex.dat.utils.FreezingLevel;
import com.vividsolutions.jts.geom.Coordinate;

public class MpeRUCFreezingLevel {

    /**
     * MPE RUC calculator
     * 
     * <pre>
     * SOFTWARE HISTORY
     * Date         Ticket#  Engineer    Description
     * ------------ -------- --------- --------------------------
     * Nov 19, 2011          dhladky    Initial Creation.
     * Oct 09, 2012 15168    wkwock     Fix incorrect values.
     * 
     * </pre>
     * 
     * @author dhladky
     * @version 1.0
     */

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MpeRUCFreezingLevel.class);

    public static String stationFilePath = AppsDefaults.getInstance().getToken(
            "mpe_station_list_dir");

    public static String modelOutputFilePath = AppsDefaults.getInstance()
            .getToken("mpe_point_freezing_dir");

    public static String dqcPreprocessorBasetime = AppsDefaults.getInstance()
            .getToken("DQC_PREPROCESSOR_BASETIME");

    public File stationFile = null;

    final String RUC2 = "RUC236";

    // models used by MPE
    public static String[] models = new String[] { "RUC236" };

    public MpeRUCFreezingLevel() {
        try {
            File directory = new File(stationFilePath);

            if (directory != null) {
                for (File file : directory.listFiles()) {
                    if (file != null) {
                        if (file.isFile()
                                && file.getName().contains(
                                        "freezing_station_list")) {
                            this.stationFile = file;
                            break;
                        }
                    }
                }
            }
        } catch (Exception e) {
            statusHandler
                    .handle(Priority.WARN, "No mpe_station_list_dir found");
        }

        // correct env vairiable dqcPreprocessorBasetime if needed
        if (dqcPreprocessorBasetime == null) {
            dqcPreprocessorBasetime = "00z";
        }

        dqcPreprocessorBasetime = dqcPreprocessorBasetime.toLowerCase();
        if (!dqcPreprocessorBasetime.equals("00z")
                && !dqcPreprocessorBasetime.equals("06z")
                && !dqcPreprocessorBasetime.equals("12z")
                && !dqcPreprocessorBasetime.equals("18z")) {
            dqcPreprocessorBasetime = "00z";
        }
    }

    /**
     * Read freezing station list from file
     * $mpe_station_list_dir/$SITENAME_freezing_station_list
     */
    private LinkedHashMap<String, Coordinate> readFrzStnLst() {
        if (stationFile == null) {
            statusHandler.handle(Priority.ERROR,
                    "File freezing_station_list not found....");
            return null;
        }

        LinkedHashMap<String, Coordinate> freezingStations = new LinkedHashMap<String, Coordinate>();

        FileInputStream ifstream = null;
        DataInputStream in = null;
        BufferedReader br = null;

        try {
            ifstream = new FileInputStream(stationFile);
            in = new DataInputStream(ifstream);
            br = new BufferedReader(new InputStreamReader(in));
            String line;
            Double lat = null;
            Double lon = null;
            while (br.ready()) {
                line = br.readLine();
                if (line != null) {
                    statusHandler.handle(Priority.INFO, line);
                    String[] aline = line.split("\\s+");

                    if (aline != null && aline.length > 0) {
                        try {
                            String stationId = aline[0].trim();
                            lat = Double.valueOf(aline[2].trim());
                            // take times negative 1 to make it true West
                            lon = Double.valueOf(aline[3].trim()) * (-1);

                            if (stationId != null && lat != null && lon != null) {
                                Coordinate coor = new Coordinate(lon, lat, 0.0);
                                freezingStations.put(stationId, coor);
                            }
                        } catch (Exception e) {
                            statusHandler.handle(
                                    Priority.INFO,
                                    "finished parsing "
                                            + stationFile.getAbsolutePath()
                                            + " \n");
                        }
                    }
                }
            }

        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Unable to read file "
                    + stationFile.getAbsolutePath() + " \n");
            e.printStackTrace();
            return null;
        } finally {
            if (br != null) {
                try {
                    br.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            if (ifstream != null) {
                try {
                    ifstream.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }

        return freezingStations;
    }

    private void writeResult(
            LinkedHashMap<String, Coordinate> freezingStations,
            HashMap<String, HashMap<Integer, FreezingLevelXML>> freezingTimeMap) {
        // Now write to the result file freezing_1_SITENAME_point_yyyymmdd
        // compare first forecast hour list against all others
        FileOutputStream ofstream = null;
        DataOutputStream out = null;
        BufferedWriter bw = null;
        Calendar dates[] = getSortedDates(dqcPreprocessorBasetime);// get the
                                                                   // dates and
                                                                   // hours in
                                                                   // order

        try {
            String site = PropertiesFactory.getInstance().getEnvProperties()
                    .getEnvValue("SITENAME");
            ofstream = new FileOutputStream(getAbsoluteOutFileName(
                    dates[3].getTime(), site));
            out = new DataOutputStream(ofstream);
            bw = new BufferedWriter(new OutputStreamWriter(out));

            String dhStr = "DH" + dqcPreprocessorBasetime.substring(0, 2);

            for (Entry<String, Coordinate> entry : freezingStations.entrySet()) {

                Coordinate coor = entry.getValue();

                StringBuffer buf = new StringBuffer();
                // ".E Z$stn $otdate1 DH18/HZIRZ/DIH+6/ $v0/ $v1/ $v2/ $v3\n"

                buf.append(".E " + entry.getKey() + " "
                        + getFormattedDate(dates[3].getTime()) + " " + dhStr
                        + "/HZIRZ/DIH+6/");

                int i = 0;
                for (int j = 0; j < dates.length; j++) {

                    FreezingLevelEntry fle = null;

                    // Does a preference for the first model defined. RUC130
                    // has higher resolution so it gets preference
                    if (fle == null) {
                        if (freezingTimeMap.containsKey(RUC2)) {
                            HashMap<Integer, FreezingLevelXML> modelFl = freezingTimeMap
                                    .get(RUC2);

                            if (modelFl.containsKey(dates[j]
                                    .get(Calendar.HOUR_OF_DAY))) {
                                FreezingLevelXML flx = modelFl.get(dates[j]
                                        .get(Calendar.HOUR_OF_DAY));
                                if (flx != null && flx.getDate() != null) {
                                    // same expected year,month,day,and hour
                                    if (Math.floor(flx.getDate().getTime() / 1000 / 60 / 60) == Math
                                            .floor(dates[j].getTimeInMillis() / 1000 / 60 / 60)) {
                                        fle = flx.getEntry(coor);
                                    }
                                }
                            }
                        }
                    }

                    String fzlev = "M";

                    if (fle != null) {
                        fzlev = String.valueOf(fle.getFreezingLevel())
                                .substring(0, 4) + "S";
                    }

                    buf.append(" " + fzlev);

                    if (i < 3) {
                        buf.append("/");
                    }
                    i++;
                }

                statusHandler.handle(Priority.INFO, buf.toString());
                bw.write(buf.toString() + "\n");
            }

        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Processing Level SHEF output failed...");
            e.printStackTrace();
        } finally {

            if (bw != null) {
                try {
                    bw.flush();
                    bw.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            if (out != null) {
                try {
                    out.flush();
                    out.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            if (ofstream != null) {
                try {
                    ofstream.flush();
                    ofstream.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }

    }

    /**
     * Process the MPE freezing Levels
     */
    public void processMpeRuc() {
        if (!AppsDefaults.getInstance().setAppContext(this)) {
            return;
        }

        Integer forecastHour = null;
        HashMap<String, HashMap<Integer, FreezingLevelXML>> freezingTimeMap = new HashMap<String, HashMap<Integer, FreezingLevelXML>>();
        LinkedHashMap<String, Coordinate> freezingStations = new LinkedHashMap<String, Coordinate>();

        // get the freezing station list from file
        freezingStations = readFrzStnLst();
        if (freezingStations == null) {
            return;
        }

        // get data from hdf5 for this run
        FreezingLevel fl = new FreezingLevel(RUC2);

        // Save data for future use
        try {
            HashMap<Coordinate, Float> freezingLevels = null;

            ArrayList<FreezingLevelEntry> freezes = null;

            if (freezingLevels == null) {
                freezingLevels = fl
                        .getFreezingLevel(getCoordinates(freezingStations));

                freezes = new ArrayList<FreezingLevelEntry>();

                for (Entry<Coordinate, Float> entry : freezingLevels.entrySet()) {
                    FreezingLevelEntry fle = new FreezingLevelEntry(
                            entry.getKey(), entry.getValue());
                    freezes.add(fle);
                }
            }

            FreezingLevelXML flx = new FreezingLevelXML(freezes);

            Calendar refTime = Calendar.getInstance();
            // only for get data for hour 00z,06z,12z, or 18z
            int adjustedHour = (refTime.get(Calendar.HOUR_OF_DAY) / 6) * 6;
            refTime.set(Calendar.HOUR_OF_DAY, adjustedHour);
            flx.setDate(refTime.getTime());
            flx.setForecastHour(adjustedHour);

            HashMap<Integer, FreezingLevelXML> modelFl = new HashMap<Integer, FreezingLevelXML>();
            modelFl.put(forecastHour, flx);
            freezingTimeMap.put(RUC2, modelFl);
            writeFreezingLevelTemp(flx, RUC2);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Processing Freezing Level failed...");
            e.printStackTrace();
        }

        // get all data from previously saved data from
        // mpe/dailyQC/freezing_level/point/RUC236[0,6,12,18]zFreezingLevel.bin
        // Get other hour files read in
        for (String model : models) {

            HashMap<Integer, FreezingLevelXML> modelFl = freezingTimeMap
                    .get(model);

            if (modelFl != null) {
                for (RUC_TIME time : RUC_TIME.values()) {

                    int fileTime = Integer.valueOf(time.getTime());

                    if (!modelFl.containsKey(fileTime)) {

                        FreezingLevelXML flx1 = readFreezingLevel(fileTime,
                                model);

                        if (flx1 != null) {
                            modelFl.put(flx1.getForecastHour(), flx1);
                        }
                    }
                }

                freezingTimeMap.put(model, modelFl);
            }
        }

        // now write to result file freezing_1_SITENAME_point_yyyymmdd
        writeResult(freezingStations, freezingTimeMap);
    }

    /**
     * setup dates
     */
    private Calendar[] getSortedDates(String dqcPreprocessorBasetime) {
        Calendar dates[] = new Calendar[4];

        Calendar cdate = Calendar.getInstance();// start date
        Calendar tdate = Calendar.getInstance();// end date
        int currentHour = cdate.get(Calendar.HOUR_OF_DAY);

        int dqcHour = Integer.parseInt(dqcPreprocessorBasetime.substring(0, 2));
        if (currentHour < dqcHour && dqcHour != 0) {
            cdate.add(Calendar.DATE, -1);
        } else if (dqcHour != 0) {
            tdate.add(Calendar.DATE, 1);
        }

        Calendar startDate = tdate;
        if (dqcPreprocessorBasetime.equalsIgnoreCase("00z")) {
            startDate = (Calendar) tdate.clone();
        } else {
            startDate = (Calendar) cdate.clone();
        }

        startDate.set(Calendar.HOUR_OF_DAY, dqcHour);

        for (int i = 0; i < dates.length; i++) {
            dates[i] = (Calendar) startDate.clone();
            startDate.add(Calendar.HOUR_OF_DAY, 6);
        }

        return dates;
    }

    /**
     * Gets the completed Temp filename
     * 
     * @return
     */
    public String getAbsoluteTempFileName(int forecastHour, String modelName) {
        return modelOutputFilePath + File.separatorChar + modelName
                + forecastHour + "zFreezingLevel" + ".bin";
    }

    /**
     * Gets the completed filename
     * 
     * @return
     */
    public String getAbsoluteOutFileName(Date date, String site) {
        return modelOutputFilePath + File.separatorChar + "freezing_1_" + site
                + "_point_" + getFormattedDate(date);
    }

    /**
     * Write your freezing level tempfiles
     * 
     * @param FreezingLevelXML
     */
    public void writeFreezingLevelTemp(FreezingLevelXML freezingLevel,
            String modelName) {

        try {

            String path = getAbsoluteTempFileName(
                    freezingLevel.getForecastHour(), modelName);
            File file = new File(path);
            byte[] bdata = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(freezingLevel);
            FileUtil.bytes2File(bdata, file);

            statusHandler.handle(
                    Priority.INFO,
                    "Wrote MPE RUC Freezing Level Temp: "
                            + freezingLevel.getForecastHour());

        } catch (SerializationException se) {
            se.printStackTrace();
        } catch (FileNotFoundException fnfe) {
            fnfe.printStackTrace();
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }
    }

    /**
     * Load freezingLevel
     * 
     * @param forecastHour
     * @return
     */
    private FreezingLevelXML readFreezingLevel(int forecastHour,
            String modelName) {

        String path = getAbsoluteTempFileName(forecastHour, modelName);
        File file = new File(path);
        FreezingLevelXML freezingLevel = null;

        try {
            freezingLevel = (FreezingLevelXML) SerializationUtil
                    .transformFromThrift(FileUtil.file2bytes(file, false));
        } catch (SerializationException e) {
            statusHandler.handle(Priority.ERROR,
                    "Couldn't find MPE RUC Freezing Level Temp File (Serial Exception): "
                            + path);
        } catch (IOException e) {
            statusHandler.handle(Priority.ERROR,
                    "Couldn't find MPE RUC Freezing Level Temp File (IO Exception): "
                            + path);
        }

        return freezingLevel;
    }

    /**
     * Time ENUM
     * 
     * @author dhladky
     * 
     */
    public enum RUC_TIME {

        z00("0"), z06("6"), z12("12"), z18("18");

        private final String time;

        private RUC_TIME(String name) {
            time = name;
        }

        public String getTime() {
            return time;
        }
    };

    /**
     * Formats the output date
     * 
     * @param date
     * @return
     */
    private String getFormattedDate(Date date) {
        if (date != null) {
            SimpleDateFormat formatter = ShefConstants.YYYYMMDD_FORMAT;
            return formatter.format(date);
        } else {
            return null;
        }
    }

    /**
     * Get just the coordinates
     * 
     * @param freezingStations
     * @return
     */
    private ArrayList<Coordinate> getCoordinates(
            HashMap<String, Coordinate> freezingStations) {
        ArrayList<Coordinate> coors = new ArrayList<Coordinate>();

        for (Entry<String, Coordinate> entry : freezingStations.entrySet()) {
            coors.add(entry.getValue());
        }

        return coors;
    }
}
