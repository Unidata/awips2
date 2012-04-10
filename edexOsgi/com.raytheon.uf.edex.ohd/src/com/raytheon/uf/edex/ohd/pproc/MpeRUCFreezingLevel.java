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
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Nov 19, 2011        dhladky    Initial Creation.
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

    public File stationFile = null;

    // models used by MPE
    public static String[] models = new String[] { "RUC130", "RUC80" };

    public MpeRUCFreezingLevel() {
        File directory = new File(stationFilePath);

        if (directory != null) {
            for (File file : directory.listFiles()) {
                if (file.isFile()
                        && file.getName().contains("freezing_station_list")) {
                    this.stationFile = file;
                    break;
                }
            }
        }
    }

    /**
     * Process the MPE freezing Levels
     */
    public void processMpeRuc() {

        if (stationFile != null) {

            HashMap<String, FreezingLevel> FreezeLevelMap = new HashMap<String, FreezingLevel>();

            for (String model : models) {
                FreezingLevel fl = new FreezingLevel(model);
                if (fl != null) {
                    FreezeLevelMap.put(model, fl);
                }
            }

            Integer forecastHour = null;
            Date outputDate = null;
            HashMap<String, HashMap<Integer, FreezingLevelXML>> freezingTimeMap = new HashMap<String, HashMap<Integer, FreezingLevelXML>>();
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

                                if (stationId != null && lat != null
                                        && lon != null) {
                                    Coordinate coor = new Coordinate(lon, lat,
                                            0.0);
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

            try {
                HashMap<Coordinate, Float> freezingLevels = null;

                for (String model : models) {
                    if (FreezeLevelMap.containsKey(model)) {
                        FreezingLevel fl = FreezeLevelMap.get(model);
                        ArrayList<FreezingLevelEntry> freezes = null;

                        if (freezingLevels == null) {
                            freezingLevels = fl
                                    .getFreezingLevel(getCoordinates(freezingStations));

                            freezes = new ArrayList<FreezingLevelEntry>();

                            for (Entry<Coordinate, Float> entry : freezingLevels
                                    .entrySet()) {
                                FreezingLevelEntry fle = new FreezingLevelEntry(
                                        entry.getKey(), entry.getValue());
                                freezes.add(fle);
                            }
                        }

                        FreezingLevelXML flx = new FreezingLevelXML(freezes);

                        if (forecastHour == null && outputDate == null) {
                            forecastHour = getConvertedForecastHour(fl);
                            outputDate = fl.getReferenceTime(fl
                                    .getForecastHour());
                        }

                        flx.setForecastHour(forecastHour);
                        HashMap<Integer, FreezingLevelXML> modelFl = new HashMap<Integer, FreezingLevelXML>();
                        modelFl.put(forecastHour, flx);
                        freezingTimeMap.put(model, modelFl);
                        writeFreezingLevelTemp(flx, model);
                    }
                }

            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR,
                        "Processing Freezing Level failed...");
                e.printStackTrace();
            }

            FileOutputStream ofstream = null;
            DataOutputStream out = null;
            BufferedWriter bw = null;

            try {
                // Get other hour files read in
                for (String model : models) {

                    HashMap<Integer, FreezingLevelXML> modelFl = freezingTimeMap
                            .get(model);

                    if (modelFl != null) {
                        for (RUC_TIME time : RUC_TIME.values()) {

                            int fileTime = Integer.valueOf(time.getTime());

                            if (!modelFl.containsKey(fileTime)) {

                                FreezingLevelXML flx1 = readFreezingLevel(
                                        fileTime, model);

                                if (flx1 != null) {
                                    modelFl.put(flx1.getForecastHour(), flx1);
                                }
                            }
                        }

                        freezingTimeMap.put(model, modelFl);
                    }
                }

                // compare first forecast hour list against all others
                String site = PropertiesFactory.getInstance()
                        .getEnvProperties().getEnvValue("SITENAME");
                ArrayList<Integer> orderedHours = getOrderedHours(forecastHour);
                ofstream = new FileOutputStream(getAbsoluteOutFileName(
                        outputDate, site));
                out = new DataOutputStream(ofstream);
                bw = new BufferedWriter(new OutputStreamWriter(out));

                for (Entry<String, Coordinate> entry : freezingStations
                        .entrySet()) {

                    Coordinate coor = entry.getValue();

                    StringBuffer buf = new StringBuffer();
                    // ".E Z$stn $otdate1 DH18/HZIRZ/DIH+6/ $v0/ $v1/ $v2/ $v3\n"
                    buf.append(".E " + entry.getKey() + " "
                            + getFormattedDate(outputDate)
                            + " DH18/HZIRZ/DIH+6/");

                    int i = 0;
                    for (Integer hour : orderedHours) {

                        FreezingLevelEntry fle = null;

                        // Does a preference for the first model defined. RUC130
                        // has higher resolution so it gets preference
                        for (String model : models) {
                            if (fle == null) {
                                if (freezingTimeMap.containsKey(model)) {
                                    HashMap<Integer, FreezingLevelXML> modelFl = freezingTimeMap
                                            .get(model);

                                    if (modelFl.containsKey(hour)) {
                                        FreezingLevelXML flx = modelFl
                                                .get(hour);
                                        fle = flx.getEntry(coor);
                                    }
                                }
                            }
                        }

                        String fzlev = "M";

                        if (fle != null) {
                            fzlev = "" + fle.getFreezingLevel();
                        }

                        buf.append(" " + fzlev);

                        if (i < 3) {
                            buf.append("/");
                        } else {
                            buf.append("\n");
                        }
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

        } else {
            statusHandler.handle(Priority.ERROR,
                    "File freezing_station_list not found....");
        }
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
     * Gets the ordering for the SHEF output
     * 
     * @param forecastHour
     * @return
     */
    private ArrayList<Integer> getOrderedHours(int forecastHour) {

        ArrayList<Integer> orderedHours = new ArrayList<Integer>();

        if (forecastHour == 0) {
            orderedHours.add(0);
            orderedHours.add(6);
            orderedHours.add(12);
            orderedHours.add(18);
        } else if (forecastHour == 6) {
            orderedHours.add(6);
            orderedHours.add(12);
            orderedHours.add(18);
            orderedHours.add(0);
        } else if (forecastHour == 12) {
            orderedHours.add(12);
            orderedHours.add(18);
            orderedHours.add(0);
            orderedHours.add(6);
        } else if (forecastHour == 18) {
            orderedHours.add(18);
            orderedHours.add(0);
            orderedHours.add(6);
            orderedHours.add(12);
        }

        return orderedHours;
    }

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

    /**
     * Used only by the MPE RUC130 for SHEF output
     * 
     * @param ft
     */
    private int getConvertedForecastHour(FreezingLevel fl) {

        int flTime = fl.getForecastHour();
        int retVal = 0;

        if (flTime >= 0 && flTime < 3) {
            retVal = 0;
        } else if (flTime >= 3 && flTime < 9) {
            retVal = 6;
        } else if (flTime >= 9 && flTime < 15) {
            retVal = 12;
        } else if (flTime >= 15 && flTime < 21) {
            retVal = 18;
        } else if (flTime >= 21) {
            retVal = 0;
        }

        return retVal;
    }

}
