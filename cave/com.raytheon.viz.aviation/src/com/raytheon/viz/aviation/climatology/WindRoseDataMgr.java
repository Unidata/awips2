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
package com.raytheon.viz.aviation.climatology;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;

import jep.JepException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.multiprocessing.PyProcessListener;
import com.raytheon.uf.common.python.multiprocessing.PythonProcess;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * WindRoseDatMgr class contains the data used to draw the wind rose diagram.
 * 
 * The data is contained in a 3 dimensional array:
 * 
 * Hours (24) Wind Direction (36** + Variable + total) Knots (in 'bins')
 * 
 * Array Index Array Index Array Index [0] [0] --- Wind 10 degrees [0] --- 0 to
 * wind var 1 [1] [1] --- Wind 20 degrees [1] --- wind var 1 to wind var 2 [2]
 * [...] --- Wind (10 degree inc) [2] --- wind var 2 to wind var 3 [...] [34]
 * --- Wind 360 degrees [3] --- above wind var 3 [23] [35] --- Wind totals [4]
 * --- Total knots [36] --- Variable winds ** Note: The Wind direction has a
 * maximum of 36 wind directions (every 10 degrees). There could also be 8 & 16
 * point wind directions. The data array will be sized accordingly.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 18 JUN 2008  1119        lvenable    Initial creation.
 * 8/11/2008    1314        grichard    Used PathManager for pathnames.
 * 2/14/2011    7781        rferrel     Modified objReceived to check data
 *                                      to prevent the indexing error.
 * 3/31/2011    8774        rferrel     killProcess when doing a disposed
 * 4/4/2011     8896        rferrel     Made timeout configurable
 * 3/9/2012     14530       zhao        Revised wind rose plot to match AWIPS-1
 * 19Mar2014    #2925       lvenable    Added dispose checks for runAsync and cleaned up code.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class WindRoseDataMgr implements PyProcessListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WindRoseDataMgr.class);

    private static WindRoseDataMgr instance = null;

    private WindRoseCanvasComp canvas;

    private String siteId;

    private int month;

    private int numMonths;

    private int hour;

    private int numHours;

    private int flightCat;

    /**
     * Array of degrees for the 8 point wind direction display.
     */
    private int[] windDir8 = { 22, 68, 112, 158, 202, 248, 292, 338 };

    /**
     * Array of degrees for the 16 point wind direction display.
     */
    private int[] windDir16 = { 11, 34, 56, 79, 101, 124, 146, 169, 191, 214,
            236, 259, 281, 304, 326, 349 };

    /**
     * Array of degrees for the 36 point wind direction display.
     */
    private int[] windDir36 = { 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105,
            115, 125, 135, 145, 155, 165, 175, 185, 195, 205, 215, 225, 235,
            245, 255, 265, 275, 285, 295, 305, 315, 325, 335, 345, 355 };

    /**
     * Number of wind directions to display on the wind rose diagram.
     */
    private int windDirPoints = 36;

    /**
     * Wind variable 1 maximum value.
     */
    private int windVar1 = 5;

    /**
     * Wind variable 2 maximum value.
     */
    private int windVar2 = 12;

    /**
     * Wind variable 3 maximum value.
     */
    private int windVar3 = 20;

    /**
     * Array index for 1 to wind variable 1.
     */
    private int zeroToWV1Idx = 1;

    /**
     * Array index for wind variable 2 to wind variable 2.
     */
    private int wv1ToWv2Idx = 2;

    /**
     * Array index for wind variable 3 to wind variable 3.
     */
    private int wv2ToWV3Idx = 3;

    /**
     * Array index for above wind variable 3 (v3+).
     */
    private int wv3PlusIdx = 4;

    /**
     * Index of the total counts for each wind direction and knot bin.
     */
    private int windDirTotalIndex = 37;

    /**
     * Index of the Calm wind direction
     */
    private int calmDirIndex = 37;

    /**
     * Index of the variable wind knot bins
     */
    private int variableWindIndex = 36;

    /**
     * Array of wind rose data (hours, wind direction, wind speed).
     */
    private double[][][] windRoseDataArray;

    /**
     * Variable containing all of the calm wind occurrences.
     */
    private double[] calm;

    /**
     * Number of 'bins' for the knots. There are 5 bins - 0 to v1, v1 to v2, v2
     * to v3, v3+, and a total count of the previous bins.
     */
    private int numOfKnotBins = 5;

    /**
     * Index of the total knots count bin.
     */
    public final int knotsTotalIndex = 0;

    private String years;

    private PythonProcess pythonScript = null;

    private WindRoseDataMgr() {
    }

    public static synchronized WindRoseDataMgr getInstance() {
        if (instance == null) {
            instance = new WindRoseDataMgr();
        }
        return instance;
    }

    public static synchronized WindRoseDataMgr getInstance(int points,
            int windVar1, int windVar2, int windVar3, String hour,
            String numHours) {
        WindRoseDataMgr.getInstance();

        if (points != instance.windDirPoints) {
            // If the number of wind directions has changed, we need to reload
            // all the data.
            instance.month = -1;
            instance.numMonths = -1;
            instance.flightCat = -1;
        }

        instance.windDirPoints = points;

        instance.windVar1 = windVar1;
        instance.windVar2 = windVar2;
        instance.windVar3 = windVar3;

        instance.hour = Integer.parseInt(hour);
        instance.numHours = Integer.parseInt(numHours);

        // Since the windRoseDataArray starts at 0, the wind direction total
        // index will be the same number as the wind direction points.
        // The variable wind direction index is one more than the wind direction
        // total index.
        instance.variableWindIndex = instance.windDirPoints;
        instance.windDirTotalIndex = instance.variableWindIndex + 1;
        instance.calmDirIndex = instance.variableWindIndex + 1;

        // Array of [hours][wind direction + variable + total column][knot
        // ranges + total]
        // instance.windRoseDataArray = new double[24][instance.windDirPoints +
        // 2][instance.numOfKnotBins];
        // instance.calm = new double[24];
        return instance;
    }

    public void clearWindRoseData() {
        calm = new double[24];
        windRoseDataArray = new double[24][windDirPoints + 2][numOfKnotBins];
    }

    public void readData(String monthStr, String numMonths,
            final int flightCat, final String site, WindRoseCanvasComp canvas) {
        final int timeout = ClimateTimeoutManager.getInstance()
                .getWindRoseTimeout();
        this.canvas = canvas;

        if (site.equals(siteId) && (Integer.parseInt(monthStr) == month)
                && (Integer.parseInt(numMonths) == this.numMonths)
                && flightCat == this.flightCat) {
            this.canvas.resetCursor();
            return;
        }

        siteId = site;
        month = Integer.parseInt(monthStr);
        this.numMonths = Integer.parseInt(numMonths);
        this.flightCat = flightCat;
        clearWindRoseData();
        final int month = Integer.parseInt(monthStr);
        int endMonth = month + Integer.parseInt(numMonths);

        if (endMonth > 12) {
            endMonth -= 12;
        }

        final int end_month = endMonth;

        t0 = System.currentTimeMillis();

        Runnable run = new Runnable() {

            @Override
            public void run() {
                PythonProcess myPythonScript = null;

                try {
                    String climateFile = null;
                    String configFile = null;
                    climateFile = ClimatePython.getClimateFilePath(site);
                    String filepath = "aviation/config/windrose.cfg";
                    IPathManager pm = PathManagerFactory.getPathManager();
                    LocalizationFile lFile = pm
                            .getStaticLocalizationFile(filepath);
                    File file = lFile.getFile();
                    configFile = file.getPath();
                    myPythonScript = ClimatePython.getClimateInterpreter();
                    if (pythonScript != null) {
                        pythonScript.killProcess();
                        pythonScript = null;
                    }
                    pythonScript = myPythonScript;
                    HashMap<String, Object> args = new HashMap<String, Object>();
                    args.put("month", month);
                    args.put("end_month", end_month);
                    args.put("flight_cat", flightCat);
                    args.put("id_", site);
                    args.put("fname", climateFile);
                    args.put("configFile", configFile);
                    myPythonScript.execute("get_windrose", args,
                            WindRoseDataMgr.this, timeout);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
                } catch (JepException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error retrieving wind rose data", e);
                } finally {
                    if (myPythonScript != null) {
                        myPythonScript.dispose();
                        if (pythonScript == myPythonScript) {
                            pythonScript = null;
                        }
                    }

                    VizApp.runAsync(new Runnable() {
                        @Override
                        public void run() {
                            if (WindRoseDataMgr.this.canvas.isDisposed() == false) {
                                WindRoseDataMgr.this.canvas.resetCursor();
                            }
                        }
                    });
                }
            }
        };
        Thread t = new Thread(run);
        t.start();
    }

    // TODO remove timing once it's good
    private long t0;

    private long t1;

    private long t2;

    @Override
    public void objReceived(Object obj) {
        if (obj instanceof java.util.List) {
            java.util.List<?> list = (java.util.List<?>) obj;
            if (list.size() == 2) {
                t1 = System.currentTimeMillis();
                int startYear = (Integer) list.get(0);
                int endYear = (Integer) list.get(1);
                setYears(startYear, endYear);
            } else {
                int hour = (Integer) list.get(0);
                int windDirBin = (Integer) list.get(1);
                int windSpdBin = (Integer) list.get(2);
                float delta = (Float) list.get(3);
                incrementData(hour, windDirBin, windSpdBin, delta);
            }
        } else {
            t2 = System.currentTimeMillis();
            System.out.println("Total process time: " + (t2 - t0));
            System.out.println("Separate process time: " + (t1 - t0));
            System.out.println("Transferring back time: " + (t2 - t1));
        }
    }

    public void killProcess() {
        if (pythonScript != null) {
            pythonScript.killProcess();
        }
    }

    private void incrementData(int hourIdx, int windDirIdx, int windSpdIdx,
            double delta) {
        if (windDirIdx == calmDirIndex) {
            calm[hourIdx] += delta;
            windRoseDataArray[hourIdx][windDirTotalIndex][knotsTotalIndex] += delta;
            return;
        }
        windRoseDataArray[hourIdx][windDirIdx][windSpdIdx] += delta;
        windRoseDataArray[hourIdx][windDirIdx][knotsTotalIndex] += delta;
        windRoseDataArray[hourIdx][windDirTotalIndex][windSpdIdx] += delta;
        windRoseDataArray[hourIdx][windDirTotalIndex][knotsTotalIndex] += delta;
    }

    /**
     * Get a data array of all wind directions and knots. The data is added up
     * over all 24 hours and put in a two dimensional array of wind
     * direction/knots
     * 
     * @return An array of wind direction/knots.
     */
    public double[][] getAllWindDirDataArray() {
        double[][] allWindDirData = new double[windDirPoints][numOfKnotBins];

        // Loop through all of the hours and add up the wind rose data for
        // each direction.

        for (int i = hour; i < (hour + numHours); ++i) {
            int h = i;
            if (h >= 24) {
                h -= 24;
            }

            for (int j = 0; j < windDirPoints; ++j) {
                for (int k = 0; k < numOfKnotBins; ++k) {
                    allWindDirData[j][k] += windRoseDataArray[h][j][k];
                }
            }
        }

        return allWindDirData;
    }

    public void setYears(int year1, int year2) {
        years = year1 + "-" + year2;
    }

    public String getYears() {
        return years;
    }

    public String getHours() {
        double t = 0;
        for (int i = hour; i < (hour + numHours); ++i) {
            int h = i;
            if (h >= 24) {
                h -= 24;
            }

            t += windRoseDataArray[h][windDirTotalIndex][knotsTotalIndex];
        }
        return String.format("%1.1f", t);
    }

    public double getTotalHoursByHour(int hourIdx) {
        return windRoseDataArray[hourIdx][windDirTotalIndex][knotsTotalIndex];
    }

    private double getCalmValue(int hourIdx) {
        return calm[hourIdx];
    }

    /**
     * Get the Calm value.
     * 
     * @return The Calm value.
     */
    public double getCalmValue() {
        double calmValue = 0;

        for (int i = hour; i < (hour + numHours); ++i) {
            int h = i;
            if (h >= 24) {
                h -= 24;
            }

            calmValue += calm[h];
        }

        return calmValue;
    }

    /**
     * Get the number of wind directions.
     * 
     * @return The number of wind directions.
     */
    public int getNumOfWindDirections() {
        return windDirPoints;
    }

    /**
     * Get the Variable value over all of the wind directions.
     * 
     * @return The Variable total value.
     */
    public double getVariableValue() {
        double varTotal = 0.0;

        for (int i = hour; i < (hour + numHours); ++i) {
            int h = i;
            if (h >= 24) {
                h -= 24;
            }

            varTotal += windRoseDataArray[h][variableWindIndex][knotsTotalIndex];
        }

        return varTotal;
    }

    /**
     * Get the average Variable value over all of the wind directions.
     * 
     * @return The Variable average.
     */
    public double getVariableAverage() {
        double varTotal = 0.0;

        for (int i = hour; i < (hour + numHours); ++i) {
            int h = i;
            if (h >= 24) {
                h -= 24;
            }

            varTotal += windRoseDataArray[h][variableWindIndex][knotsTotalIndex];
        }

        return varTotal / windDirPoints;
    }

    /**
     * Get the average Calm value over all of the wind directions.
     * 
     * @return The Calm average.
     */
    public double getCalmAverage() {
        double calmAve = getCalmValue() / windDirPoints;

        return calmAve;
    }

    /**
     * Get the percents of all of the knots plus calm and variable. The percents
     * are multiplied by 100 so .104 will be stored as 10.4 (for display
     * purposes).
     * 
     * Array indexes: 0 - calm 1 - variable 2 - 0 to v1 3 - v1 to v2 4 - v2 to
     * v3 5 - v3+
     * 
     * @return Array of knot total percentages.
     */
    public double[] getKnotPercents() {
        // 0 - calm
        // 1 - variable
        // 2 - 0 to v1
        // 3 - v1 to v2
        // 4 - v2 to v3
        // 5 - v3+
        double[] dblArray = new double[6];

        double totalCount = getTotalWindDirCount();

        for (int i = hour; i < (hour + numHours); ++i) {
            int h = i;
            if (h >= 24) {
                h -= 24;
            }

            dblArray[0] += calm[h];
            dblArray[1] += windRoseDataArray[h][variableWindIndex][knotsTotalIndex];
            dblArray[2] += windRoseDataArray[h][windDirTotalIndex][zeroToWV1Idx];
            dblArray[3] += windRoseDataArray[h][windDirTotalIndex][wv1ToWv2Idx];
            dblArray[4] += windRoseDataArray[h][windDirTotalIndex][wv2ToWV3Idx];
            dblArray[5] += windRoseDataArray[h][windDirTotalIndex][wv3PlusIdx];
        }

        for (int i = 0; i < dblArray.length; ++i) {
            dblArray[i] = (dblArray[i] / totalCount) * 100;
        }

        return dblArray;
    }

    /**
     * Get the total of all the counts for all of the wind directions/knots,
     * 
     * @return The total count of all winds in all directions.
     */
    public double getTotalWindDirCount() {
        int total = 0;

        for (int x = hour; x < (hour + numHours); ++x) {
            int h = x;
            if (h >= 24) {
                h -= 24;
            }

            total += windRoseDataArray[h][windDirTotalIndex][knotsTotalIndex];
        }

        return total;
    }

    /**
     * Get the largest percent of wind in one direction.
     * 
     * @return The largest percent of wind.
     */
    public double getLargestWindDirectionPercent() {
        double dirPercent = -1.0;
        double tmpDbl = -1.0;

        double calmAve = getCalmAverage();
        double variableAve = getVariableAverage();

        int[] windDirTotals = new int[36];

        // Total wind direction count that will include calm and
        // variable winds
        double totalWindDirCount = getTotalWindDirCount();

        // Find the wind direction that has the greatest percent of wind.
        // Loop over the hours.
        for (int x = hour; x < (hour + numHours); ++x) {
            int h = x;
            if (h >= 24) {
                h -= 24;
            }
            // Loop over the wind directions.
            for (int i = 0; i < windDirPoints; i++) {
                windDirTotals[i] += windRoseDataArray[h][i][knotsTotalIndex];
            }
        }

        for (int j = 0; j < windDirTotals.length; j++) {
            tmpDbl = (windDirTotals[j] + calmAve + variableAve)
                    / totalWindDirCount;
            dirPercent = (dirPercent > tmpDbl) ? dirPercent : tmpDbl;
        }

        return dirPercent;
    }

    public void printData(String filename) {
        File file = new File(filename);
        FileWriter writer = null;
        BufferedWriter buf = null;
        try {
            writer = new FileWriter(file);
            buf = new BufferedWriter(writer);

            for (int i = 0; i < numMonths; i++) {
                int monthIdx = month + i;
                if (monthIdx > 12) {
                    monthIdx -= 12;
                }

                buf.write(siteId + " " + WindRosePlotDlg.MONTHS[monthIdx - 1]
                        + "\n");

                for (int j = 0; j < numHours; j++) {
                    int t = hour + j;
                    if (t >= 24) {
                        t -= 24;
                    }

                    buf.write(String.format("HOUR:      %02dZ\n", t));
                    buf.write(String.format("TOTAL:%8.1f\n",
                            getTotalHoursByHour(t)));
                    buf.write(String.format("CALM:%9.1f\n", getCalmValue(t)));
                    buf.write("SPEED:   0-" + windVar1 + " kt " + windVar1
                            + "-" + windVar2 + " kt " + windVar2 + "-"
                            + windVar3 + " kt " + windVar3 + "+ kt\n");
                    buf.write(String
                            .format("VRB:%10.1f%8.1f%9.1f%7.1f\n",
                                    windRoseDataArray[t][variableWindIndex][zeroToWV1Idx],
                                    windRoseDataArray[t][variableWindIndex][wv1ToWv2Idx],
                                    windRoseDataArray[t][variableWindIndex][wv2ToWV3Idx],
                                    windRoseDataArray[t][variableWindIndex][wv3PlusIdx]));

                    for (int k = 0; k < windDirPoints; k++) {
                        double endAngle = 0;
                        double angleDelta = 0;
                        if (windDirPoints == 8) {
                            endAngle = windDir8[k];
                            angleDelta = windDir8[1] - windDir8[0];
                        } else if (windDirPoints == 16) {
                            endAngle = windDir16[k];
                            angleDelta = windDir16[1] - windDir16[0];
                        } else if (windDirPoints == 36) {
                            endAngle = windDir36[k];
                            angleDelta = windDir36[1] - windDir36[0];
                        }

                        double startAngle = endAngle - angleDelta;

                        if (startAngle < 0) {
                            startAngle += 360;
                        }

                        buf.write(String.format(
                                "%03d-%03d:%6.1f%8.1f%9.1f%7.1f\n",
                                (int) startAngle, (int) endAngle,
                                windRoseDataArray[t][k][zeroToWV1Idx],
                                windRoseDataArray[t][k][wv1ToWv2Idx],
                                windRoseDataArray[t][k][wv2ToWV3Idx],
                                windRoseDataArray[t][k][wv3PlusIdx]));
                    }

                    buf.write("\n");
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                buf.close();
                writer.close();
            } catch (IOException e) {
                // Ignore
            }
        }
    }
}
