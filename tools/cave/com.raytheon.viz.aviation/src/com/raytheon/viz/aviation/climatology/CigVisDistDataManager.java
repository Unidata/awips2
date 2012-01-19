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

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2010            avarani     Initial creation
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */

public class CigVisDistDataManager {
    private String[] winDirsArray = { "  N", "NNE", " NE", "ENE", "  E", "ESE",
            " SE", "SSE", "  S", "SSW", " SW", "WSW", "  W", "WNW", " NW",
            "NNW", "VRB", "  C" };

    final String[] months = { "null", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

    public enum Element {
        VISIBILITY, CEILING, JOINT
    }

    private final int NUM_ELEMENTS = Element.values().length;

    private final int NUM_FLIGHT_CATS = 5;

    private final int NUM_HOURS = 24;

    private final int NUM_MONTHS = 13; // 12 months + annual total

    private final int NUM_WIND_DIRS = winDirsArray.length;

    private final int MONTHS_ANNUAL_INDEX = NUM_MONTHS - 1;

    // Dimensions are Element, flight cat, hour, month + annual, and wind dir
    private float[][][][][] dataArray;

    private int startYear;

    private int endYear;

    private String siteId;

    public CigVisDistDataManager() {
        dataArray = new float[NUM_ELEMENTS][NUM_FLIGHT_CATS][NUM_HOURS][NUM_MONTHS][NUM_WIND_DIRS];
    }

    public void setYears(int startYear, int endYear) {
        this.startYear = startYear;
        this.endYear = endYear;
    }

    public int getStartYear() {
        return startYear;
    }

    public int getEndYear() {
        return endYear;
    }

    public void setSite(String site) {
        siteId = site;
    }

    public String getSite() {
        return siteId;
    }

    public void set(int month, int hour, int windDir, int flightCat, float vis,
            float cig, float jnt) {
        dataArray[0][flightCat][hour][month][windDir] = vis;
        dataArray[0][flightCat][hour][MONTHS_ANNUAL_INDEX][windDir] += vis;
        dataArray[1][flightCat][hour][month][windDir] = cig;
        dataArray[1][flightCat][hour][MONTHS_ANNUAL_INDEX][windDir] += cig;
        dataArray[2][flightCat][hour][month][windDir] = jnt;
        dataArray[2][flightCat][hour][MONTHS_ANNUAL_INDEX][windDir] += jnt;
    }

    // Returned dimensions are flight cat and hour
    public float[][] getDataByHour(Element element, int month, int numMonths) {
        month--;
        float[][] tempData = new float[NUM_FLIGHT_CATS][NUM_HOURS];

        for (int c = 0; c < NUM_FLIGHT_CATS; c++) {
            for (int h = 0; h < NUM_HOURS; h++) {
                for (int m0 = month; m0 < (month + numMonths); m0++) {
                    int m = m0;

                    if (m >= (NUM_MONTHS - 1)) {
                        m -= (NUM_MONTHS - 1);
                    }

                    for (int d = 0; d < NUM_WIND_DIRS; d++) {
                        tempData[c][h] += dataArray[element.ordinal()][c][h][m][d];
                    }
                }
            }
        }

        return tempData;
    }

    // Returned dimensions are flight cat and month
    public float[][] getDataByMonth(Element element, int hour, int numHours) {
        float[][] tempData = new float[NUM_FLIGHT_CATS][NUM_MONTHS];

        for (int c = 0; c < NUM_FLIGHT_CATS; c++) {
            for (int h0 = hour; h0 < (hour + numHours); h0++) {
                int h = h0;

                if (h >= NUM_HOURS) {
                    h -= NUM_HOURS;
                }

                for (int m = 0; m < NUM_MONTHS; m++) {
                    for (int d = 0; d < NUM_WIND_DIRS; d++) {
                        tempData[c][m] += dataArray[element.ordinal()][c][h][m][d];
                    }
                }
            }
        }

        return tempData;
    }

    // Returned dimensions are flight cat and wind dir
    public float[][] getDataByWindDir(Element element, int hour, int numHours,
            int month, int numMonths) {
        month--;
        float[][] tempData = new float[NUM_FLIGHT_CATS][NUM_WIND_DIRS];

        for (int c = 0; c < NUM_FLIGHT_CATS; c++) {
            for (int h0 = hour; h0 < (hour + numHours); h0++) {
                int h = h0;

                if (h >= NUM_HOURS) {
                    h -= NUM_HOURS;
                }

                for (int m0 = month; m0 < (month + numMonths); m0++) {
                    int m = m0;

                    if (m >= (NUM_MONTHS - 1)) {
                        m -= (NUM_MONTHS - 1);
                    }

                    for (int d = 0; d < NUM_WIND_DIRS; d++) {
                        tempData[c][d] += dataArray[element.ordinal()][c][h][m][d];
                    }
                }
            }
        }

        return tempData;
    }

    private String elementStr(Element element) {
        int ord = element.ordinal();
        if (ord == 0) {
            return "Visibility";
        } else if (ord == 1) {
            return "Ceiling";
        } else {
            return "Flight Category";
        }
    }

    private String monthStr(int startMonth, int endMonth) {
        int numMonths = endMonth - startMonth;

        if (numMonths < 1) {
            numMonths += 12;
        }

        if (numMonths == 1) {
            return months[startMonth];
        } else {
            return String.format("%s-%s", months[startMonth],
                    months[endMonth - 1]);
        }
    }

    private String hourStr(int startHour, int endHour) {
        int numHours = endHour - startHour;

        if (numHours < 1) {
            numHours += 24;
        }

        if (numHours == 1) {
            return Integer.toString(startHour);
        } else {
            return String.format("%02d-%02d", startHour, endHour);
        }
    }

    public void saveStatsByMonth(String filename, Element element,
            int startHour, int endHour) {
        int numHours = endHour - startHour;

        if (numHours < 1) {
            numHours += 24;
        }

        File file = new File(filename);
        FileWriter writer;
        try {
            writer = new FileWriter(file);
            BufferedWriter buf = new BufferedWriter(writer);

            buf
                    .write(String.format("%s (%d-%d)\n", siteId, startYear,
                            endYear));

            for (int i = 0; i < numHours; i++) {
                int hour = startHour + i;
                if (hour >= 24) {
                    hour -= 24;
                }

                buf.write(String.format("HOUR: %02dZ         %s\n", hour,
                        elementStr(element)));
                buf.write("MONTH VLIFR   LIFR    IFR     MVFR    VFR\n");

                float[][] data = new float[NUM_FLIGHT_CATS][NUM_MONTHS];

                for (int c = 0; c < NUM_FLIGHT_CATS; c++) {
                    for (int m = 0; m < (NUM_MONTHS - 1); m++) {
                        for (int d = 0; d < NUM_WIND_DIRS; d++) {
                            data[c][m] += dataArray[element.ordinal()][c][hour][m][d];
                        }
                    }
                }

                for (int j = 0; j < (NUM_MONTHS - 1); j++) {
                    buf.write(String.format("%s   %-8s%-8s%-8s%-8s%s\n",
                            months[j + 1], String.format("%1.2f", data[0][j]),
                            String.format("%1.2f", data[1][j]), String.format(
                                    "%1.2f", data[2][j]), String.format(
                                    "%1.2f", data[3][j]), String.format(
                                    "%1.2f", data[4][j])));
                }

                buf.write("\n");
            }

            buf.close();
            writer.close();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public void saveStatsByHour(String filename, Element element,
            int startMonth, int endMonth) {
        int numMonths = endMonth - startMonth;

        if (numMonths < 1) {
            numMonths += 12;
        }

        File file = new File(filename);
        FileWriter writer;
        try {
            writer = new FileWriter(file);
            BufferedWriter buf = new BufferedWriter(writer);

            buf
                    .write(String.format("%s (%d-%d)\n", siteId, startYear,
                            endYear));

            for (int i = 0; i < numMonths; i++) {
                int month = startMonth + i;

                if (month >= 12) {
                    month -= 12;
                }

                buf.write(String.format("MONTH: %s        %s\n",
                        months[month + 1], elementStr(element)));
                buf.write("DIR   VLIFR   LIFR    IFR     MVFR    VFR\n");

                float[][] data = new float[NUM_FLIGHT_CATS][NUM_HOURS];

                for (int c = 0; c < NUM_FLIGHT_CATS; c++) {
                    for (int h = 0; h < NUM_HOURS; h++) {
                        for (int d = 0; d < NUM_WIND_DIRS; d++) {
                            data[c][h] += dataArray[element.ordinal()][c][h][month][d];
                        }
                    }
                }

                for (int j = 0; j < NUM_HOURS; j++) {
                    buf.write(String.format("%02dZ   %-8s%-8s%-8s%-8s%s\n", j,
                            String.format("%1.2f", data[0][j]), String.format(
                                    "%1.2f", data[1][j]), String.format(
                                    "%1.2f", data[2][j]), String.format(
                                    "%1.2f", data[3][j]), String.format(
                                    "%1.2f", data[4][j])));
                }

                buf.write("\n");
            }

            buf.close();
            writer.close();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public void saveStatsByWindDir(String filename, Element element,
            int startHour, int endHour, int startMonth, int endMonth) {
        int numHours = endHour - startHour;

        if (numHours < 1) {
            numHours += 24;
        }

        int numMonths = endMonth - startMonth;

        if (numMonths < 1) {
            numMonths += 12;
        }

        File file = new File(filename);
        FileWriter writer;
        try {
            writer = new FileWriter(file);
            BufferedWriter buf = new BufferedWriter(writer);

            buf
                    .write(String.format("%s (%d-%d)\n", siteId, startYear,
                            endYear));
            buf.write(String.format("HOUR: %sZ MONTH: %s  %s\n", hourStr(
                    startHour, endHour), monthStr(startMonth, endMonth),
                    elementStr(element)));
            buf.write("DIR   VLIFR   LIFR    IFR     MVFR    VFR\n");

            float[][] data = new float[NUM_FLIGHT_CATS][NUM_WIND_DIRS];

            for (int c = 0; c < NUM_FLIGHT_CATS; c++) {
                for (int h = 0; h < numHours; h++) {
                    int hour = startHour + h;
                    if (hour >= 24) {
                        hour -= 24;
                    }

                    for (int m = 0; m < numMonths; m++) {
                        int month = startMonth + m;

                        if (month >= 12) {
                            month -= 12;
                        }

                        for (int d = 0; d < NUM_WIND_DIRS; d++) {
                            data[c][d] += dataArray[element.ordinal()][c][h][m][d];
                        }
                    }
                }
            }

            for (int i = 0; i < NUM_WIND_DIRS; i++) {
                buf.write(String
                        .format("%s   %-8s%-8s%-8s%-8s%s\n", winDirsArray[i],
                                String.format("%1.2f", data[0][i]), String
                                        .format("%1.2f", data[1][i]), String
                                        .format("%1.2f", data[2][i]), String
                                        .format("%1.2f", data[3][i]), String
                                        .format("%1.2f", data[4][i])));
            }

            buf.close();
            writer.close();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
