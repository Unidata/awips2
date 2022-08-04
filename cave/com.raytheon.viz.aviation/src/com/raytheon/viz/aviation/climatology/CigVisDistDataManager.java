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

import java.io.IOException;
import java.io.Writer;

import org.eclipse.swt.graphics.RGB;

/**
 * CigVisDist Data Manager
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ---------------------------------
 * Jan 19, 2010           avarani   Initial creation
 * May 14, 2020  8067     randerso  Major refactor and code cleanup.
 *
 * </pre>
 *
 * @author avarani
 */

public class CigVisDistDataManager {
    public enum Element {
        VISIBILITY("Visibility"),
        CEILING("Ceiling"),
        JOINT("Flight Category", "Joint");

        private final String displayString;

        private final String buttonLabel;

        private Element(String displayString) {
            this(displayString, displayString);
        }

        private Element(String displayString, String buttonLabel) {
            this.displayString = displayString;
            this.buttonLabel = buttonLabel;
        }

        /**
         * @return the displayString
         */
        public String getDisplayString() {
            return displayString;
        }

        /**
         * @return the buttonLabel
         */
        public String getButtonLabel() {
            return buttonLabel;
        }
    }

    public enum FlightCategory {
        VLIFR(new RGB(171, 0, 255)),
        LIFR(new RGB(255, 0, 0)),
        IFR(new RGB(255, 255, 0)),
        MVFR(new RGB(0, 255, 0)),
        VFR(null);

        private final RGB legendColor;

        private FlightCategory(RGB legendColor) {
            this.legendColor = legendColor;
        }

        /**
         * @return the legendColor
         */
        public RGB getLegendColor() {
            return legendColor;
        }

        public boolean isDisplayed() {
            return legendColor != null;
        }
    }

    private static final String[] winDirs = { "N", "NNE", "NE", "ENE", "E",
            "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW",
            "NNW", "VRB", "C" };

    private static final String[] months = { "Jan", "Feb", "Mar", "Apr", "May",
            "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Annual" };

    private static final int NUM_ELEMENTS = Element.values().length;

    private static final int NUM_FLIGHT_CATS = FlightCategory.values().length;

    public static final int NUM_HOURS = 24;

    public static final int NUM_MONTHS = 12;

    private static final int NUM_WIND_DIRS = winDirs.length;

    private static final int MONTHS_ANNUAL_INDEX = NUM_MONTHS;

    private static String[] generateHourLabels() {
        String[] hourLabels = new String[NUM_HOURS];
        for (int hour = 0; hour < hourLabels.length; hour++) {
            hourLabels[hour] = String.format("%02d", hour);
        }

        return hourLabels;
    }

    public enum GraphType {
        MONTH(
                months,
                "%1$s %2$s %3$sZ (%5$d-%6$d)",
                NUM_MONTHS + 1,
                false,
                true,
                false),
        HOUR(
                generateHourLabels(),
                "%1$s %2$s %4$s (%5$d-%6$d)",
                NUM_HOURS,
                false,
                false,
                true),
        WIND_DIR(
                winDirs,
                "%1$s %2$s %3$sZ %4$s (%5$d-%6$d)",
                NUM_WIND_DIRS,
                true,
                true,
                true);

        private final String[] labels;

        private final String legendFormat;

        private final int dimension;

        private final boolean displayHours;

        private final boolean showHourControls;

        private final boolean showMonthControls;

        private GraphType(String[] labels, String legendFormat, int dimension,
                boolean displayHours, boolean showHourControls,
                boolean showMonthControls) {
            this.labels = labels;
            this.legendFormat = legendFormat;
            this.dimension = dimension;
            this.displayHours = displayHours;
            this.showHourControls = showHourControls;
            this.showMonthControls = showMonthControls;
        }

        /**
         * @return the labels
         */
        public String[] getLabels() {
            return labels;
        }

        /**
         * @return the numColumns
         */
        public int getNumColumns() {
            return labels.length;
        }

        /**
         * @return the legendFormat
         */
        public String getLegendFormat() {
            return legendFormat;
        }

        /**
         * @return the dimension
         */
        public int getDimension() {
            return dimension;
        }

        /**
         * @return the displayHours
         */
        public boolean isDisplayHours() {
            return displayHours;
        }

        /**
         * @return the showHourControls
         */
        public boolean isShowHourControls() {
            return showHourControls;
        }

        /**
         * @return the showMonthControls
         */
        public boolean isShowMonthControls() {
            return showMonthControls;
        }
    }

    // Dimensions are Element, flight cat, hour, month + annual, and wind dir
    private float[][][][][] dataArray;

    private int startYear;

    private int endYear;

    private String siteId;

    public CigVisDistDataManager() {
        dataArray = new float[NUM_ELEMENTS][NUM_FLIGHT_CATS][NUM_HOURS][NUM_MONTHS
                + 1][NUM_WIND_DIRS];
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

    /**
     * @param graphType
     * @param element
     * @param hour
     * @param numHours
     * @param month
     * @param numMonths
     * @return 2D array of floats indexed by flight category and graph type
     */
    public float[][] getDataByGraphType(GraphType graphType, Element element,
            int hour, int numHours, int month, int numMonths) {
        float[][] tempData = new float[NUM_FLIGHT_CATS][graphType
                .getDimension()];

        /* array of indices per graphType */
        int[] index = new int[GraphType.values().length];
        int h = GraphType.HOUR.ordinal();
        int m = GraphType.MONTH.ordinal();
        int wd = GraphType.WIND_DIR.ordinal();

        /* index for the selected graphType */
        int x = graphType.ordinal();

        for (int c = 0; c < NUM_FLIGHT_CATS; c++) {
            for (int h0 = 0; h0 < numHours; h0++) {
                index[h] = (hour + h0) % NUM_HOURS;

                for (int m0 = 0; m0 < numMonths; m0++) {
                    index[m] = month + m0;
                    if (numMonths <= NUM_MONTHS) {
                        index[m] %= NUM_MONTHS;
                    }

                    for (int d = 0; d < NUM_WIND_DIRS; d++) {
                        index[wd] = d;
                        tempData[c][index[x]] += dataArray[element
                                .ordinal()][c][index[h]][index[m]][d];
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

    public static String monthStr(int startMonth, int numMonths) {
        if (numMonths == 1) {
            return months[startMonth];
        } else {
            int endMonth = (startMonth + numMonths - 1) % NUM_MONTHS;
            return String.format("%s-%s", months[startMonth], months[endMonth]);
        }
    }

    public static String hourStr(int startHour, int numHours) {
        if (numHours == 1) {
            return String.format("%02d", startHour);
        } else {
            int endHour = (startHour + numHours - 1) % NUM_HOURS;
            return String.format("%02d-%02d", startHour, endHour);
        }
    }

    public void saveStatsByMonth(Writer writer, Element element, int startHour,
            int numHours) throws IOException {

        writer.write(String.format("%s (%d-%d)\n", siteId, startYear, endYear));

        for (int i = 0; i < numHours; i++) {
            int hour = startHour + i;
            if (hour >= 24) {
                hour -= 24;
            }

            writer.write(String.format("HOUR: %02dZ         %s\n", hour,
                    elementStr(element)));
            writer.write("MONTH VLIFR   LIFR    IFR     MVFR    VFR\n");

            float[][] data = new float[NUM_FLIGHT_CATS][NUM_MONTHS + 1];

            for (int c = 0; c < NUM_FLIGHT_CATS; c++) {
                for (int m = 0; m < NUM_MONTHS; m++) {
                    for (int d = 0; d < NUM_WIND_DIRS; d++) {
                        data[c][m] += dataArray[element
                                .ordinal()][c][hour][m][d];
                    }
                }
            }

            for (int j = 0; j < NUM_MONTHS; j++) {
                writer.write(String.format("%s   %-8s%-8s%-8s%-8s%s\n",
                        months[j + 1], String.format("%1.2f", data[0][j]),
                        String.format("%1.2f", data[1][j]),
                        String.format("%1.2f", data[2][j]),
                        String.format("%1.2f", data[3][j]),
                        String.format("%1.2f", data[4][j])));
            }

            writer.write("\n");
        }
    }

    public void saveStatsByHour(Writer writer, Element element, int startMonth,
            int numMonths) throws IOException {

        writer.write(String.format("%s (%d-%d)\n", siteId, startYear, endYear));

        for (int i = 0; i < numMonths; i++) {
            int month = (startMonth + i) % NUM_MONTHS;

            writer.write(String.format("MONTH: %s        %s\n", months[month],
                    elementStr(element)));
            writer.write("DIR   VLIFR   LIFR    IFR     MVFR    VFR\n");

            float[][] data = new float[NUM_FLIGHT_CATS][NUM_HOURS];

            for (int c = 0; c < NUM_FLIGHT_CATS; c++) {
                for (int h = 0; h < NUM_HOURS; h++) {
                    for (int d = 0; d < NUM_WIND_DIRS; d++) {
                        data[c][h] += dataArray[element
                                .ordinal()][c][h][month][d];
                    }
                }
            }

            for (int j = 0; j < NUM_HOURS; j++) {
                writer.write(String.format("%02dZ   %-8s%-8s%-8s%-8s%s\n", j,
                        String.format("%1.2f", data[0][j]),
                        String.format("%1.2f", data[1][j]),
                        String.format("%1.2f", data[2][j]),
                        String.format("%1.2f", data[3][j]),
                        String.format("%1.2f", data[4][j])));
            }

            writer.write("\n");
        }
    }

    public void saveStatsByWindDir(Writer writer, Element element, int startHour,
            int numHours, int startMonth, int numMonths) throws IOException {
        writer.write(String.format("%s (%d-%d)\n", siteId, startYear, endYear));
        writer.write(String.format("HOUR: %sZ MONTH: %s  %s\n",
                hourStr(startHour, numHours), monthStr(startMonth, numMonths),
                elementStr(element)));
        writer.write("DIR   VLIFR   LIFR    IFR     MVFR    VFR\n");

        float[][] data = new float[NUM_FLIGHT_CATS][NUM_WIND_DIRS];

        for (int c = 0; c < NUM_FLIGHT_CATS; c++) {
            for (int h = 0; h < numHours; h++) {
                int hour = startHour + h;
                if (hour >= 24) {
                    hour -= 24;
                }

                for (int m = 0; m < numMonths; m++) {
                    int month = (startMonth + m) % NUM_MONTHS;
                    for (int d = 0; d < NUM_WIND_DIRS; d++) {
                        data[c][d] += dataArray[element
                                .ordinal()][c][h][month][d];
                    }
                }
            }
        }

        for (int i = 0; i < NUM_WIND_DIRS; i++) {
            writer.write(String.format("%s   %-8s%-8s%-8s%-8s%s\n", winDirs[i],
                    String.format("%1.2f", data[0][i]),
                    String.format("%1.2f", data[1][i]),
                    String.format("%1.2f", data[2][i]),
                    String.format("%1.2f", data[3][i]),
                    String.format("%1.2f", data[4][i])));
        }
    }
}
