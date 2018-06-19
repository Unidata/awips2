package com.raytheon.viz.mpe.ui.dialogs.postanalysis;

import java.io.File;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.graphics.Point;

import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.xmrg.XmrgFile;
import com.raytheon.uf.common.xmrg.hrap.HrapConversionException;
import com.raytheon.uf.common.xmrg.hrap.HrapUtil;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.util.MPEColors;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.GetColorValues;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Class to handle general management of PostAnalysis functionality.
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * December 2013 DCS 167    C. Gobs     Initial version
 * February 2015            C. Gobs     Fixed issue with parsing unexpected file name formats in xmrg directory
 * Aug 11, 2015 4500        rjpeter     Fixed type casts.
 * Aug 11, 2016 4619        bkowal      {@link HrapUtil} was moved to common. Cleanup.
 * </pre>
 * 
 * 
 * @author Chip Gobs
 * 
 */

public class PostAnalysisManager {

    public static final IUFStatusHandler handler = UFStatus
            .getHandler(PostAnalysisManager.class);

    private java.awt.Rectangle extent = null;

    private XmrgFile.XmrgHeader xmrgHeader = null;

    private boolean misbin[][];

    private static final double MISSING_VALUE = -9999.0;

    private static final double MM_PER_INCH = 25.4;

    private MergeParameters mergeParameters = null;

    private static final float DEFAULT_ESTIMATED_SCALE = 1.0f;

    private static final float DEFAULT_RHAT = 0.95f;

    private static Map<String, NamedColorUseSet> colorUseSetMap = new HashMap<String, NamedColorUseSet>();

    private void sortFileNames(List<String> xmrgFileNameList) {
        Comparator<String> fileNameComparator = new FileNameComparator(false);
        Collections.sort(xmrgFileNameList, fileNameComparator);
    }

    /**
     * Retrieves and returns the currently selected {@link Date} from
     * {@link DailyQcUtils}.
     * 
     * @return the currently selected {@link Date}
     */
    public static Date getSelectedDate() {
        Date date = DailyQcUtils.pdata[DailyQcUtils.pcpn_day].data_time;

        return date;
    }

    /**
     * Retrieves and returns the currently selected date from
     * {@link DailyQcUtils} as a formatted {@link String}.
     * 
     * @return a formatted {@link String} representative of the currently
     *         selected date.
     */
    public static String getSelectedDateString() {
        String header = "postAnalysisManager.getSelectedDateString(): ";

        Date date = getSelectedDate();

        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMdd");
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
        String dateString = dateFormat.format(date);

        // this section is just for debugging purposes
        SimpleDateFormat dateFormat2 = new SimpleDateFormat(
                "yyyy-MM- dd HH:mm:ss");
        dateFormat2.setTimeZone(TimeZone.getTimeZone("UTC"));
        String dateString2 = dateFormat2.format(date);
        handler.debug(header + " my reformatted selected Date = " + dateString2);

        return dateString;

    }

    /**
     * Retrieves and returns the location of the 24-hour gage only file
     * directory as a {@link File} from Apps Defaults.
     * 
     * @return the 24-hour gage only file directory as a {@link File}
     */
    public static String get24HourGageOnlyFilePath() {
        String filePath = null;
        final String mpe_grid_precip_dir_tok = "mpe_grid_precip_dir";
        AppsDefaults ad = AppsDefaults.getInstance();

        String mpe_grid_precip_dir = ad.getToken(mpe_grid_precip_dir_tok);

        String currentQcArea = DailyQcUtils.currentQcArea;

        String dateString = PostAnalysisManager.getSelectedDateString();

        if (mpe_grid_precip_dir != null) {
            filePath = mpe_grid_precip_dir + "/precip_" + currentQcArea
                    + "_grid_" + "12_12_" + dateString;
        }

        handler.debug("FilePath for 24-hour gage field = " + filePath);

        return filePath;
    }

    public long getTimeFromFileName(String fileName) {
        return getLongTimeFromString(getTimeStringFromFileName(fileName));
    }

    private long getLongTimeFromString(String timeString) {
        Date shiftedDate = null;
        long longTime = -1;
        if (timeString != null) {
            SimpleDateFormat utcSdf2 = new SimpleDateFormat("MMddyyyyHH");
            utcSdf2.setTimeZone(TimeZone.getTimeZone("UTC"));
            try {
                shiftedDate = utcSdf2.parse(timeString);
                longTime = shiftedDate.getTime();
            } catch (ParseException e) {
                e.printStackTrace();
            }

        }

        return longTime;
    }

    private String getTimeStringFromFileName(String fileName) {
        return fileName.substring(4, 14);
    }

    private class FileNameComparator implements Comparator<String> {

        boolean useAscendingOrder = true;

        public FileNameComparator(boolean useAscendingOrder) {
            this.useAscendingOrder = useAscendingOrder;
        }

        public int compare(String o1, String o2) {
            String fileName1 = (String) o1;
            String fileName2 = (String) o2;

            int returnValue = 0;

            String timeString1 = getTimeStringFromFileName(fileName1);
            String timeString2 = getTimeStringFromFileName(fileName2);

            long time1 = getLongTimeFromString(timeString1);
            long time2 = getLongTimeFromString(timeString2);

            if (time1 < time2) {
                returnValue = -1;
            } else if (time1 > time2) {
                returnValue = 1;
            } else // (time1 == time2)
            {
                returnValue = 0;
            }

            if (!useAscendingOrder) {
                // flip it
                returnValue *= -1;
            }

            return returnValue;
        }

    }

    public List<String> getListOfAvailableXmrgFiles() {
        File fileDirectory = getXmrgFileDirectory();

        String[] fileNameArray = fileDirectory.list();

        int precipDay = DailyQcUtils.pcpn_day;

        List<String> filteredFileNameList = filterFileNames(precipDay,
                fileNameArray);
        sortFileNames(filteredFileNameList);

        return filteredFileNameList;
    }

    public static long getEndTime() {
        Date currentDate = MPEDisplayManager.getCurrent()
                .getCurrentDisplayedDate();
        long currentTime = currentDate.getTime();

        long endTime = (currentTime / TimeUtil.MILLIS_PER_DAY);
        // set endTime to the beginning of the day
        endTime *= TimeUtil.MILLIS_PER_DAY;

        final long millisIn12Hours = (12 * TimeUtil.MILLIS_PER_HOUR);

        if (currentTime >= endTime + millisIn12Hours) {
            /*
             * if it is past 12Z today, then set endTime to 12Z today
             */
            endTime += millisIn12Hours;
        } else {
            // set the time to 12Z the day before
            endTime -= millisIn12Hours;
        }

        return endTime;
    }

    public static double[][] get24HourTotalPrecip(int height, int width,
            double scaleFactor) {
        Date endDate = PostAnalysisManager.getSelectedDate();
        double missingValue = -999.0;

        PrecipTotaller totaller = new PrecipTotaller();
        // totalPrecipGrid units are inches
        double[][] totalPrecipGrid = totaller.get24HourBestEstimateGrid(height,
                width, endDate, missingValue, scaleFactor);

        return totalPrecipGrid;
    }

    private List<String> filterFileNames(int precipDay, String[] fileNameArray) {
        List<String> filteredFileNameList = new ArrayList<String>();

        long endTime = getEndTime();
        long startTime = endTime - TimeUtil.MILLIS_PER_DAY;

        for (int i = 0; i < fileNameArray.length; i++) {
            long fileTime = getTimeFromFileName(fileNameArray[i]);
            if ((fileTime > startTime) && (fileTime <= endTime)) {
                filteredFileNameList.add(fileNameArray[i]);
            }
        }

        return filteredFileNameList;
    }

    /**
     * Retrieves and returns the location of the xmrg file directory as a
     * {@link File} from Apps Defaults.
     * 
     * @return the xmrg file directory as a {@link File}
     */
    public File getXmrgFileDirectory() {
        String header = "PostAnalysisManager.getXmrgFileDirectory(): ";

        String directoryTokenName = "rfcwide_xmrg_dir";
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        String fileDirectoryName = appsDefaults.getToken(directoryTokenName);
        handler.debug(header + "qpe fileDirectoryName = " + fileDirectoryName);

        return new File(fileDirectoryName);
    }

    /**
     * Retrieves and returns the location of the post analysis file directory as
     * a {@link File} from Apps Defaults.
     * 
     * @return the post analysis file directory as a {@link File}
     */
    public File getPostAnalysisFileDirectory() {
        String header = "PostAnalysisManager.getPostAnalysisFileDirectory(): ";

        String directoryTokenName = "mpe_post_output";
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        String fileDirectoryName = appsDefaults
                .getToken(directoryTokenName,
                        "/awips2/edex/data/share/hydroapps/precip_proc/local/data/mpe/post_analysis");
        handler.debug(header + "PA fileDirectoryName = " + fileDirectoryName);
        if (fileDirectoryName != null) {
            return new File(fileDirectoryName);
        } else {
            return null;
        }
    }

    /**
     * Calculate the weight given to either the gauge only or the summed hourly
     * MPE fields. This weighting factor is computed based on the distance
     * between the current [i][j] HRAP bin and the nearest precip station. This
     * method is called from mergeData during the merge process.
     */
    public double computeObservedWeight(int i, int j, double logRHat,
            List<Station> precipStationList, int numPrecipStations,
            float estimatedScale) {

        double distanceSquared;
        double nearestDistanceSquared = 9999999.0;

        int XOR = DailyQcUtils.getHrap_grid().hrap_minx;
        int YOR = DailyQcUtils.getHrap_grid().hrap_miny;

        // Find the distance to the nearest precip station from this HRAP
        // bin at [i][j]

        for (int k = 0; k < numPrecipStations; ++k) {
            Station precipStation = precipStationList.get(k);
            /*
             * station's hrap coordinates MUST be relative to the LL corner of
             * the grid
             */
            float xDistance = precipStation.hrap_x - XOR - i;
            float yDistance = precipStation.hrap_y - YOR - j;

            distanceSquared = xDistance * xDistance + yDistance * yDistance;

            if (distanceSquared < nearestDistanceSquared) {
                nearestDistanceSquared = distanceSquared;
            }

        } // for k

        double nearestDistance = Math.sqrt(nearestDistanceSquared);

        return Math.exp((double) estimatedScale * logRHat * nearestDistance);

    } // computeObservedWeight()

    /**
     * Creates a 3-dimensional array of doubles based on the {@link Hrap_Grid}.
     * 
     * @return the constructed 3-dimensional array of doubles
     */
    public double[][][] create3DGridArray() {
        String header = "PostAnalysisManager.create3DGridArray(): ";

        Hrap_Grid hrap_grid = DailyQcUtils.getHrap_grid();
        int maxI = hrap_grid.maxi;
        int maxJ = hrap_grid.maxj;

        double[][][] array = new double[DailyQcUtils.MAX_GAGEQC_DAYS][maxI][maxJ];
        printDimensions(header, array);

        return array;
    }

    /**
     * Creates a 2-dimensional array of doubles based on the {@link Hrap_Grid}.
     * 
     * @return the constructed 2-dimensional array of doubles
     */
    public double[][] create2DGridArray() {
        Hrap_Grid hrap_grid = DailyQcUtils.getHrap_grid();
        int maxCols = hrap_grid.maxi;
        int maxRows = hrap_grid.maxj;

        double[][] array = new double[maxCols][maxRows];

        return array;
    }

    /*
     * TODO: why do all of these print methods exist? Were they just for debug
     * purposes as the code was being written? Any further action is outside the
     * scope of the present DR (DR 4619).
     */

    private void printDimensions(String text, double[][][] threeDArray) {
        String header = "PostAnalysisManager.printDimensions(): ";

        int dim1 = threeDArray.length;
        int dim2 = threeDArray[0].length;
        int dim3 = threeDArray[0][0].length;

        String message = String.format(text
                + "array of dimensions [%d][%d][%d]", dim1, dim2, dim3);
        handler.debug(header + message);
    }

    public double[][] createGridArray(String dataFilePath,
            boolean flipYOrientation, boolean flipXOrientation,
            boolean exchangeXY) {

        double[][] precipArray = create2DGridArray();

        // in hundredths of inches
        precipArray = readAsciiXmrgGridData(dataFilePath, flipYOrientation,
                flipXOrientation, exchangeXY);

        return precipArray;
    }

    public void printShortArray(String text, short[] shortArray) {
        int count = shortArray.length;
        short value = 0;

        for (int i = 0; i < count; i++) {
            value = shortArray[i];

            if (value > 0) {
                handler.debug(text + "i = " + i + " value = " + value);
            }

        }

        return;
    }

    public void printFloatArray(String text, float[] floatArray) {
        int count = floatArray.length;
        float value = 0.0f;

        for (int i = 0; i < count; i++) {
            value = floatArray[i];

            if (value > 0.0f) {
                handler.debug(text + "i = " + i + " value = " + value);
            }

        }

        return;
    }

    public void printShort2DArray(short[][] shortArray) {
        int rowCount = shortArray.length;

        if (rowCount < 1) {
            handler.debug("printShort2DArray(): rowCount < 1 ");
            return;
        }

        int colCount = shortArray[0].length;

        for (int row = 0; row < rowCount; row++) {
            for (int col = 0; col < colCount; col++) {
                if (shortArray[row][col] > 0) {
                    handler.debug("shortArray[" + row + "][" + col + "] = "
                            + shortArray[row][col]);
                }
            }
        }

    }

    public void printDouble2DArray(double[][] doubleArray) {
        int rowCount = doubleArray.length;
        int colCount = doubleArray[0].length;

        for (int row = 0; row < rowCount; row++) {
            for (int col = 0; col < colCount; col++) {
                String valueString = String.format("%-5.2f",
                        doubleArray[row][col]);
                handler.debug(valueString + " ");
            }
        }
    }

    public void printDouble2DArrayShape(double[][] doubleArray) {
        int rowCount = doubleArray.length;
        int colCount = doubleArray[0].length;

        char blank = ' ';
        char star = '*';
        char c = blank;

        for (int row = 0; row < rowCount; row++) {
            for (int col = 0; col < colCount; col++) {
                double value = doubleArray[row][col];
                if (value > 0) {
                    c = star;
                } else {
                    c = blank;
                }

                handler.debug(String.valueOf(c));
            }
        }
    }

    public void printDouble2DArrayIfGreaterThanZero(double[][] doubleArray) {
        int rowCount = doubleArray.length;
        int colCount = doubleArray[0].length;

        for (int row = 0; row < rowCount; row++) {
            for (int col = 0; col < colCount; col++) {

                if (doubleArray[row][col] > 0) {
                    handler.debug("doubleArray[" + row + "][" + col + "] = "
                            + doubleArray[row][col]);
                }
            }
        }

    }

    /**
     * Converts a 2-dimensional array of shorts to a 2-dimensional array of
     * doubles.
     * 
     * @param precipArray
     *            the 2-dimesional array of shorts to convert
     * @param flipYOrientation
     *            boolean flag indicating if the Y orientation of the data
     *            should be flipped
     * @param exchangeXY
     *            boolean flag indicating whether the X and Y dimensions should
     *            swap sizes
     * @return the 2-dimensional array of doubles that is produced
     */
    public double[][] convertTo2DDoubleArray(short[][] precipArray,
            boolean flipYOrientation, boolean exchangeXY) {
        int rowCount = precipArray.length;
        int colCount = precipArray[0].length;

        double[][] doubleArray = null;

        if (exchangeXY) {
            // the X and Y dimensions swap sizes
            doubleArray = new double[colCount][rowCount];

            if (flipYOrientation) {

                /*
                 * xrmg reads from south to north
                 */
                for (int row = 0; row < rowCount; row++) {
                    int targetRow = (rowCount - 1) - row;
                    for (int col = 0; col < colCount; col++) {
                        doubleArray[col][targetRow] = (double) precipArray[row][col];
                    }
                }
            } else {
                /*
                 * xrmg reads from south to north
                 */
                for (int row = 0; row < rowCount; row++) {
                    for (int col = 0; col < colCount; col++) {
                        doubleArray[col][row] = (double) precipArray[row][col];
                    }
                }
            }

        } else {
            // leave XY dimensions the same
            doubleArray = new double[rowCount][colCount];

            if (flipYOrientation) {
                /*
                 * xrmg reads from south to north
                 */
                for (int row = 0; row < rowCount; row++) {
                    int targetRow = (rowCount - 1) - row;
                    for (int col = 0; col < colCount; col++) {

                        doubleArray[targetRow][col] = (double) precipArray[row][col];
                    }
                }
            } else {
                /*
                 * xrmg reads from south to north
                 */
                for (int row = 0; row < rowCount; row++) {
                    for (int col = 0; col < colCount; col++) {
                        doubleArray[row][col] = (double) precipArray[row][col];
                    }
                }
            }
        }
        return doubleArray;
    }

    public double[][] convertTo2DDoubleArray(int[][] precipArray,
            double scaleFactor, boolean flipYOrientation,
            boolean flipXOrientation, boolean exchangeXY) {
        int rowCount = precipArray.length;
        int colCount = precipArray[0].length;

        double[][] doubleArray = null;

        if (exchangeXY) // the X and Y dimensions swap sizes
        {
            doubleArray = new double[colCount][rowCount];

            if ((flipYOrientation) && (!flipXOrientation)) {

                for (int row = 0; row < rowCount; row++) // xrmg reads from
                                                         // south to north
                {
                    int targetRow = (rowCount - 1) - row;
                    for (int col = 0; col < colCount; col++) {
                        doubleArray[col][targetRow] = (double) precipArray[row][col]
                                * scaleFactor;
                    }
                }
            } else if ((flipXOrientation) && (!flipYOrientation)) {

                for (int row = 0; row < rowCount; row++) // xrmg reads from
                                                         // south to north
                {
                    for (int col = 0; col < colCount; col++) {
                        int targetCol = (colCount - 1) - col;
                        doubleArray[targetCol][row] = (double) precipArray[row][col]
                                * scaleFactor;
                    }
                }
            } else if ((flipXOrientation) && (flipYOrientation)) {

                for (int row = 0; row < rowCount; row++) // xrmg reads from
                                                         // south to north
                {
                    int targetRow = (rowCount - 1) - row;
                    for (int col = 0; col < colCount; col++) {
                        int targetCol = (colCount - 1) - col;
                        doubleArray[targetCol][targetRow] = (double) precipArray[row][col]
                                * scaleFactor;
                    }
                }
            } else {

                for (int row = 0; row < rowCount; row++) // xrmg reads from
                                                         // south to north
                {
                    for (int col = 0; col < colCount; col++) {
                        doubleArray[col][row] = (double) precipArray[row][col]
                                * scaleFactor;
                    }
                }
            }

        }

        else // leave XY dimensions the same
        {

            doubleArray = new double[rowCount][colCount];

            if ((flipYOrientation) && (!flipXOrientation)) {

                for (int row = 0; row < rowCount; row++) // xrmg reads from
                                                         // south to north
                {
                    int targetRow = (rowCount - 1) - row;
                    for (int col = 0; col < colCount; col++) {
                        doubleArray[targetRow][col] = (double) precipArray[row][col]
                                * scaleFactor;
                    }
                }
            } else if ((flipXOrientation) && (!flipYOrientation)) {

                for (int row = 0; row < rowCount; row++) // xrmg reads from
                                                         // south to north
                {

                    for (int col = 0; col < colCount; col++) {
                        int targetCol = (colCount - 1) - col;
                        doubleArray[row][targetCol] = (double) precipArray[row][col]
                                * scaleFactor;
                    }
                }
            } else if ((flipXOrientation) && (flipYOrientation)) {

                for (int row = 0; row < rowCount; row++) // xrmg reads from
                                                         // south to north
                {
                    int targetRow = (rowCount - 1) - row;
                    for (int col = 0; col < colCount; col++) {
                        int targetCol = (colCount - 1) - col;
                        doubleArray[targetRow][targetCol] = (double) precipArray[row][col]
                                * scaleFactor;
                    }
                }
            } else {

                for (int row = 0; row < rowCount; row++) // xrmg reads from
                                                         // south to north
                {
                    for (int col = 0; col < colCount; col++) {
                        doubleArray[row][col] = (double) precipArray[row][col]
                                * scaleFactor;
                    }
                }
            }
        }
        return doubleArray;
    }

    /**
     * Converts an array of shorts to an array of floats
     * 
     * @param shortArray
     *            the array of shorts to convert
     * @return the array of floats that is produced
     */
    public float[] convertToFloatArray(short[] shortArray) {
        int newArraySize = shortArray.length;
        float floatArray[] = new float[newArraySize];

        for (int i = 0; i < newArraySize; i++) {
            floatArray[i] = (float) shortArray[i];

        }

        return floatArray;
    }

    /**
     * Converts an array of floats to an array of shorts
     * 
     * @param floatArray
     *            the array of floats to convert
     * @param conversionFactor
     *            a constant factor to multiple every element in the array of
     *            floats by during the conversion process
     * @return the array of shorts that is produced
     */
    public short[] convertToShortArray(float[] floatArray,
            float conversionFactor) {
        int newArraySize = floatArray.length;
        short shortArray[] = new short[newArraySize];

        for (int i = 0; i < newArraySize; i++) {
            shortArray[i] = (short) (floatArray[i] * conversionFactor);
        }

        return shortArray;
    }

    public float[] convertToSingleArray(double[][] precipArray,
            boolean needsToBeFlipped, boolean needsXYReversal) {
        int rowCount = precipArray.length;
        int colCount = precipArray[0].length;

        int newArraySize = rowCount * colCount;

        float singleArray[] = new float[newArraySize];

        int i = 0;

        if (needsXYReversal) {

            if (needsToBeFlipped) {
                for (int col = colCount - 1; col >= 0; col--) {
                    for (int row = rowCount - 1; row >= 0; row--) // xrmg reads
                                                                  // from south
                                                                  // to north
                    {
                        singleArray[i] = (float) precipArray[row][col];
                        i++;
                    }
                }

            } else {
                for (int col = 0; col < colCount; col++) {
                    for (int row = 0; row < rowCount; row++) // xrmg reads from
                                                             // south to north
                    {
                        singleArray[i] = (float) precipArray[row][col];
                        i++;
                    }
                }
            }

        } else // does not need XY reversal
        {

            if (needsToBeFlipped) {
                for (int row = rowCount - 1; row >= 0; row--) // xrmg reads from
                                                              // south to north
                {
                    for (int col = 0; col < colCount; col++) {
                        singleArray[i] = (float) precipArray[row][col];
                        i++;
                    }
                }
            } else {
                for (int row = 0; row < rowCount; row++) // xrmg reads from
                                                         // south to north
                {
                    for (int col = 0; col < colCount; col++) {
                        singleArray[i] = (float) precipArray[row][col];
                        i++;
                    }
                }
            }

        }

        return singleArray;
    }

    public double[][] readGridData(String filePath, boolean flipYOrientation,
            boolean exchangeXY) {
        double[][] doubleArray = null;

        XmrgFile file1 = new XmrgFile(filePath);

        try {
            long fileLength = file1.getFile().length();

            if (fileLength > 0) {
                file1.load();
                java.awt.Rectangle extent = file1.getHrapExtent();
                setExtent(extent);

                short[][] shortArray = file1.getData(extent);

                xmrgHeader = file1.getHeader();

                doubleArray = convertTo2DDoubleArray(shortArray,
                        flipYOrientation, exchangeXY);

            } else {
                System.out.println("Zero length file for "
                        + file1.getFile().getPath());
            }
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
            System.out.println("Issue with file1 loading for "
                    + file1.getFile().getPath());
        }

        return doubleArray;

    }

    public double[][] readAsciiXmrgGridData(String filePath,
            boolean flipYOrientation, boolean flipXOrientation,
            boolean exchangeXY) {
        AsciiXmrgReader reader = new AsciiXmrgReader(filePath);

        reader.read();

        java.awt.Rectangle extent = reader.getHrapExtent();
        setExtent(extent);

        int[][] intArray = reader.getDataAsGrid();

        double scaleFactor = 1.0; // units as read from the file are hundredths
                                  // of inches

        double[][] doubleArray = convertTo2DDoubleArray(intArray, scaleFactor,
                flipYOrientation, flipXOrientation, exchangeXY);

        return doubleArray;
    }

    private boolean[][] getMisBin() throws Exception {
        // true means that there IS radar coverage at a particular HRAP grid bin
        // NOTE: this "misbin" concept is not the same as the MPE concept that
        // checks
        // RadClim, radar climatology to determine if there are sufficient radar
        // returns for specific bins under the coverage umbrella
        // In this PostAnalysis version of the misbin concept,
        // EVERY bin under the theoretical coverage umbrella is considered to be
        // covered
        Hrap_Grid grid = DailyQcUtils.getHrap_grid();

        int maxJ = grid.maxj;
        int maxI = grid.maxi;
        int XOR = grid.hrap_minx;
        int YOR = grid.hrap_miny;

        final int maxDistanceSquared = 66 * 66; // (66 is approximately half the
                                                // distance of the 131 * 131
                                                // radar grid)

        boolean[][] misbinArray = new boolean[maxJ][maxI];

        Point[] radarLocPointArray = getRadarLocPointArray();
        boolean haveRadarLoc = false;

        int radarCount = radarLocPointArray.length;

        if (radarCount > 0) {
            haveRadarLoc = true;
        }

        if (haveRadarLoc) {
            for (int j = 0; j < maxJ; j++) {
                for (int i = 0; i < maxI; i++) {
                    misbinArray[j][i] = false;

                    for (int r = 0; r < radarCount; r++) {
                        int xDist = i - (radarLocPointArray[r].x - XOR);
                        int yDist = j - (radarLocPointArray[r].y - YOR);
                        int distanceSquared = (xDist * xDist) + (yDist * yDist);
                        if (distanceSquared <= maxDistanceSquared) {
                            misbinArray[j][i] = true;
                            break;
                        }
                    }
                }
            }
        } else // don't have a point array, so set the misbin values all to true
               // (meaning radar is available at all points)
        {
            for (int j = 0; j < maxJ; j++) {
                for (int i = 0; i < maxI; i++) {
                    misbinArray[j][i] = true;
                }
            }

        }
        return misbinArray;
    }

    // ----------------------------------------------------------------------
    public Point[] getRadarLocPointArray() throws Exception {
        Coordinate[] radarCoordinateArray = null;

        radarCoordinateArray = getRadarLatLonCoordinateArray();

        int radarCount = radarCoordinateArray.length;

        Point[] pointArray = new Point[radarCount];

        for (int i = 0; i < radarCount; i++) {
            Coordinate latLonCoord = radarCoordinateArray[i];

            Point hrapPoint = getRadarPointFromLatLon(latLonCoord.y,
                    latLonCoord.x);
            pointArray[i] = hrapPoint;
        }

        return pointArray;
    }

    // ----------------------------------------------------------------------
    public Coordinate[] getRadarLatLonCoordinateArray() throws VizException {
        Coordinate[] radarCoordinateArray = null;

        try {

            String query = "select * from radarloc where use_radar='T' order by radid asc";

            List<Object[]> rs = DirectDbQuery.executeQuery(query,
                    HydroConstants.IHFS, QueryLanguage.SQL);

            double lat = 0;
            double lon = 0;

            radarCoordinateArray = new Coordinate[rs.size()];
            for (int i = 0; i < rs.size(); i++) {
                Object[] rowAsObjectArray = rs.get(i);

                try {
                    lat = (Double) rowAsObjectArray[5];
                    lon = -1 * (Double) rowAsObjectArray[6];
                } catch (Exception e) {
                    e.printStackTrace();
                    lat = 0.0;
                    lon = 0.0;
                }

                radarCoordinateArray[i] = new Coordinate(lon, lat);
            }
        } catch (Exception e) {
            e.printStackTrace();
            radarCoordinateArray = new Coordinate[0];
        }
        return radarCoordinateArray;

    }

    // ----------------------------------------------------------------------
    private Point getRadarPointFromLatLon(double lat, double lon)
            throws HrapConversionException {
        Coordinate hrapCoordinate = HrapUtil.latLonToHrap(new Coordinate(lon,
                lat));

        Point hrapPoint = new Point((int) hrapCoordinate.x,
                (int) hrapCoordinate.y);
        return hrapPoint;
    }

    // ----------------------------------------------------------------------
    /**
     * Initializes a {@link NamedColorUseSet} based on the specified name. The
     * name is used to retrieve a {@link List} of {@link Colorvalue}s from
     * {@link GetColorValues}.
     * 
     * @param colorUseName
     *            the specified name
     * @return the {@link NamedColorUseSet} that was created.
     */
    public static NamedColorUseSet createNamedColorUseSet(String colorUseName) {
        String user_id = System.getProperty("user.name");

        int duration = TimeUtil.SECONDS_PER_DAY; // 24 hours

        List<Colorvalue> colorList = GetColorValues.get_colorvalues(user_id,
                HydroDisplayManager.MPE_APPLICATION_NAME, colorUseName,
                duration, "E", MPEColors.build_mpe_colors());

        int listSize = colorList.size() - 2;

        double[] thresholdValues = new double[listSize];
        String[] colorNameArray = new String[listSize];

        int index = 0;

        double thresholdValue = 0.0;
        String colorNameValue = null;

        String missingColorName = "GREY50";
        String lowValueColorName = "BLACK";

        for (Colorvalue colorValue : colorList) {
            thresholdValue = colorValue.getId().getThresholdValue();
            colorNameValue = colorValue.getColorname().getColorName();

            if (thresholdValue == MISSING_VALUE) {
                missingColorName = colorNameValue;
            } else if (thresholdValue == -8888.0) {
                lowValueColorName = colorNameValue;

            } else // regular value
            {
                // -8888.0
                colorNameArray[index] = colorNameValue;
                thresholdValues[index] = thresholdValue;
                index++;
                if (index >= listSize) {
                    break;
                }
            }
        }

        NamedColorUseSet namedColorUseSet1 = new NamedColorUseSet(colorUseName,
                colorUseName, thresholdValues, colorNameArray,
                missingColorName, lowValueColorName, duration);
        return namedColorUseSet1;
    }

    /**
     * Lookups and returns the {@link NamedColorUseSet} associated with the
     * specified name. If a NamedColorUseSet cannot be found in the lookup
     * {@link Map}, one will be created.
     * 
     * @param colorUseName
     *            the specified name
     * @return the {@link NamedColorUseSet} that was found or created
     */
    public static NamedColorUseSet getNamedColorUseSet(String colorUseName) {
        NamedColorUseSet namedColorUseSet = colorUseSetMap.get(colorUseName);
        if (namedColorUseSet == null) {
            namedColorUseSet = createNamedColorUseSet(colorUseName);
            colorUseSetMap.put(colorUseName, namedColorUseSet);
        }

        return namedColorUseSet;

    }

    /*
     * This method puts data in the mergeArray and the ratioArray. The
     * mergeArray gets displayed as the Merged Field on Screen 2 of PostAnalysis
     * (left-hand side). To create this field, the sum of 24 one-hour best
     * estimate QPE fields are "merged" with 24 hour gage-only fields rendered
     * by DailyQC.
     * 
     * The ratioArray is called the Grid Bias Field and is also displayed on
     * Screen 2 of PostAnalysis on the right-hand side.
     * 
     * This method could call computeObservedWeight to retrieve a weighting
     * factor calculated based on the distance between the current [i][j] HRAP
     * bin and the nearest precip station.
     */
    public void mergeData(
            double accum24HourPrecip[][], // reverse order MAXY, MAXX
            double gageGrid[][], double mergeArray[][], double ratioArray[][],
            double disaggArray[][], float estimatedScale, float rhat)
            throws Exception {
        if (misbin == null) {
            misbin = getMisBin();
        }

        String header = "PostAnalysisManager.mergeData(): ";
        int MAXX = DailyQcUtils.getHrap_grid().maxi;
        int MAXY = DailyQcUtils.getHrap_grid().maxj;

        double logRHat;
        int i, j;
        double gageOnlyGridValueInInches, qpeInches;
        double weightingFactor;
        int precipStationCount;

        List<Station> precipStationList = DailyQcUtils.precip_stations;

        precipStationCount = precipStationList.size();

        logRHat = Math.log((double) rhat);

        handler.debug(header + "rhat = " + rhat + "\n");
        handler.debug(header + "logRHat = " + logRHat + "\n");

        /*
         * units of gageOnlyGrid are hundredths of inches - read directly from
         * precip grid fields
         */
        /*
         * units of QPEAccum24hr are inches - generated in routine
         * generate_accum_grids
         */
        /* units of mergeArray are MM.divide(100) hundredths of millimeters */

        double conversionFactor = MM_PER_INCH * 100;

        for (i = 0; i < MAXX; i++) {
            for (j = 0; j < MAXY; j++) {
                /*
                 * For whatever reason, the QPEAccum24hr array is declared as
                 * [day][MAXY][MAXX], so the [j][i] notation below is correct
                 * even though it looks like a mistake. See the malloc call in
                 * generate_accum_grids.c
                 */

                qpeInches = accum24HourPrecip[j][i];

                gageOnlyGridValueInInches = gageGrid[i][j] / 100.0;

                /* Put a value in the mergeArray */

                if (gageOnlyGridValueInInches < 0.0000001
                        && qpeInches < 0.0000001) // if both are 0
                {
                    mergeArray[i][j] = 0.0; // don't convert, since already
                                            // 0.0
                }
                /*
                 * misbin is a 2-D boolean array also used with x,y (i,j) in
                 * reversed order, so the [j][i] below is correct.
                 */
                else if (misbin[j][i] == false) {
                    mergeArray[i][j] = gageOnlyGridValueInInches
                            * conversionFactor;
                } else if (precipStationCount == 0) {
                    mergeArray[i][j] = qpeInches * conversionFactor;
                } else {
                    weightingFactor = computeObservedWeight(i, j, logRHat,
                            precipStationList, precipStationCount,
                            estimatedScale);

                    double mergedValueInInches = weightingFactor
                            * gageOnlyGridValueInInches
                            + (1.0 - weightingFactor) * qpeInches;

                    mergeArray[i][j] = mergedValueInInches * conversionFactor;
                }

                /*
                 * Put a value in the ratio Array. This array eventually becomes
                 * the Grid Bias Field in the PostAnalysis utility.
                 */

                if (qpeInches <= 0.0) {
                    ratioArray[i][j] = MISSING_VALUE;
                    disaggArray[i][j] = mergeArray[i][j];
                }

                else // qpeInches > 0.0
                {
                    double qpeHundredthsofMM = qpeInches * conversionFactor;

                    ratioArray[i][j] = mergeArray[i][j] / qpeHundredthsofMM;
                    disaggArray[i][j] = MISSING_VALUE;
                }

            } /* for j */
        } /* for i */
    } /* mergeData() */

    public void setExtent(java.awt.Rectangle extent) {
        this.extent = extent;
    }

    public java.awt.Rectangle getExtent() {
        return extent;
    }

    public MergeParameters loadMergeParameters() throws VizException {

        String query = "select * from S3PostAnalParams limit 1";
        MergeParameters parameters = null;

        List<Object[]> rs = DirectDbQuery.executeQuery(query,
                HydroConstants.IHFS, QueryLanguage.SQL);

        float rhat = 0.0f;
        float estimatedScale = 0.0f;

        Object[] rowAsObjectArray = rs.get(0);
        estimatedScale = ((Number) rowAsObjectArray[3]).floatValue();
        rhat = ((Number) rowAsObjectArray[4]).floatValue();

        parameters = new MergeParameters();
        parameters.setEstimatedScale(estimatedScale);
        parameters.setRhat(rhat);

        return parameters;

    }

    public float getRhat() throws Exception {
        MergeParameters parameters = getMergeParameters();
        handler.debug("PostAnalysisManager.getRhat():  = "
                + parameters.getRhat());

        return parameters.getRhat();
    }

    public float getEstimatedScale() throws Exception {
        MergeParameters parameters = getMergeParameters();
        return parameters.getEstimatedScale();
    }

    private MergeParameters getMergeParameters() throws Exception {
        String header = "PostAnalysisManager.getMergeParameters(): ";

        if (mergeParameters == null) {
            mergeParameters = loadMergeParameters();

            if (mergeParameters == null) {
                // if not in the database, then use hardcoded defaults
                mergeParameters = new MergeParameters();
                mergeParameters.setEstimatedScale(DEFAULT_ESTIMATED_SCALE);
                mergeParameters.setRhat(DEFAULT_RHAT);
            }
        }

        handler.debug(header + mergeParameters);

        return mergeParameters;
    }

    public void setXmrgHeader(XmrgFile.XmrgHeader xmrgHeader) {
        this.xmrgHeader = xmrgHeader;
    }

    public XmrgFile.XmrgHeader getXmrgHeader() {
        return xmrgHeader;
    }

    private class MergeParameters {

        private float rhat;

        private float estimatedScale;

        private void setRhat(float rhat) {
            this.rhat = rhat;
        }

        private float getRhat() {
            return rhat;
        }

        private void setEstimatedScale(float estimatedScale) {
            this.estimatedScale = estimatedScale;
        }

        private float getEstimatedScale() {
            return estimatedScale;
        }

        public String toString() {
            return "estimated scale = " + estimatedScale + " rhat = " + rhat;
        }
    }

} // end PostAnalysisManager

