package gov.noaa.gsd.viz.ensemble.display.chart;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * 
 * A utility class that provides some common statistical methods.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 15, 2016   12301         jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */
public abstract class StatisticsUtilities {

    /**
     * Returns the mean of an array of numbers. This is equivalent to calling
     * calculateMean(values, true).
     * 
     * @param values
     *            -The values.
     * 
     * @return The mean.
     */
    public static double calculateMean(Number[] values) {
        return calculateMean(values, true);
    }

    /**
     * Returns the mean of an array of numbers.
     * 
     * @param values
     *            -the values.
     * @param includeNullAndNaN
     *            -a flag that controls whether or not null and Double.NaN
     *            values are included in the calculation (if either is present
     *            in the array, the result is {@link Double#NaN}).
     * 
     * @return The mean.
     * 
     */
    public static double calculateMean(Number[] values,
            boolean includeNullAndNaN) {

        if (values == null) {
            return Double.NaN;

        }

        double sum = 0;
        double current;
        int counter = 0;
        for (int i = 0; i < values.length; i++) {
            /* treat nulls the same as NaNs */
            if (values[i] != null) {
                current = values[i].doubleValue();
            } else {
                current = Double.NaN;
            }
            /* Calculate the sum and count */
            if (includeNullAndNaN || !Double.isNaN(current)) {
                sum = sum + current;
                counter++;
            }
        }
        double result = (sum / counter);
        return result;
    }

    /**
     * Calculates the mean.
     * 
     * @param values
     *            - values for the calculation
     * @return The mean
     */
    public static double calculateMedian(double[] values) {
        List<Double> list = new ArrayList<Double>();
        for (double d : values) {
            list.add(d);
        }
        return (double) calculateMedian(list);
    }

    /**
     * Calculates the median for a list of values. The list of values will be
     * copied, and the copy sorted, before calculating the median. To avoid this
     * step (if your list of values is already sorted).
     * 
     * @param values
     *            -The values (<code>null permitted).
     * 
     * @return The median.
     */
    public static double calculateMedian(List<Double> values) {
        return calculateMedian(values, true);
    }

    /**
     * Calculates the median for a list of values. If copyAndSort is false, the
     * list is assumed to be presorted in ascending order by value.
     * 
     * @param values
     *            -The values.
     * @param copyAndSort
     *            -A flag that controls whether the list of values is copied and
     *            sorted.
     * 
     * @return The median.
     */
    public static double calculateMedian(List<Double> values,
            boolean copyAndSort) {

        double result = Double.NaN;
        if (values != null) {
            if (copyAndSort) {
                int itemCount = values.size();
                List<Double> copy = new ArrayList<Double>(itemCount);
                for (int i = 0; i < itemCount; i++) {
                    copy.add(i, values.get(i));
                }
                Collections.sort(copy);
                values = copy;
            }
            int count = values.size();
            if (count > 0) {
                if (count % 2 == 1) {
                    if (count > 1) {
                        Number value = values.get((count - 1) / 2);
                        result = value.doubleValue();
                    } else {
                        Number value = values.get(0);
                        result = value.doubleValue();
                    }
                } else {
                    Number value1 = values.get(count / 2 - 1);
                    Number value2 = values.get(count / 2);
                    result = (double) ((value1.doubleValue() + value2
                            .doubleValue()) / 2.0);
                }
            }
        }
        return result;
    }

    /**
     * Calculates the median for a sublist within a list of values.
     * 
     * @param values
     *            -The values, in any order (null not permitted).
     * @param start
     *            -The start index.
     * @param end
     *            -The end index.
     * 
     * @return The median.
     */
    public static double calculateMedian(List<Double> values, int start, int end) {
        return calculateMedian(values, start, end, true);
    }

    /**
     * Calculates the median for a sublist within a list of values. The entire
     * list will be sorted if the ascending flag (copyAndSort) is false.
     * 
     * @param values
     *            -The values (null not permitted).
     * @param start
     *            -The start index.
     * @param end
     *            -The end index.
     * @param copyAndSort
     *            -A flag that that controls whether the list of values is
     *            copied and sorted.
     * 
     * @return The median.
     */
    public static double calculateMedian(List<Double> values, int start,
            int end, boolean copyAndSort) {

        double result = Double.NaN;
        if (copyAndSort) {
            List<Double> working = new ArrayList<Double>(end - start + 1);
            for (int i = start; i <= end; i++) {
                working.add(values.get(i));
            }
            Collections.sort(working);
            result = calculateMedian(working, false);
        } else {
            int count = end - start + 1;
            if (count > 0) {
                if (count % 2 == 1) {
                    if (count > 1) {
                        Number value = values.get(start + (count - 1) / 2);
                        result = value.doubleValue();
                    } else {
                        Number value = (Number) values.get(start);
                        result = value.doubleValue();
                    }
                } else {
                    Number value1 = values.get(start + count / 2 - 1);
                    Number value2 = values.get(start + count / 2);
                    result = (double) ((value1.doubleValue() + value2
                            .doubleValue()) / 2.0);
                }
            }
        }
        return result;

    }

    /**
     * Returns the standard deviation of a set of numbers.
     * 
     * @param data
     *            -The data .
     * 
     * @return The standard deviation of a set of numbers.
     */
    public static double getStdDev(Number[] data) {
        if (data == null || data.length == 0) {
            return Double.NaN;

        }

        double avg = calculateMean(data);
        double sum = 0;

        for (int counter = 0; counter < data.length; counter++) {
            double diff = data[counter].doubleValue() - avg;
            sum = sum + diff * diff;
        }
        return (double) (Math.sqrt(sum / (data.length - 1)));
    }

    /**
     * Fits a straight line to a set of (x, y) data, returning the slope and
     * intercept.
     * 
     * @param xData
     *            -The x-data.
     * @param yData
     *            -The y-data .
     * 
     * @return A double array with the intercept in [0] and the slope in [1].
     */
    public static double[] getLinearFit(Number[] xData, Number[] yData) {

        if (xData == null || yData == null || xData.length != yData.length) {
            return null;
        }
        double[] result = new double[2];
        /* slope */
        result[1] = getSlope(xData, yData);
        /* intercept */
        result[0] = calculateMean(yData) - result[1] * calculateMean(xData);

        return result;

    }

    /**
     * Finds the slope of a regression line using least squares.
     * 
     * @param xData
     *            -The x-values.
     * @param yData
     *            -The y-values.
     * 
     * @return The slope.
     */
    public static double getSlope(Number[] xData, Number[] yData) {
        if (xData == null || yData == null || xData.length != yData.length) {
            return Double.NaN;
        }

        // ********* stat function for linear slope ********
        // y = a + bx
        // a = ybar - b * xbar
        // sum(x * y) - (sum (x) * sum(y)) / n
        // b = ------------------------------------
        // sum (x^2) - (sum(x)^2 / n
        // *************************************************

        // sum of x, x^2, x * y, y
        double sx = 0, sxx = 0, sxy = 0, sy = 0;
        int counter;
        for (counter = 0; counter < xData.length; counter++) {
            sx = sx + xData[counter].doubleValue();
            sxx = sxx + (double) (Math.pow(xData[counter].doubleValue(), 2));
            sxy = sxy + yData[counter].doubleValue()
                    * xData[counter].doubleValue();
            sy = sy + yData[counter].doubleValue();
        }
        return (sxy - (sx * sy) / counter) / (sxx - (sx * sx) / counter);

    }

    /**
     * Calculates the correlation between two data sets. Both arrays should
     * contain the same number of items. Null values are treated as zero.
     * 
     * 
     * @param data1
     *            -the first data set.
     * @param data2
     *            -The second data set.
     * 
     * @return The correlation.
     */
    public static double getCorrelation(Number[] data1, Number[] data2) {
        if (data1 == null || data2 == null || data1.length != data2.length) {
            return Double.NaN;

        }

        int n = data1.length;
        double sumX = 0;
        double sumY = 0;
        double sumX2 = 0;
        double sumY2 = 0;
        double sumXY = 0;
        for (int i = 0; i < n; i++) {
            double x = 0;
            if (data1[i] != null) {
                x = data1[i].doubleValue();
            }
            double y = 0;
            if (data2[i] != null) {
                y = data2[i].doubleValue();
            }
            sumX = sumX + x;
            sumY = sumY + y;
            sumXY = sumXY + (x * y);
            sumX2 = sumX2 + (x * x);
            sumY2 = sumY2 + (y * y);
        }
        return (double) ((n * sumXY - sumX * sumY) / Math.pow((n * sumX2 - sumX
                * sumX)
                * (n * sumY2 - sumY * sumY), 0.5));
    }

    /**
     * Calculates maximum of a group float values.
     * 
     * @param workValue
     *            -The work array contains the data values
     * @param length
     *            - Length the data in the work array.
     * @return The maximum
     */
    protected float calculateMax(float[] workValue, int length) {

        float max = Float.NaN;
        for (int i = 0; i < length; i++) {
            if (Float.isNaN(max)) {
                max = workValue[i];
                continue;
            }

            if (max < workValue[i]) {
                max = workValue[i];
            }
        }

        return max;
    }

    /**
     * Calculates maximum of a group double values.
     * 
     * @param workValue
     *            -The work array contains the data values
     * @param length
     *            - Length the data in the work array.
     * @return The maximum
     */
    protected static double calculateMax(double[] workValue, int length) {
        double max = Double.NaN;
        for (int i = 0; i < length; i++) {
            if (Double.isNaN(max)) {
                max = workValue[i];
                continue;
            }

            if (max < workValue[i]) {
                max = workValue[i];
            }
        }

        return max;
    }

    /**
     * Calculates minimum of a group float values.
     * 
     * @param workValue
     *            -The work array contains the data values
     * @param length
     *            - Length the data in the work array.
     * @return The minimum
     */
    public float calculateMin(float[] workValue, int length) {

        float min = Float.NaN;
        for (int i = 0; i < length; i++) {
            if (Float.isNaN(min)) {
                min = workValue[i];
                continue;
            }

            if (min > workValue[i]) {
                min = workValue[i];
            }
        }

        return min;
    }

    /**
     * Calculates minimum of a group double values.
     * 
     * @param workValue
     *            -The work array contains the data values
     * @param length
     *            - Length the data in the work array.
     * @return The minimum
     */
    public static double calculateMin(double[] workValue, int length) {

        double min = Double.NaN;
        for (int i = 0; i < length; i++) {
            if (Double.isNaN(min)) {
                min = workValue[i];
                continue;
            }
            if (min > workValue[i])
                min = workValue[i];
        }

        return min;
    }

    /**
     * Calculates range of a group float values.
     * 
     * @param workValue
     *            -The work array contains the data values
     * @param length
     *            - Length the data in the work array.
     * @return The range
     */
    public float calculateRange(float[] workValue, int length) {
        if (workValue == null || length < 1)
            return Float.NaN;

        float range = Float.NaN;

        float min = Float.NaN;
        float max = Float.NaN;
        for (int i = 0; i < length; i++) {

            if (Float.isNaN(workValue[i])) {
                continue;
            }

            if (Float.isNaN(max) && Float.isNaN(min)) {
                max = min = workValue[i];
                continue;
            }

            if (max < workValue[i]) {
                max = workValue[i];
            }

            if (min > workValue[i]) {
                min = workValue[i];
            }

        }
        if (Float.isNaN(max) || Float.isNaN(min)) {
            range = Float.NaN;
        } else {
            range = max - min;
        }

        return range;
    }

    /**
     * Calculates sum of a group float values.
     * 
     * @param workValue
     *            -The work array contains the data values
     * @param length
     *            - Length the data in the work array.
     * @return The sum
     */
    public float calculateSum(float[] workValue, int length) {
        if (workValue == null || length < 1)
            return Float.NaN;

        float sum = 0;

        for (int i = 0; i < length; i++) {
            /* This solution is for same GridGeometry grids */
            if (!Float.isNaN(workValue[i])) {
                sum += workValue[i];
            }

        }

        return sum;
    }

    /**
     * Converts a float list to float array.
     * 
     * @param data
     *            - A float data list
     * @return the float array
     */
    public static float[] floatListToFloatArray(List<Float> data) {

        float[] values = new float[data.size()];
        int i = 0;
        for (Float f : data) {
            values[i++] = (f != null ? f : Float.NaN);
        }
        return values;
    }

    /**
     * Converts a float list to double array.
     * 
     * @param data
     *            - A float data list
     * @return the double array
     */
    public static double[] floatListToDoubleArray(List<Float> data) {

        double[] values = new double[data.size()];
        int i = 0;
        for (Float f : data) {
            values[i++] = (f != null ? f : Double.NaN);
        }
        return values;
    }

    /**
     * Calculates mean.
     * 
     * @param workValue
     *            -The work array contains the data values
     * @param length
     *            - Length the data in the work array.
     * 
     * @return The mean
     */
    protected float calculateMean(float[] workValue, int length) {

        float mean = Float.NaN;
        int count = 0;
        for (int i = 0; i < length; i++) {
            if (Float.isNaN(workValue[i])) {
                continue;
            }

            count++;

            if (Float.isNaN(mean)) {
                mean = workValue[i];

                continue;
            }
            mean += workValue[i];
        }

        if (!Float.isNaN(mean) && count > 0) {
            mean = mean / count;
        }

        return mean;

    }

    /**
     * Calculates mean.
     * 
     * @param workValue
     *            -The work array contains the data values
     * @return The mean
     */
    protected static double calculateMean(double[] workValue) {

        if (workValue == null) {
            return Double.NaN;
        }
        double mean = Double.NaN;
        int count = 0;
        for (int i = 0; i < workValue.length; i++) {
            if (Double.isNaN(workValue[i])) {
                continue;
            }

            count++;

            if (Double.isNaN(mean)) {
                mean = workValue[i];

                continue;
            }
            mean += workValue[i];
        }
        if (!Double.isNaN(mean) && count > 0) {
            mean = mean / count;
        }

        return mean;

    }
}