package com.raytheon.uf.viz.xy;

import java.awt.Rectangle;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.viz.core.graphing.xy.XYData;

/**
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ??					   ??		     Initial creation
 * Oct 2, 2012  DR 15259   M.Porricelli  Interpolate below 850MB                                     
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class InterpUtils {

    /**
     * 
     * @param dataList
     * @param ny
     * @param graph
     * @param lowToHigh
     *            pass in true if graph y axis is labeled low on the bottom to
     *            high on the top
     * @param fillValue
     *            value to fill in undefined values with ( recommend Float.NaN )
     * @return
     */
    public static float[] makeColumn(List<XYData> dataList, int ny,
            IGraph graph, boolean lowToHigh, float fillValue) {

        float[] column = new float[ny];
        if (dataList.size() <= 1) {
            Arrays.fill(column, fillValue);
            return column;
        }
        if (lowToHigh) {
            Collections.sort(dataList, new Comparator<XYData>() {
                @Override
                public int compare(XYData o1, XYData o2) {
                    return Double.compare(((Number) o1.getY()).doubleValue(),
                            ((Number) o2.getY()).doubleValue());
                }

            });
        } else {
            Collections.sort(dataList, new Comparator<XYData>() {
                @Override
                public int compare(XYData o1, XYData o2) {
                    return Double.compare(((Number) o2.getY()).doubleValue(),
                            ((Number) o1.getY()).doubleValue());
                }

            });
        }

        double maxYAxisVal = ((Number) dataList.get(0).getY()).doubleValue();
        double minYAxisVal = ((Number) dataList.get(dataList.size() - 1).getY())
                .doubleValue();
        // Allow interpolation below 850 when this is lowest level
        if (maxYAxisVal == 850.0){
        	maxYAxisVal = 1000.0;
        }

        if (maxYAxisVal < minYAxisVal) {
            double tmp = maxYAxisVal;
            maxYAxisVal = minYAxisVal;
            minYAxisVal = tmp;
        }

        double maxY = graph.getExtent().getMaxY();
        double minY = graph.getExtent().getMinY();
        double yStep = (maxY - minY) / (ny - 1);

        Iterator<XYData> itr = dataList.iterator();
        XYData data = itr.next();
        double x1 = ((Number) (data.getX())).doubleValue();
        double y1 = ((Number) (data.getY())).doubleValue();
        data = itr.next();
        double x2 = ((Number) (data.getX())).doubleValue();
        double y2 = ((Number) (data.getY())).doubleValue();
        for (int i = 0; i < ny; i++) {
            double y = graph.getVirtualLocation(0, maxY - yStep * i)[1];

            while (((lowToHigh && y > y2) || (!lowToHigh && y < y2))
                    && itr.hasNext()) {
                data = itr.next();
                x1 = x2;
                y1 = y2;
                x2 = ((Number) (data.getX())).floatValue();
                y2 = ((Number) (data.getY())).doubleValue();
            }

            // get our error if we are outside the min and max range of y values
            double relativeError = 0.0;
            if (y < minYAxisVal) {
                // calculate how far we are from minXAxisVal
                relativeError = (minYAxisVal - y) / ((minYAxisVal + y) / 2.0);
            } else if (y > maxYAxisVal) {
                // calculate how far we are from the max
                relativeError = (y - maxYAxisVal) / ((maxYAxisVal + y) / 2.0);
            }

            if ((lowToHigh && y2 < y) || (!lowToHigh && y2 > y)) {
                column[i] = fillValue;
            } else if (relativeError > .000001) {
                column[i] = fillValue;
            } else {
                column[i] = (float) (x1 + (y - y1) * (x2 - x1) / (y2 - y1));
            }

        }
        return column;
    }

    public static float[] makeRows(float[][] columns, float[] xVals, int nx,
            IGraph graph, boolean left_to_right, float fillValue) {
        if (columns == null || columns.length < 1) {
            // TODO - return something different?
            return new float[] {};
        }
        float[] result = new float[nx * columns[0].length];
        Arrays.fill(result, fillValue);
        for (int j = 0; j < columns[0].length; j++) {
            double maxX = graph.getExtent().getMaxX();
            double minX = graph.getExtent().getMinX();
            double xStep = (maxX - minX) / (columns[0].length - 1);

            // get x axis min and max, assumes xVals is sorted
            double minXAxisVal = xVals[0];
            double maxXAxisVal = xVals[xVals.length - 1];
            if (minXAxisVal > maxXAxisVal) {
                double tmp = minXAxisVal;
                minXAxisVal = maxXAxisVal;
                maxXAxisVal = tmp;
            }

            double x1, y1, x2, y2;

            int c = 0;
            // find the first non NaN value
            while (c < xVals.length && Float.isNaN(columns[c][j])) {
                c += 1;
            }
            if (c < xVals.length) {
                y1 = columns[c][j];
                x1 = xVals[c];
            } else {
                continue;
            }
            // find the second non NaN value
            c += 1;
            while (c < xVals.length && Float.isNaN(columns[c][j])) {
                c += 1;
            }
            if (c < xVals.length) {
                y2 = columns[c][j];
                x2 = xVals[c];
            } else {
                continue;
            }

            for (int i = 0; i < nx; i++) {
                double x = graph.getVirtualLocation(maxX - xStep * i, 0)[0];
                // only set a value if x is between min and max, assumes array
                // is already filled with invalid values

                // get the error if we are above max or below min, this fixes
                // where the edges of the graph may not get values set
                double relativeError = 0.0;
                if (x < minXAxisVal) {
                    // calculate how far we are from minXAxisVal
                    relativeError = (minXAxisVal - x)
                            / ((minXAxisVal + x) / 2.0);
                } else if (x > maxXAxisVal) {
                    // calculate how far we are from the max
                    relativeError = (x - maxXAxisVal)
                            / ((maxXAxisVal + x) / 2.0);
                }

                // if we are inside .0001% greater than min and .0001% less than
                // max go ahead and set the value
                if (relativeError < 0.000001) {
                    if (((!left_to_right && x > x2) || (left_to_right && x < x2))
                            && c < xVals.length) {
                        c += 1;
                        // Scan through the columns finding the next value that
                        // is not NaN.
                        while (c < xVals.length && Float.isNaN(columns[c][j])) {
                            c += 1;
                        }
                        if (c < xVals.length) {
                            y1 = y2;
                            x1 = x2;
                            y2 = columns[c][j];
                            x2 = xVals[c];
                        }
                    }

                    result[j * nx + nx - 1 - i] = (float) (y1 + (x - x1)
                            * (y2 - y1) / (x2 - x1));
                }
            }
        }
        return result;
    }

    public static float getInterpolatedData(Rectangle area, double x, double y,
            float[] data) {
        // bilinear interpolation
        float val = 0;
        float missing = 1;
        int[] xr = { (int) Math.ceil(x), (int) Math.floor(x) };
        int[] yr = { (int) Math.ceil(y), (int) Math.floor(y) };
        // this occurs when x == (int) x
        if (xr[0] == xr[1]) {
            xr = new int[] { xr[0] };
        }
        // this occurs when y == (int) y
        if (yr[0] == yr[1]) {
            yr = new int[] { yr[0] };
        }
        for (int x1 : xr) {
            for (int y1 : yr) {
                float val11 = -999999;
                if (y1 < area.getMinY() || y1 >= area.getMaxY()) {
                    val11 = -999999;
                } else if (x1 < area.getMinX() || x1 >= area.getMaxX()) {
                    val11 = -999999;
                } else {
                    val11 = data[(int) ((y1 - area.getMinY()) * area.getWidth() + (x1 - area
                            .getMinX()))];
                }
                if (val11 > -9999) {
                    val += (1 - Math.abs(x1 - x)) * (1 - Math.abs(y1 - y))
                            * val11;
                } else {
                    missing -= (1 - Math.abs(x1 - x)) * (1 - Math.abs(y1 - y));
                }
            }
        }
        if (missing > 0.25) {
            return val / missing;
        } else {
            return -999999f;
        }

    }

    public static float[] convertToArray(List<Float> list) {
        float[] array = new float[list.size()];
        for (int i = 0; i < array.length; i++) {
            array[i] = list.get(i);
        }
        return array;
    }
}
