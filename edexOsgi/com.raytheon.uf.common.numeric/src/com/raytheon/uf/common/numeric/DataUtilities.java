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
package com.raytheon.uf.common.numeric;

import com.raytheon.uf.common.numeric.dest.DataDestination;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.numeric.source.OffsetDataSource;

/**
 * Provide utility method for working with a {@link DataSource} or
 * {@link DataDestination}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 27, 2014  2791     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class DataUtilities {

    /**
     * Copy all data within the specified range from a source to a destination.
     * For copying of different ranges {@link OffsetDataSource} can be used to
     * shift the source indexes.
     * 
     * @param source
     *            Any data source
     * @param destination
     *            any data destination
     * @param nx
     *            the width of the data to copy
     * @param ny
     *            the hieght of the data to copy.
     * 
     * @return the same destination that was provided.
     */
    public static final <D extends DataDestination> D copy(DataSource source,
            D destination, int nx, int ny) {
        for (int i = 0; i < nx; i += 1) {
            for (int j = 0; j < ny; j += 1) {
                destination.setDataValue(source.getDataValue(i, j), i, j);
            }
        }
        return destination;
    }

    /**
     * Determine the extrama of a source.
     * 
     * @param source
     *            Any data source
     * @param nx
     *            the width of the data to copy
     * @param ny
     *            the hieght of the data to copy.
     * @return The min/max values for the source.
     */
    public static final MinMax getMinMax(DataSource source, int nx, int ny) {
        double minValue = Double.POSITIVE_INFINITY;
        double maxValue = Double.NEGATIVE_INFINITY;
        for (int i = 0; i < nx; i += 1) {
            for (int j = 0; j < ny; j += 1) {
                double val = source.getDataValue(i, j);
                if (Double.isNaN(val)) {
                    continue;
                }
                if (val < minValue) {
                    minValue = val;
                }
                if (val > maxValue) {
                    maxValue = val;
                }
            }
        }
        return new MinMax(minValue, maxValue);
    }

    /**
     * Class for holding Min/Max data values.
     */
    public static class MinMax {

        private final double min;

        private final double max;

        private MinMax(double min, double max) {
            this.min = min;
            this.max = max;
        }

        public double getMin() {
            return min;
        }

        public double getMax() {
            return max;
        }

        public double getSpan() {
            return max - min;
        }

    }

}
