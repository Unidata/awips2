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
package com.raytheon.viz.hydro.timeseries.util;

/**
 * Handles graph scaling. Reused from ScalingHelper OHD.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 11, 2010            mpduff      Initial creation
 * Apr 15, 2011  8963      jpiatt      Adjusted range for y-axis.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ScaleManager {

	// includes the first and last ticks
	// private final static int _minMajorTickCount = 4;
	/**
	 * Max number of major ticks, including first and last
	 */
	private final static int MAX_MAJOR_TICK_ = 6;

	private final static double ZOOMED_FACTOR = 0.01;

	private final static double UNZOOMED_FACTOR = 5.0;

	private double[] niceMajorIncrementArray = { 0.01, 0.02, 0.05, 0.1, 0.2,
			0.5, 1.0, 2.0, 5.0 };

//	private double[] niceMinorIncrementArray = { .2, .5, 1 };

	private double baseFactorStartingPoint = .01;

	private double minDataValue;

	private double maxDataValue;

	private double minScaleValue;

	private double maxScaleValue;

	private int majorTickCount;

	private double majorTickIncrement;

	private boolean zoomFlag;

	public ScaleManager(double minDataValue, double maxDataValue) {
		this.minDataValue = minDataValue;
		this.maxDataValue = maxDataValue;

		rescale();
	}

	private synchronized void rescale() {
		int multipleCount = 0;

		// if zooming, use the ZOOMED_FACTOR value
		if (zoomFlag) {
			multipleCount = (int) Math.floor(minDataValue / ZOOMED_FACTOR);
			minScaleValue = multipleCount * ZOOMED_FACTOR;
		}
		// if NOT zooming, use the UNZOOMED_FACTOR value
		else {
			multipleCount = (int) Math.floor(minDataValue / UNZOOMED_FACTOR);
			minScaleValue = multipleCount * UNZOOMED_FACTOR;
			
			if ((maxDataValue - minDataValue < 10) && (minDataValue > .5)) {
			    minScaleValue = minDataValue - .5;
			}
		}

		double baseFactor = baseFactorStartingPoint;

		boolean done = false;
		int i = 0;

		// set the range the values fit into
		double range = maxDataValue - minScaleValue;
		if (range < 1 && !zoomFlag) {
		    range = 1;
		}

		while (!done) {
			double testIncrement = niceMajorIncrementArray[i] * (baseFactor);

			int testTickCount = (int) Math.ceil(range / testIncrement + 1);

			// if there are a reasonable number of tickCounts, then stop
			if (testTickCount <= MAX_MAJOR_TICK_) {

				majorTickCount = testTickCount;

				majorTickIncrement = testIncrement;

				// first tick counts as a tick, so subtract 1
				maxScaleValue = minScaleValue
						+ ((majorTickCount - 1) * majorTickIncrement);

				return;
			}

			i++;

			if (i >= niceMajorIncrementArray.length) {
				i = 0;
				baseFactor *= 10.0;
			}

		} // end while !done

		return;
	}

    public void setMaxDataValue(double maxDataValue) {
        this.maxDataValue = maxDataValue;

        rescale();
    }

    public double getMaxDataValue() {
        return maxDataValue;

    }

    public void setMinDataValue(double minDataValue) {
        this.minDataValue = minDataValue;
        rescale();
    }

    public double getMinDataValue() {
        return minDataValue;
    }

    public int getMajorTickCount() {
        return majorTickCount;
    }

    public double getMajorTickIncrement() {
        return majorTickIncrement;
    }

    public double getMaxScaleValue() {
        return maxScaleValue;
    }

    public double getMinScaleValue() {
        return minScaleValue;
    }

    public void setNiceMajorIncrementArray(double[] niceMajorIncrementArray) {
        this.niceMajorIncrementArray = niceMajorIncrementArray;
        rescale();
    }

    public double[] getNiceMajorIncrementArray() {
        return niceMajorIncrementArray;
    }

    public boolean isZoomFlag() {
        return zoomFlag;
    }

    public void setZoomFlag(boolean zoomFlag) {
        this.zoomFlag = zoomFlag;
        rescale();

    }

    /**
     * @param baseFactorStartingPoint
     *            The baseFactorStartingPoint to set.
     */
    public void setBaseFactorStartingPoint(double baseFactorStartingPoint) {
        this.baseFactorStartingPoint = baseFactorStartingPoint;
    }

    /**
     * @return Returns the baseFactorStartingPoint.
     */
    public double getBaseFactorStartingPoint() {
        return baseFactorStartingPoint;
    }

	@Override
	public String toString() {
		String outString = " minDataValue = " + getMinDataValue()
				+ " maxDataValue = " + getMaxDataValue() + "\n"
				+ " minScaleValue = " + getMinScaleValue()
				+ " maxScaleValue = " + getMaxScaleValue() + "\n"
				+ " majorTickCount = " + getMajorTickCount()
				+ " majorTickIncrement = " + getMajorTickIncrement();

		return outString;
	}

	// For testing
	public static void main(String[] argArray) {
		double minValue = 0;
		double maxValue = .01;
		ScaleManager scaler = new ScaleManager(minValue, maxValue);

		System.out.println(scaler.toString());
	}
}
