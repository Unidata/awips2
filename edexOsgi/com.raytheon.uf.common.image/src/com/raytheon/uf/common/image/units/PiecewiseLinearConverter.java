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
package com.raytheon.uf.common.image.units;

import java.util.Arrays;

import javax.measure.converter.ConversionException;
import javax.measure.converter.UnitConverter;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class PiecewiseLinearConverter extends UnitConverter {

	private static final long serialVersionUID = 1L;

	private double[] xVals;

	private double[] yVals;

	public PiecewiseLinearConverter(double[] xVals, double[] yVals) {
		this.xVals = xVals;
		this.yVals = yVals;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.measure.converter.UnitConverter#convert(double)
	 */
	@Override
	public double convert(double x) throws ConversionException {
		if (Double.isNaN(x)) {
			return Double.NaN;
		}

		int i;
		for (i = 0; i < xVals.length - 1; i++) {
			if ((x >= xVals[i]) && (x <= xVals[i + 1]))
				break;
		}

		double y;

		if (i >= xVals.length - 1) {
			y = Double.NaN;
		} else {
			// interpolate
			y = (x - xVals[i]) * (yVals[i + 1] - yVals[i])
					/ (xVals[i + 1] - xVals[i]) + yVals[i];
		}

		return y;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.measure.converter.UnitConverter#inverse()
	 */
	@Override
	public UnitConverter inverse() {
		return new PiecewiseLinearConverter(yVals, xVals);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.measure.converter.UnitConverter#isLinear()
	 */
	@Override
	public boolean isLinear() {
		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.measure.converter.UnitConverter#concatenate(javax.measure.converter
	 * .UnitConverter)
	 */
	@Override
	public UnitConverter concatenate(UnitConverter converter) {
		// TODO Auto-generated method stub
		UnitConverter result = super.concatenate(converter);
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Arrays.hashCode(xVals);
		result = prime * result + Arrays.hashCode(yVals);
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		final PiecewiseLinearConverter other = (PiecewiseLinearConverter) obj;
		if (!Arrays.equals(xVals, other.xVals))
			return false;
		if (!Arrays.equals(yVals, other.yVals))
			return false;
		return true;
	}

}
