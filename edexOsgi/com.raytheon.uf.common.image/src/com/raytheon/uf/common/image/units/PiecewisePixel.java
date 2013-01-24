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

import javax.measure.converter.UnitConverter;
import javax.measure.quantity.Quantity;
import javax.measure.unit.DerivedUnit;
import javax.measure.unit.Unit;

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

public class PiecewisePixel<Q extends Quantity> extends DerivedUnit<Q> {

	private final Unit<Q> stdUnit;

	private final double[] pixelValues;

	private final double[] stdValues;

	@SuppressWarnings("unchecked")
	public PiecewisePixel(Unit<Q> dispUnit, double[] pixelValues,
			double[] dispValues) {
		super();
		if (pixelValues.length != dispValues.length) {
			throw new IllegalArgumentException(
					"pixelValues and stdValues arrays must be of equal length");
		}
		this.pixelValues = pixelValues;

		if (!dispUnit.toStandardUnit().isLinear()) {
			stdUnit = dispUnit;
		} else {
			this.stdUnit = (Unit<Q>) dispUnit.getStandardUnit();
		}

		UnitConverter toStd = dispUnit.getConverterTo(stdUnit);
		this.stdValues = new double[dispValues.length];
		for (int i = 0; i < dispValues.length; i++) {
			stdValues[i] = toStd.convert(dispValues[i]);
		}
	}

	private static final long serialVersionUID = 1L;

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.measure.unit.Unit#getStandardUnit()
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Unit<Q> getStandardUnit() {
		return (Unit<Q>) stdUnit.getStandardUnit();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.measure.unit.Unit#toStandardUnit()
	 */
	@Override
	public UnitConverter toStandardUnit() {
		if (!stdUnit.toStandardUnit().isLinear()) {
			return stdUnit.toStandardUnit().concatenate(
					new PiecewiseLinearConverter(pixelValues, stdValues));
		} else {
			return new PiecewiseLinearConverter(pixelValues, stdValues);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + Arrays.hashCode(pixelValues);
		result = prime * result + ((stdUnit == null) ? 0 : stdUnit.hashCode());
		result = prime * result + Arrays.hashCode(stdValues);
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final PiecewisePixel<Q> other = (PiecewisePixel<Q>) obj;
		if (!Arrays.equals(pixelValues, other.pixelValues))
			return false;
		if (stdUnit == null) {
			if (other.stdUnit != null)
				return false;
		} else if (!stdUnit.equals(other.stdUnit))
			return false;
		if (!Arrays.equals(stdValues, other.stdValues))
			return false;
		return true;
	}

}
