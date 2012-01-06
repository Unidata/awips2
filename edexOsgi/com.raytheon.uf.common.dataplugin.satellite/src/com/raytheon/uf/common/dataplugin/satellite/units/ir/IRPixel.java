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

package com.raytheon.uf.common.dataplugin.satellite.units.ir;

import javax.measure.converter.UnitConverter;
import javax.measure.quantity.Temperature;
import javax.measure.unit.DerivedUnit;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.apache.commons.lang.builder.HashCodeBuilder;

/**
 * Represents a pixel value on a satellite IR image
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 29, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 */
public class IRPixel extends DerivedUnit<Temperature> {

	private static final long serialVersionUID = 1L;

	@Override
	public boolean equals(Object anObject) {
		return (anObject instanceof IRPixel);
	}

	@Override
	public Unit<Temperature> getStandardUnit() {
		return SI.KELVIN;
	}

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public UnitConverter toStandardUnit() {
		return new IRPixelToTempConverter();
	}

}
