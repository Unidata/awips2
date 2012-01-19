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
package com.raytheon.edex.util;

import java.text.ParseException;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

public class UnitConv {

	public static Unit<?> deserializer(String unit) throws ParseException {
		Unit<?> retVal = Unit.ONE;

		if (unit != null) {
			if (!unit.equals("")) {
				retVal = (Unit<?>) UnitFormat.getUCUMInstance().parseObject(
						unit);
			}
		}
		return retVal;

	}

	public static String serializer(Unit<?> unit) {
		if (unit == null) {
			return "";
		} else {
			return unit.toString();
		}
	}

}
