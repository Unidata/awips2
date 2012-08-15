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
/**
 * 
 */
package com.raytheon.viz.warngen.util;

import org.apache.velocity.tools.generic.MathTool;

/**
 * @author bwoodle
 *
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 31, 2012   15219    Qinglu Lin  Added roundAndPad().
 * 
 */
public class WarnGenMathTool extends MathTool {

	/**
	 * 
	 */
	public WarnGenMathTool() {
		super();
	}
	
	/**
	 * This method is useful for rounding to the nearest 5 or 10 of a specific value.  For instance, wind speeds in the third bullet should be rounded to the nearest 5 MPH so they should call this method with the arguments (windSpeed, 5).
	 * @param num
	 * @param multiple
	 * @return
	 */
	public static Integer roundToInt(double num, double multiple) {
		double lowValue = num - (num % multiple);
		double highValue = num + (multiple - (num % multiple));
		if((highValue - num) <= (num - lowValue)) {
			return Integer.valueOf((int) Math.rint(highValue));
		} else {
			return Integer.valueOf((int) Math.rint(lowValue));
		}
	}
	
	public static Integer roundTo5(double num) {
		return roundToInt(num, 5);
	}

	/**
	 * Round movement direction to integer, and pad it with leading zeros
	 * if it's less than 100
	 */
	public static String roundAndPad(double direction) {
		int num = (int)Math.rint(direction);
		if (num < 10)
			return String.format ("00%s",num);
		else 
			if (num < 100)
				return  String.format ("0%s",num);
			else
				return String.format ("%s",num);
	}
	
}
