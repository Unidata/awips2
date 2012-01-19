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

package com.raytheon.viz.core.graphing.xy;

/**
 * A tuple of associated data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 17, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 */
public class XYData {

	private Object x;

	private Object y;

	public XYData(Object aX, Object aY) {
		x = aX;
		y = aY;
	}

	/**
	 * @return the x
	 */
	public Object getX() {
		return x;
	}

	/**
	 * @param x
	 *            the x to set
	 */
	public void setX(Object x) {
		this.x = x;
	}

	/**
	 * @return the y
	 */
	public Object getY() {
		return y;
	}

	/**
	 * @param y
	 *            the y to set
	 */
	public void setY(Object y) {
		this.y = y;
	}

}
