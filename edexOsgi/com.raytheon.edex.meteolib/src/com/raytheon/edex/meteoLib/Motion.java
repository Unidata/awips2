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

package com.raytheon.edex.meteoLib;

public class Motion {
	private float direction;
	private float speed;
	private float uComp;
	private float vComp;

	/**
	 * @return the direction
	 */
	public float getDirection() {
		return direction;
	}

	/**
	 * @param direction
	 *            the direction to set
	 */
	public void setDirection(float direction) {
		this.direction = direction;
	}

	/**
	 * @return the speed
	 */
	public float getSpeed() {
		return speed;
	}

	/**
	 * @param speed
	 *            the speed to set
	 */
	public void setSpeed(float speed) {
		this.speed = speed;
	}

	/**
	 * @return the uComp
	 */
	public float getUComp() {
		return uComp;
	}

	/**
	 * @param comp
	 *            the uComp to set
	 */
	public void setUComp(float comp) {
		uComp = comp;
	}

	/**
	 * @return the vComp
	 */
	public float getVComp() {
		return vComp;
	}

	/**
	 * @param comp
	 *            the vComp to set
	 */
	public void setVComp(float comp) {
		vComp = comp;
	}
}
