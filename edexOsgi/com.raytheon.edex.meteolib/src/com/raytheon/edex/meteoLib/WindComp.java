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

public class WindComp {
	private float windDirection;
	private float windSpeed;
	private float uComp;
	private float vComp;
	private float[] uCompArray;
	private float[] vCompArray;
	private float[] windDirectionArray;
	private float[] windSpeedArray;
	private float stormMotionDir;
	private float stormMotionSpd;
	private float stormRelativeHelicity;
	private float helicity;
	private float[] compFirstInSecond;
	private float[] compFirstInKDir;
	private int gustPotential;

	/**
	 * @return the windDirection
	 */
	public float getWindDirection() {
		return windDirection;
	}

	/**
	 * @param windDirection
	 *            the windDirection to set
	 */
	public void setWindDirection(float windDirection) {
		this.windDirection = windDirection;
	}

	/**
	 * @return the windSpeed
	 */
	public float getWindSpeed() {
		return windSpeed;
	}

	/**
	 * @param windSpeed
	 *            the windSpeed to set
	 */
	public void setWindSpeed(float windSpeed) {
		this.windSpeed = windSpeed;
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

	/**
	 * @return the uCompArray
	 */
	public float[] getUCompArray() {
		return uCompArray;
	}

	/**
	 * @param compArray
	 *            the uCompArray to set
	 */
	public void setUCompArray(float[] compArray) {
		uCompArray = compArray;
	}

	/**
	 * @return the vCompArray
	 */
	public float[] getVCompArray() {
		return vCompArray;
	}

	/**
	 * @param compArray
	 *            the vCompArray to set
	 */
	public void setVCompArray(float[] compArray) {
		vCompArray = compArray;
	}

	/**
	 * @return the windDirectionArray
	 */
	public float[] getWindDirectionArray() {
		return windDirectionArray;
	}

	/**
	 * @param windDirectionArray
	 *            the windDirectionArray to set
	 */
	public void setWindDirectionArray(float[] windDirectionArray) {
		this.windDirectionArray = windDirectionArray;
	}

	/**
	 * @return the windSpeedArray
	 */
	public float[] getWindSpeedArray() {
		return windSpeedArray;
	}

	/**
	 * @param windSpeedArray
	 *            the windSpeedArray to set
	 */
	public void setWindSpeedArray(float[] windSpeedArray) {
		this.windSpeedArray = windSpeedArray;
	}

	/**
	 * @return the stormMotionDir
	 */
	public float getStormMotionDir() {
		return stormMotionDir;
	}

	/**
	 * @param stormMotionDir
	 *            the stormMotionDir to set
	 */
	public void setStormMotionDir(float stormMotionDir) {
		this.stormMotionDir = stormMotionDir;
	}

	/**
	 * @return the stormMotionSpd
	 */
	public float getStormMotionSpd() {
		return stormMotionSpd;
	}

	/**
	 * @param stormMotionSpd
	 *            the stormMotionSpd to set
	 */
	public void setStormMotionSpd(float stormMotionSpd) {
		this.stormMotionSpd = stormMotionSpd;
	}

	/**
	 * @return the stormRelativeHelicity
	 */
	public float getStormRelativeHelicity() {
		return stormRelativeHelicity;
	}

	/**
	 * @param stormRelativeHelicity
	 *            the stormRelativeHelicity to set
	 */
	public void setStormRelativeHelicity(float stormRelativeHelicity) {
		this.stormRelativeHelicity = stormRelativeHelicity;
	}

	/**
	 * @return the helicity
	 */
	public float getHelicity() {
		return helicity;
	}

	/**
	 * @param helicity
	 *            the helicity to set
	 */
	public void setHelicity(float helicity) {
		this.helicity = helicity;
	}

	/**
	 * @return the compFirstInSecond
	 */
	public float[] getCompFirstInSecond() {
		return compFirstInSecond;
	}

	/**
	 * @param compFirstInSecond
	 *            the compFirstInSecond to set
	 */
	public void setCompFirstInSecond(float[] compFirstInSecond) {
		this.compFirstInSecond = compFirstInSecond;
	}

	/**
	 * @return the compFirstInKDir
	 */
	public float[] getCompFirstInKDir() {
		return compFirstInKDir;
	}

	/**
	 * @param compFirstInKDir
	 *            the compFirstInKDir to set
	 */
	public void setCompFirstInKDir(float[] compFirstInKDir) {
		this.compFirstInKDir = compFirstInKDir;
	}

	/**
	 * @return the gustPotential
	 */
	public int getGustPotential() {
		return gustPotential;
	}

	/**
	 * @param gustPotential
	 *            the gustPotential to set
	 */
	public void setGustPotential(int gustPotential) {
		this.gustPotential = gustPotential;
	}
}
