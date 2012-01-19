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

public class PHT {
	private float temperature;
	private float temperature1;
	private float pressure;
	private float pressure1;
	private float dewpoint;
	private float height;
	private float height1;
	private float[] heightArray;
	private float[] pressureArray;
	private float[] temperatureArray;
	private float[] dewpointArray;
	private float[] virtualTemps;
	private float[] soundingTemps;
	private float[] soundingVirtTemps;
	private float mixingRatio;
	private int numLevels;
	private int completion;
	private float wetBulbTemp;
	private float positiveEnergy;
	private float cin;
	private float dryAdiabat;
	private float moistAdiabat;
	private int status;

	/**
	 * @return the temperature
	 */
	public float getTemperature() {
		return temperature;
	}

	/**
	 * @param temperature
	 *            the temperature to set
	 */
	public void setTemperature(float temperature) {
		this.temperature = temperature;
	}

	/**
	 * @return the temperature1
	 */
	public float getTemperature1() {
		return temperature1;
	}

	/**
	 * @param temperature1
	 *            the temperature1 to set
	 */
	public void setTemperature1(float temperature1) {
		this.temperature1 = temperature1;
	}

	/**
	 * @return the pressure
	 */
	public float getPressure() {
		return pressure;
	}

	/**
	 * @param pressure
	 *            the pressure to set
	 */
	public void setPressure(float pressure) {
		this.pressure = pressure;
	}

	/**
	 * @return the pressure1
	 */
	public float getPressure1() {
		return pressure1;
	}

	/**
	 * @param pressure1
	 *            the pressure1 to set
	 */
	public void setPressure1(float pressure1) {
		this.pressure1 = pressure1;
	}

	/**
	 * @return the dewpoint
	 */
	public float getDewpoint() {
		return dewpoint;
	}

	/**
	 * @param dewpoint
	 *            the dewpoint to set
	 */
	public void setDewpoint(float dewpoint) {
		this.dewpoint = dewpoint;
	}

	/**
	 * @return the height
	 */
	public float getHeight() {
		return height;
	}

	/**
	 * @param height
	 *            the height to set
	 */
	public void setHeight(float height) {
		this.height = height;
	}

	/**
	 * @return the height1
	 */
	public float getHeight1() {
		return height1;
	}

	/**
	 * @param height1
	 *            the height1 to set
	 */
	public void setHeight1(float height1) {
		this.height1 = height1;
	}

	/**
	 * @return the heightArray
	 */
	public float[] getHeightArray() {
		return heightArray;
	}

	/**
	 * @param heightArray
	 *            the heightArray to set
	 */
	public void setHeightArray(float[] heightArray) {
		this.heightArray = heightArray;
	}

	/**
	 * @return the pressureArray
	 */
	public float[] getPressureArray() {
		return pressureArray;
	}

	/**
	 * @param pressureArray
	 *            the pressureArray to set
	 */
	public void setPressureArray(float[] pressureArray) {
		this.pressureArray = pressureArray;
	}

	/**
	 * @return the temperatureArray
	 */
	public float[] getTemperatureArray() {
		return temperatureArray;
	}

	/**
	 * @param temperatureArray
	 *            the temperatureArray to set
	 */
	public void setTemperatureArray(float[] temperatureArray) {
		this.temperatureArray = temperatureArray;
	}

	/**
	 * @return the dewpointArray
	 */
	public float[] getDewpointArray() {
		return dewpointArray;
	}

	/**
	 * @param dewpointArray
	 *            the dewpointArray to set
	 */
	public void setDewpointArray(float[] dewpointArray) {
		this.dewpointArray = dewpointArray;
	}

	/**
	 * @return the virtualTemps
	 */
	public float[] getVirtualTemps() {
		return virtualTemps;
	}

	/**
	 * @param virtualTemps
	 *            the virtualTemps to set
	 */
	public void setVirtualTemps(float[] virtualTemps) {
		this.virtualTemps = virtualTemps;
	}

	/**
	 * @return the soundingTemps
	 */
	public float[] getSoundingTemps() {
		return soundingTemps;
	}

	/**
	 * @param soundingTemps
	 *            the soundingTemps to set
	 */
	public void setSoundingTemps(float[] soundingTemps) {
		this.soundingTemps = soundingTemps;
	}

	/**
	 * @return the soundingVirtTemps
	 */
	public float[] getSoundingVirtTemps() {
		return soundingVirtTemps;
	}

	/**
	 * @param soundingVirtTemps
	 *            the soundingVirtTemps to set
	 */
	public void setSoundingVirtTemps(float[] soundingVirtTemps) {
		this.soundingVirtTemps = soundingVirtTemps;
	}

	/**
	 * @return the mixingRatio
	 */
	public float getMixingRatio() {
		return mixingRatio;
	}

	/**
	 * @param mixingRatio
	 *            the mixingRatio to set
	 */
	public void setMixingRatio(float mixingRatio) {
		this.mixingRatio = mixingRatio;
	}

	/**
	 * @return the numLevels
	 */
	public int getNumLevels() {
		return numLevels;
	}

	/**
	 * @param numLevels
	 *            the numLevels to set
	 */
	public void setNumLevels(int numLevels) {
		this.numLevels = numLevels;
	}

	/**
	 * @return the completion
	 */
	public int getCompletion() {
		return completion;
	}

	/**
	 * @param completion
	 *            the completion to set
	 */
	public void setCompletion(int completion) {
		this.completion = completion;
	}

	/**
	 * @return the wetBulbTemp
	 */
	public float getWetBulbTemp() {
		return wetBulbTemp;
	}

	/**
	 * @param wetBulbTemp
	 *            the wetBulbTemp to set
	 */
	public void setWetBulbTemp(float wetBulbTemp) {
		this.wetBulbTemp = wetBulbTemp;
	}

	/**
	 * @return the positiveEnergy
	 */
	public float getPositiveEnergy() {
		return positiveEnergy;
	}

	/**
	 * @param positiveEnergy
	 *            the positiveEnergy to set
	 */
	public void setPositiveEnergy(float positiveEnergy) {
		this.positiveEnergy = positiveEnergy;
	}

	/**
	 * @return the cin
	 */
	public float getCin() {
		return cin;
	}

	/**
	 * @param cin
	 *            the cin to set
	 */
	public void setCin(float cin) {
		this.cin = cin;
	}

	/**
	 * @return the dryAdiabat
	 */
	public float getDryAdiabat() {
		return dryAdiabat;
	}

	/**
	 * @param dryAdiabat
	 *            the dryAdiabat to set
	 */
	public void setDryAdiabat(float dryAdiabat) {
		this.dryAdiabat = dryAdiabat;
	}

	/**
	 * @return the moistAdiabat
	 */
	public float getMoistAdiabat() {
		return moistAdiabat;
	}

	/**
	 * @param moistAdiabat
	 *            the moistAdiabat to set
	 */
	public void setMoistAdiabat(float moistAdiabat) {
		this.moistAdiabat = moistAdiabat;
	}

	/**
	 * @return the status
	 */
	public int getStatus() {
		return status;
	}

	/**
	 * @param status
	 *            the status to set
	 */
	public void setStatus(int status) {
		this.status = status;
	}

}
