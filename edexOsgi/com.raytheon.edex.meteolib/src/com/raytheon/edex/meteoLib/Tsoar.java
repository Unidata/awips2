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

public class Tsoar {
	private float potentialTempForecast;
	private float heightMinimumEffectiveConvection;
	private float tempMinimumEffectiveConvection;
	private float heightMaxThermalAlt;
	private float tempMaxThermalAlt;
	private float soarIndex;
	private float triggerTemperature;

	/**
	 * @return the potentialTempForecast
	 */
	public float getPotentialTempForecast() {
		return potentialTempForecast;
	}

	/**
	 * @param potentialTempForecast
	 *            the potentialTempForecast to set
	 */
	public void setPotentialTempForecast(float potentialTempForecast) {
		this.potentialTempForecast = potentialTempForecast;
	}

	/**
	 * @return the heightMinimumEffectiveConvection
	 */
	public float getHeightMinimumEffectiveConvection() {
		return heightMinimumEffectiveConvection;
	}

	/**
	 * @param heightMinimumEffectiveConvection
	 *            the heightMinimumEffectiveConvection to set
	 */
	public void setHeightMinimumEffectiveConvection(
			float heightMinimumEffectiveConvection) {
		this.heightMinimumEffectiveConvection = heightMinimumEffectiveConvection;
	}

	/**
	 * @return the tempMinimumEffectiveConvection
	 */
	public float getTempMinimumEffectiveConvection() {
		return tempMinimumEffectiveConvection;
	}

	/**
	 * @param tempMinimumEffectiveConvection
	 *            the tempMinimumEffectiveConvection to set
	 */
	public void setTempMinimumEffectiveConvection(
			float tempMinimumEffectiveConvection) {
		this.tempMinimumEffectiveConvection = tempMinimumEffectiveConvection;
	}

	/**
	 * @return the heightMaxThermalAlt
	 */
	public float getHeightMaxThermalAlt() {
		return heightMaxThermalAlt;
	}

	/**
	 * @param heightMaxThermalAlt
	 *            the heightMaxThermalAlt to set
	 */
	public void setHeightMaxThermalAlt(float heightMaxThermalAlt) {
		this.heightMaxThermalAlt = heightMaxThermalAlt;
	}

	/**
	 * @return the tempMaxThermalAlt
	 */
	public float getTempMaxThermalAlt() {
		return tempMaxThermalAlt;
	}

	/**
	 * @param tempMaxThermalAlt
	 *            the tempMaxThermalAlt to set
	 */
	public void setTempMaxThermalAlt(float tempMaxThermalAlt) {
		this.tempMaxThermalAlt = tempMaxThermalAlt;
	}

	/**
	 * @return the soarIndex
	 */
	public float getSoarIndex() {
		return soarIndex;
	}

	/**
	 * @param soarIndex
	 *            the soarIndex to set
	 */
	public void setSoarIndex(float soarIndex) {
		this.soarIndex = soarIndex;
	}

	/**
	 * @return the triggerTemperature
	 */
	public float getTriggerTemperature() {
		return triggerTemperature;
	}

	/**
	 * @param triggerTemperature
	 *            the triggerTemperature to set
	 */
	public void setTriggerTemperature(float triggerTemperature) {
		this.triggerTemperature = triggerTemperature;
	}

}
