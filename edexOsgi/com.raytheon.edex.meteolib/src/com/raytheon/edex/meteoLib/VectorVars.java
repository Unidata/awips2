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

public class VectorVars {
	private float[] qx;
	private float[] qy;
	private float[] slqy;
	private float[] slqx;
	private float[] dadxdt;
	private float[] dadydt;
	private float minimum;
	private float maximum;
	private float range;
	private float[] bx;
	private float[] by;
	private float[] solutionVector;
	private int status;

	/**
	 * @return the qx
	 */
	public float[] getQx() {
		return qx;
	}

	/**
	 * @param qx
	 *            the qx to set
	 */
	public void setQx(float[] qx) {
		this.qx = qx;
	}

	/**
	 * @return the qy
	 */
	public float[] getQy() {
		return qy;
	}

	/**
	 * @param qy
	 *            the qy to set
	 */
	public void setQy(float[] qy) {
		this.qy = qy;
	}

	/**
	 * @return the slqy
	 */
	public float[] getSlqy() {
		return slqy;
	}

	/**
	 * @param slqy
	 *            the slqy to set
	 */
	public void setSlqy(float[] slqy) {
		this.slqy = slqy;
	}

	/**
	 * @return the slqx
	 */
	public float[] getSlqx() {
		return slqx;
	}

	/**
	 * @param slqx
	 *            the slqx to set
	 */
	public void setSlqx(float[] slqx) {
		this.slqx = slqx;
	}

	/**
	 * @return the dadxdt
	 */
	public float[] getDadxdt() {
		return dadxdt;
	}

	/**
	 * @param dadxdt
	 *            the dadxdt to set
	 */
	public void setDadxdt(float[] dadxdt) {
		this.dadxdt = dadxdt;
	}

	/**
	 * @return the dadydt
	 */
	public float[] getDadydt() {
		return dadydt;
	}

	/**
	 * @param dadydt
	 *            the dadydt to set
	 */
	public void setDadydt(float[] dadydt) {
		this.dadydt = dadydt;
	}

	/**
	 * @return the minimum
	 */
	public float getMinimum() {
		return minimum;
	}

	/**
	 * @param minimum
	 *            the minimum to set
	 */
	public void setMinimum(float minimum) {
		this.minimum = minimum;
	}

	/**
	 * @return the maximum
	 */
	public float getMaximum() {
		return maximum;
	}

	/**
	 * @param maximum
	 *            the maximum to set
	 */
	public void setMaximum(float maximum) {
		this.maximum = maximum;
	}

	/**
	 * @return the range
	 */
	public float getRange() {
		return range;
	}

	/**
	 * @param range
	 *            the range to set
	 */
	public void setRange(float range) {
		this.range = range;
	}

	/**
	 * @return the bx
	 */
	public float[] getBx() {
		return bx;
	}

	/**
	 * @param bx
	 *            the bx to set
	 */
	public void setBx(float[] bx) {
		this.bx = bx;
	}

	/**
	 * @return the by
	 */
	public float[] getBy() {
		return by;
	}

	/**
	 * @param by
	 *            the by to set
	 */
	public void setBy(float[] by) {
		this.by = by;
	}

	/**
	 * @return the solutionVector
	 */
	public float[] getSolutionVector() {
		return solutionVector;
	}

	/**
	 * @param solutionVector
	 *            the solutionVector to set
	 */
	public void setSolutionVector(float[] solutionVector) {
		this.solutionVector = solutionVector;
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
