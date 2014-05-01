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
package com.raytheon.viz.hydro.pointprecipitation;

import java.util.Arrays;

/**
 * Point Precipitation Data Object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 5, 2009  2257       mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PointPrecipData {
	private String lid = null;

	private String pe = null;

	private String ts = null;

	private int[] summedFlag = new int[PointPrecipConstants.NUMDURATIONS];

	private double[] hrfill = new double[PointPrecipConstants.NUMDURATIONS];

	private double[] amount = new double[PointPrecipConstants.NUMDURATIONS];

	private double maxValue = PointPrecipConstants.MISSING_VALUE;

	public PointPrecipData() {
		Arrays.fill(amount, PointPrecipConstants.MISSING_PRECIP);
	}

	/**
	 * @return the lid
	 */
	public String getLid() {
		return lid;
	}

	/**
	 * @param lid
	 *            the lid to set
	 */
	public void setLid(String lid) {
		this.lid = lid;
	}

	/**
	 * @return the pe
	 */
	public String getPe() {
		return pe;
	}

	/**
	 * @param pe
	 *            the pe to set
	 */
	public void setPe(String pe) {
		this.pe = pe;
	}

	/**
	 * @return the ts
	 */
	public String getTs() {
		return ts;
	}

	/**
	 * @param ts
	 *            the ts to set
	 */
	public void setTs(String ts) {
		this.ts = ts;
	}

	/**
	 * @return the summedFlag
	 */
	public int[] getSummedFlag() {
		return summedFlag;
	}

	/**
	 * @param summedFlag
	 *            the summedFlag to set
	 */
	public void setSummedFlag(int[] summedFlag) {
		this.summedFlag = summedFlag;
	}

	/**
	 * @return the hrfill
	 */
	public double[] getHrfill() {
		return hrfill;
	}

	/**
	 * @param hrfill
	 *            the hrfill to set
	 */
	public void setHrfill(double[] hrfill) {
		this.hrfill = hrfill;
	}

	/**
	 * @return the amount
	 */
	public double[] getAmount() {
		return amount;
	}

	/**
	 * @param amount
	 *            the amount to set
	 */
	public void setAmount(double[] amount) {
		this.amount = amount;
	}

	/**
	 * @return the maxValue
	 */
	public double getMaxValue() {
		return maxValue;
	}

	/**
	 * @param maxValue
	 *            the maxValue to set
	 */
	public void setMaxValue(double maxValue) {
		this.maxValue = maxValue;
	}
}
