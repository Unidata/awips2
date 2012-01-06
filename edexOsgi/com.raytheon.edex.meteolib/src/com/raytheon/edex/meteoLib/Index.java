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

public class Index {
	private float totalIndex;
	private float crossTotalIndex;
	private float verticalTotalsIndex;

	/**
	 * @return the totalIndex
	 */
	public float getTotalIndex() {
		return totalIndex;
	}

	/**
	 * @param totalIndex
	 *            the totalIndex to set
	 */
	public void setTotalIndex(float totalIndex) {
		this.totalIndex = totalIndex;
	}

	/**
	 * @return the crossTotalIndex
	 */
	public float getCrossTotalIndex() {
		return crossTotalIndex;
	}

	/**
	 * @param crossTotalIndex
	 *            the crossTotalIndex to set
	 */
	public void setCrossTotalIndex(float crossTotalIndex) {
		this.crossTotalIndex = crossTotalIndex;
	}

	/**
	 * @return the verticalTotalsIndex
	 */
	public float getVerticalTotalsIndex() {
		return verticalTotalsIndex;
	}

	/**
	 * @param verticalTotalsIndex
	 *            the verticalTotalsIndex to set
	 */
	public void setVerticalTotalsIndex(float verticalTotalsIndex) {
		this.verticalTotalsIndex = verticalTotalsIndex;
	}
}
