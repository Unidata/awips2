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
package com.raytheon.uf.edex.wcs;

import java.util.LinkedList;
import java.util.List;

public class WcsServiceInfo {

	protected String onlineResource;

	protected List<WcsOperationInfo> operations = new LinkedList<WcsOperationInfo>();

	/**
	 * 
	 */
	public WcsServiceInfo(String onlineResouce) {
		this.onlineResource = onlineResouce;
	}

	public void addOperationInfo(WcsOperationInfo info) {
		operations.add(info);
	}

	/**
	 * @return the onlineResource
	 */
	public String getOnlineResource() {
		return onlineResource;
	}

	/**
	 * @param onlineResource
	 *            the onlineResource to set
	 */
	public void setOnlineResource(String onlineResource) {
		this.onlineResource = onlineResource;
	}

	/**
	 * @return the operations
	 */
	public List<WcsOperationInfo> getOperations() {
		return operations;
	}

	/**
	 * @param operations
	 *            the operations to set
	 */
	public void setOperations(List<WcsOperationInfo> operations) {
		this.operations = operations;
	}

}
