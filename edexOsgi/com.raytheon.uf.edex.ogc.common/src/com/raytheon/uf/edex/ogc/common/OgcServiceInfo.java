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
package com.raytheon.uf.edex.ogc.common;

import java.util.LinkedList;
import java.util.List;


/**
 * Contains services metadata used to populated OGC capabilities documents.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 * @param <T>
 */
public class OgcServiceInfo<T> {
	protected String onlineResource;

	protected List<OgcOperationInfo<T>> operations = new LinkedList<OgcOperationInfo<T>>();

	/**
	 * 
	 */
	public OgcServiceInfo(String onlineResouce) {
		this.onlineResource = onlineResouce;
	}

	public void addOperationInfo(OgcOperationInfo<T> info) {
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
	public List<OgcOperationInfo<T>> getOperations() {
		return operations;
	}

	/**
	 * @param operations
	 *            the operations to set
	 */
	public void setOperations(List<OgcOperationInfo<T>> operations) {
		this.operations = operations;
	}

}
