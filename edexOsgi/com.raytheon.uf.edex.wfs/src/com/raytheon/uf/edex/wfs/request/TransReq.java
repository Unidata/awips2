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
package com.raytheon.uf.edex.wfs.request;

import net.opengis.wfs.v_1_1_0.TransactionType;

import com.raytheon.uf.edex.wfs.reg.Unique;

/**
 * Wrapper for WFS transaction request
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * April 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class TransReq extends WfsRequest {

	public enum TransType {
		Insert, Update, Delete, Native
	}

	protected TransType transType;

	protected String vendorId;

	protected boolean safeToIgnore = true;

	protected String parameter;

	/**
	 * @param type
	 */
	public TransReq(TransType type) {
		super(WfsRequest.Type.Transaction);
		this.transType = type;
	}

	/**
	 * @param xml
	 * @param obj
	 * @return
	 */
    public static WfsRequest buildTransReq(
            TransactionType transaction) {
        WfsRequest rval = null;
		for (Object obj : transaction.getInsertOrUpdateOrDelete()) {
			if (obj instanceof Unique) {
				TransReq ret = new TransReq(TransType.Native);
				Unique op = (Unique) obj;
				ret.setSafeToIgnore(op.isSafeToIgnore());
				ret.setVendorId(op.getVendorId());
				ret.setParameter(op.getParameter());
				rval = ret;
			}
		}
        rval.setRawrequest(transaction);
		return rval;
	}

	/**
	 * @return the transType
	 */
	public TransType getTransType() {
		return transType;
	}

	/**
	 * @param transType
	 *            the transType to set
	 */
	public void setTransType(TransType transType) {
		this.transType = transType;
	}

	/**
	 * @return the vendorId
	 */
	public String getVendorId() {
		return vendorId;
	}

	/**
	 * @param vendorId
	 *            the vendorId to set
	 */
	public void setVendorId(String vendorId) {
		this.vendorId = vendorId;
	}

	/**
	 * @return the safeToIgnore
	 */
	public boolean isSafeToIgnore() {
		return safeToIgnore;
	}

	/**
	 * @param safeToIgnore
	 *            the safeToIgnore to set
	 */
	public void setSafeToIgnore(boolean safeToIgnore) {
		this.safeToIgnore = safeToIgnore;
	}

	/**
	 * @return the parameter
	 */
	public String getParameter() {
		return parameter;
	}

	/**
	 * @param parameter
	 *            the parameter to set
	 */
	public void setParameter(String parameter) {
		this.parameter = parameter;
	}

}
