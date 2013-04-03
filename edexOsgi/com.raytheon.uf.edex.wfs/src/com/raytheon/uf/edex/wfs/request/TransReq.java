/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2010 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 6825 Pine Street, Suite 340
 * Mail Stop B8
 * Omaha, NE 68106
 * 402.291.0100
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *
 */
package com.raytheon.uf.edex.wfs.request;

import net.opengis.wfs.v_1_1_0.TransactionType;

import com.raytheon.uf.edex.wfs.reg.Unique;

/**
 * @author bclement
 * 
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
	public static WfsRequest buildTransReq(TransactionType transaction) {
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
