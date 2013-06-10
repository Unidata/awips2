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
 * Apr 29, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs.provider;

import java.math.BigInteger;
import java.util.Iterator;
import java.util.List;

import net.opengis.wfs.v_1_1_0.TransactionResponseType;
import net.opengis.wfs.v_1_1_0.TransactionResultsType;
import net.opengis.wfs.v_1_1_0.TransactionSummaryType;

import com.raytheon.uf.edex.wfs.request.TransReq;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class Transactor {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wfs.WfsProvider#transaction(com.raytheon.uf.edex
	 * .wfs.request.TransReq)
	 */
	public TransactionResponseType transaction(TransReq request) {
		TransactionResponseType rval = new TransactionResponseType();
		rval.setTransactionSummary(getTransSummary(0, 0, 0));
		// FIXME should use source
		// rval.setTransactionResults(getTransResults(request));
		return rval;
	}

	/**
	 * @param request
	 * @return
	 */
	protected TransactionResultsType getTransResults(TransReq request) {
		TransactionResultsType rval = new TransactionResultsType();
		switch (request.getTransType()) {
		case Native:
			String[] parts = request.getParameter().replaceAll("\\w+:", "")
					.split("/");
			String param = (parts.length > 1 ? parts[1] : parts[0]);
			for (int i = 2; i < parts.length; ++i) {
				param += "." + parts[i];
			}
			// String plugname = featureManager.getFeaturePlugName(parts[0]);
			// MorphiaStrategy strat = new MorphiaStrategy();
			// try {
			// List<Object> unique = strat.getUnique(plugname, param);
			// ActionType atype = new ActionType();
			// atype.setLocator("unique");
			// atype.setMessage(join(unique));
			// List<ActionType> alist = Arrays.asList(atype);
			// rval.setAction(alist);
			// } catch (PluginException e) {
			// log.error("Problem querying for plugin: " + plugname, e);
			// }
		}
		return rval;
	}

	protected String join(List<Object> list) {
		String rval = "";
		if (!list.isEmpty()) {
			Iterator<Object> i = list.iterator();
			rval = i.next().toString();
			while (i.hasNext()) {
				rval += "," + i.next();
			}
		}
		return rval;
	}

	/**
	 * @return
	 */
	protected TransactionSummaryType getTransSummary(int deleted, int inserted,
			int updated) {
		TransactionSummaryType rval = new TransactionSummaryType();
		rval.setTotalDeleted(getBigInt(deleted));
		rval.setTotalInserted(getBigInt(inserted));
		rval.setTotalUpdated(getBigInt(updated));
		return rval;
	}

	protected BigInteger getBigInt(int i) {
		return new BigInteger("" + i);
	}
}
