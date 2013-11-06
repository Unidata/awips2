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
package com.raytheon.uf.edex.wfs.v1_1_0;

import java.math.BigInteger;
import java.util.Iterator;
import java.util.List;

import net.opengis.wfs.v_1_1_0.TransactionResponseType;
import net.opengis.wfs.v_1_1_0.TransactionResultsType;
import net.opengis.wfs.v_1_1_0.TransactionSummaryType;

import com.raytheon.uf.edex.wfs.request.TransReq;

/**
 * Handles transaction requests for WFS 1.1.0
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 29, 2011            bclement     Initial creation
 * 
 * </pre>
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
            // String[] parts = request.getParameter().replaceAll("\\w+:", "")
            // .split("/");
            // String param = (parts.length > 1 ? parts[1] : parts[0]);
            // for (int i = 2; i < parts.length; ++i) {
            // param += "." + parts[i];
            // }
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
        default:
            break;
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
