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

import java.math.BigInteger;
import java.util.LinkedList;
import java.util.List;

import javax.xml.namespace.QName;

import net.opengis.filter.v_1_1_0.SortByType;
import net.opengis.filter.v_1_1_0.SortOrderType;
import net.opengis.filter.v_1_1_0.SortPropertyType;
import net.opengis.wfs.v_1_1_0.GetFeatureType;
import net.opengis.wfs.v_1_1_0.QueryType;
import net.opengis.wfs.v_1_1_0.ResultTypeType;

import com.raytheon.uf.edex.wfs.request.FeatureQuery.QFilterType;
import com.raytheon.uf.edex.wfs.request.SortBy.Order;

/**
 * @author bclement
 * 
 */
public class GetFeatureReq extends WfsRequest {

	public enum ResultType {
		results, hits
	}

	protected List<FeatureQuery> queries = new LinkedList<FeatureQuery>();

	protected String outputformat = "text/xml; subtype=gml/3.1.1";

	protected ResultType resulttype = ResultType.results;

	// may want to set default
	protected int maxFeatures = Integer.MAX_VALUE;

	/**
	 * @param type
	 */
	public GetFeatureReq() {
		super(Type.GetFeature);
	}

	public GetFeatureReq(GetFeatureType req) {
		super(Type.GetFeature);
		setRawrequest(req);
		ResultTypeType resultType = req.getResultType();
		if (resultType == ResultTypeType.HITS) {
			setResulttype(ResultType.hits);
		}
		String outputFormat = req.getOutputFormat();
		if (outputFormat != null) {
			setOutputformat(outputFormat);
		}
		BigInteger maxFeatures = req.getMaxFeatures();
		if (maxFeatures != null) {
			setMaxFeatures(maxFeatures.intValue());
		}
		List<QueryType> query = req.getQuery();
		if (query != null) {
			for (QueryType qt : query) {
				addQuery(getQuery(qt));
			}
		}
	}

	/**
	 * @param qt
	 * @return
	 */
	protected FeatureQuery getQuery(QueryType qt) {
		FeatureQuery rval = new FeatureQuery();
		rval.setFilter(qt.getFilter(), QFilterType.XMLOBJ);
		SortByType sortBy = qt.getSortBy();
		if (sortBy != null) {
			for (SortPropertyType prop : sortBy.getSortProperty()) {
				String name = prop.getPropertyName().getContent().get(0)
						.toString();
				Order o = (prop.getSortOrder() == SortOrderType.DESC ? Order.Descending
						: Order.Ascending);
				rval.addSortBy(new SortBy(name, o));
			}
		}
		String srsName = qt.getSrsName();
		if (srsName != null) {
			rval.setSrsName(srsName);
		}
		for (QName q : qt.getTypeName()) {
			rval.addTypeName(new QualifiedName(q.getNamespaceURI(), q
					.getLocalPart(), q.getPrefix()));
		}
		return rval;
	}

	public void addQuery(FeatureQuery query) {
		this.queries.add(query);
	}

	/**
	 * @return the queries
	 */
	public List<FeatureQuery> getQueries() {
		return queries;
	}

	/**
	 * @param queries
	 *            the queries to set
	 */
	public void setQueries(List<FeatureQuery> queries) {
		this.queries = queries;
	}

	/**
	 * @return the outputformat
	 */
	public String getOutputformat() {
		return outputformat;
	}

	/**
	 * @param outputformat
	 *            the outputformat to set
	 */
	public void setOutputformat(String outputformat) {
		this.outputformat = outputformat;
	}

	/**
	 * @return the resulttype
	 */
	public ResultType getResulttype() {
		return resulttype;
	}

	/**
	 * @param resulttype
	 *            the resulttype to set
	 */
	public void setResulttype(ResultType resulttype) {
		this.resulttype = resulttype;
	}

	/**
	 * @return the maxFeatures
	 */
	public int getMaxFeatures() {
		return maxFeatures;
	}

	/**
	 * @param maxFeatures
	 *            the maxFeatures to set
	 */
	public void setMaxFeatures(int maxFeatures) {
		this.maxFeatures = maxFeatures;
	}

}
