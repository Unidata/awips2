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
* May 9, 2012            bclement     Initial creation
*
*/ 
package com.raytheon.uf.edex.wfs.reg;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.Criteria;
import org.hibernate.SessionFactory;
import org.hibernate.classic.Session;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;

import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.ogc.common.OgcGeoBoundingBox;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.WfsException.Code;
import com.raytheon.uf.edex.wfs.WfsFeatureType;
import com.raytheon.uf.edex.wfs.request.QualifiedName;
import com.raytheon.uf.edex.wfs.request.SortBy;

/**
 *
 * @author bclement
 * @version 1.0	
 */
public abstract class AbstractWfsSource<T> implements WfsSource {

	protected String key;

	protected static String defaultCRS = "crs:84";

	protected static OgcGeoBoundingBox fullBbox = new OgcGeoBoundingBox(180,
			-180, 90, -90);

	protected Log log = LogFactory.getLog(this.getClass());

	protected abstract CoreDao getDao() throws Exception;

	public AbstractWfsSource(String key) {
		this.key = key;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wfs.reg.WfsSource#listFeatureTypes()
	 */
	@Override
	public abstract List<WfsFeatureType> listFeatureTypes();

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wfs.reg.WfsSource#describeFeatureType(com.raytheon
	 * .uf.edex.wfs.request.QualifiedName)
	 */
	@Override
	public abstract String describeFeatureType(QualifiedName feature);

	/**
	 * Utility method for reading text files from the classpath
	 * 
	 * @param loader
	 * @param location
	 * @return
	 * @throws IOException
	 */
	protected String getResource(ClassLoader loader, String location)
			throws IOException {
		String rval;
		try {
			InputStream in = loader.getResourceAsStream(location);
			rval = new java.util.Scanner(in).useDelimiter("\\A").next();
		} catch (Throwable e) {
			throw new IOException(e);
		}
		return rval;
	}

	protected List<T> queryInternal(QualifiedName feature, WfsQuery query)
			throws WfsException {
		query = modQuery(query);
		List<T> rval;
		Session sess = null;
		try {
			CoreDao dao = getDao();
			sess = dao.getSessionFactory().openSession();
			Criteria criteria = sess.createCriteria(getFeatureEntity(feature));
			criteria = modCriteria(criteria);
			criteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
			populateCriteria(criteria, query);
			criteria.setMaxResults(query.getMaxResults());
			List<SortBy> sortBys = query.getSortBys();
			addOrder(criteria, sortBys);
			rval = getResults(criteria);
		} catch (Exception e) {
			log.error("Problem querying for feature", e);
			throw new WfsException(Code.INTERNAL_SERVER_ERROR);
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		return rval;
	}

	@SuppressWarnings("unchecked")
	protected List<T> getResults(Criteria criteria) {
		return criteria.list();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wfs.reg.WfsSource#getFeatureSpatialField(com.raytheon
	 * .uf.edex.wfs.request.QualifiedName)
	 */
	@Override
	public abstract String getFeatureSpatialField(QualifiedName feature);

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wfs.reg.WfsSource#getFeatureEntity(com.raytheon.
	 * uf.edex.wfs.request.QualifiedName)
	 */
	@Override
	public abstract Class<?> getFeatureEntity(QualifiedName feature);

	/**
	 * Hook for implementing classes to modify the query object
	 * 
	 * @param wfsq
	 * @return
	 */
	protected WfsQuery modQuery(WfsQuery wfsq) {
		return wfsq;
	}

	protected Criteria modCriteria(Criteria crit) {
		return crit;
	}

	protected void addOrder(Criteria criteria, List<SortBy> sortBys) {
		if (sortBys == null || sortBys.isEmpty()) {
			return;
		}
		for (SortBy sb : sortBys) {
			switch (sb.getOrder()) {
			case Ascending:
				criteria.addOrder(Order.asc(sb.getProperty()));
				break;
			case Descending:
				criteria.addOrder(Order.desc(sb.getProperty()));
				break;
			default:
				log.warn("Unrecognized order: " + sb.getOrder());
			}
		}
	}

	protected void populateCriteria(Criteria criteria, WfsQuery query) {
		query = modQuery(query);
		Criterion criterion = query.getCriterion();
		if (criterion != null) {
			criteria.add(criterion);
		}
		int maxResults = query.getMaxResults();
		if (maxResults > -1) {
			criteria.setMaxResults(maxResults);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wfs.reg.WfsSource#distinct(com.raytheon.uf.edex.
	 * wfs.request.QualifiedName, com.raytheon.uf.edex.db.api.DatabaseQuery)
	 */
	@Override
	public List<String> distinct(QualifiedName feature, WfsQuery query) {
		query = modQuery(query);
		List<String> rval;
		try {
			// List<?> res = getDao().queryByCriteria(query);
			// rval = new ArrayList<String>(res.size());
			// for (Object obj : res) {
			// ConvertUtil converter = BundleContextAccessor
			// .getService(ConvertUtil.class);
			// rval.add(converter.toString(obj));
			// }
			// FIXME
			rval = null;
		} catch (Exception e) {
			log.error("Problem querying for metar", e);
			rval = new ArrayList<String>(0);
		}
		return rval;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wfs.reg.WfsSource#count(com.raytheon.uf.edex.wfs
	 * .request.QualifiedName, com.raytheon.uf.edex.db.api.DatabaseQuery)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public long count(QualifiedName feature, WfsQuery query)
			throws WfsException {
		long rval;
		Session sess = null;
		try {
			CoreDao dao = getDao();
			SessionFactory sessFact = dao.getSessionFactory();
			sess = sessFact.openSession();
			Criteria criteria = sess.createCriteria(getFeatureEntity(feature));
			criteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
			populateCriteria(criteria, query);
			criteria.setProjection(Projections.rowCount());
			List<Number> list = criteria.list();
			rval = list.get(0).longValue();
		} catch (Exception e) {
			log.error(e);
			throw new WfsException(Code.INTERNAL_SERVER_ERROR);
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		return rval;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wfs.reg.WfsSource#getKey()
	 */
	@Override
	public String getKey() {
		return key;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wfs.reg.WfsSource#getJaxbClasses()
	 */
	@Override
	public abstract Class<?>[] getJaxbClasses();

	@Override
	public Map<String, String> getFieldMap() {
		return null;
	}
}
