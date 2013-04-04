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
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;

import net.opengis.filter.v_1_1_0.FilterType;
import net.opengis.gml.v_3_1_1.AbstractFeatureType;
import net.opengis.gml.v_3_1_1.FeaturePropertyType;
import net.opengis.wfs.v_1_1_0.FeatureCollectionType;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.geometry.jts.JTS;
import org.hibernate.criterion.Criterion;
import org.hibernatespatial.criterion.SpatialRestrictions;
import org.opengis.feature.simple.SimpleFeature;

import com.raytheon.uf.edex.ogc.common.OgcBoundingBox;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.WfsException.Code;
import com.raytheon.uf.edex.wfs.WfsProvider.WfsOpType;
import com.raytheon.uf.edex.wfs.filter.FilterProcessor;
import com.raytheon.uf.edex.wfs.reg.WfsQuery;
import com.raytheon.uf.edex.wfs.reg.WfsRegistryImpl;
import com.raytheon.uf.edex.wfs.reg.WfsSource;
import com.raytheon.uf.edex.wfs.request.FeatureQuery;
import com.raytheon.uf.edex.wfs.request.GetFeatureReq;
import com.raytheon.uf.edex.wfs.request.GetFeatureReq.ResultType;
import com.raytheon.uf.edex.wfs.request.QualifiedName;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Polygon;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class FeatureFetcher {

	protected Log log = LogFactory.getLog(this.getClass());

	protected WfsRegistryImpl registry;

	/**
	 * @param registry
	 */
	public FeatureFetcher(WfsRegistryImpl registry) {
		this.registry = registry;
	}

	public List<List<SimpleFeature>> getSimpleFeatures(GetFeatureReq request,
			OgcServiceInfo<WfsOpType> serviceinfo) throws WfsException {
		List<List<SimpleFeature>> rval = new ArrayList<List<SimpleFeature>>();
		for (FeatureQuery q : request.getQueries()) {
			for (QualifiedName type : q.getTypeNames()) {
				WfsSource source = registry.getSource(type);
				if (source != null) {
					String spatial = source.getFeatureSpatialField(type);
					VisitorBag bag = new VisitorBag(
							source.getFeatureEntity(type), spatial);
					bag.setFieldMap(source.getFieldMap());
					WfsQuery wfsq;
					try {
						wfsq = new WfsQuery(getQuery(q, bag),
								request.getMaxFeatures(), q.getSortBys(),
								q.getPropertyNames());
					} catch (Exception e) {
						log.error("Problem parsing wfs query", e);
						throw new WfsException(Code.INVALID_REQUEST,
								"Invalid filter");
					}
					rval.add(source.querySimple(type, wfsq));
				} else {
					throw new WfsException(Code.INVALID_REQUEST,
							"Unkown feature type: " + type);
				}
			}
		}
		return rval;
	}

	public FeatureCollectionType getFeatures(GetFeatureReq request,
			OgcServiceInfo<WfsOpType> serviceinfo) throws WfsException {
		FeatureCollectionType featColl = new FeatureCollectionType();
		List<FeaturePropertyType> members = featColl.getFeatureMember();
		long count = 0;
		for (FeatureQuery q : request.getQueries()) {
			for (QualifiedName type : q.getTypeNames()) {
				WfsSource source = registry.getSource(type);
				if (source != null) {
					String spatial = source.getFeatureSpatialField(type);
					VisitorBag bag = new VisitorBag(
							source.getFeatureEntity(type), spatial);
					bag.setFieldMap(source.getFieldMap());
					WfsQuery wfsq;
					try {
						wfsq = new WfsQuery(getQuery(q, bag),
								request.getMaxFeatures(), q.getSortBys(),
								q.getPropertyNames());
					} catch (Exception e) {
						log.error("Problem parsing wfs query", e);
						throw new WfsException(Code.INVALID_REQUEST,
								"Invalid filter");
					}
					if (request.getResulttype() == ResultType.hits) {
						count += source.count(type, wfsq);
					} else {
						List<JAXBElement<? extends AbstractFeatureType>> result = source
								.query(type, wfsq);
						count += populateFeatures(members, result);
					}
				} else {
					throw new WfsException(Code.INVALID_REQUEST,
							"Unkown feature type: " + type);
				}
			}
		}
		featColl.setNumberOfFeatures(new BigInteger("" + count));
		return featColl;
	}

	protected long populateFeatures(List<FeaturePropertyType> members,
			List<JAXBElement<? extends AbstractFeatureType>> results) {
		if (results == null || results.isEmpty()) {
			return 0;
		}
		for (JAXBElement<? extends AbstractFeatureType> feature : results) {
			FeaturePropertyType propType = new FeaturePropertyType();
			propType.setFeature(feature);
			members.add(propType);
		}
		return results.size();
	}

	/**
	 * @param q
	 * @param spatial
	 * @return
	 * @throws Exception
	 */
	protected Criterion getQuery(FeatureQuery q, VisitorBag bag)
			throws Exception {
		Criterion rval = null;
		switch (q.getFilterType()) {
		case BBOX:
			OgcBoundingBox bbox = (OgcBoundingBox) q.getFilter();
			rval = getBbox(bbox.getMiny(), bbox.getMinx(), bbox.getMaxy(),
					bbox.getMaxx(), bag);
			break;
		case FIDS:
			// TODO
			rval = getDefault(bag);
			break;
		case XML:
			FilterType f = parseFilterXml((String) q.getFilter());
			rval = getFromFilter(f, bag);
			break;
		case XMLOBJ:
			FilterType filter = (FilterType) q.getFilter();
			rval = getFromFilter(filter, bag);
			break;
		default:
			rval = getDefault(bag);
		}
		return rval;
	}

	protected Criterion getDefault(VisitorBag bag) {
		return null;
	}

	protected FilterType parseFilterXml(String xml) throws JAXBException {
		return (FilterType) registry.unmarshal(xml);
	}

	protected Criterion getFromFilter(FilterType filter, VisitorBag bag)
			throws Exception {
		if (filter != null) {
			FilterProcessor proc = new FilterProcessor(filter);
			return (Criterion) proc.accept(new QueryFilterVisitor(), bag);
		} else {
			// FIXME
			return null;
		}

	}

	protected Criterion getBbox(double lowerLat, double lowerLon,
			double upperLat, double upperLon, VisitorBag bag) {
		Polygon geom = JTS.toGeometry(new Envelope(lowerLon, upperLon,
				lowerLat, lowerLat));
		return SpatialRestrictions.within(bag.getSpatialField(), geom);
	}

}
