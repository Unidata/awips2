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
* Feb 21, 2012            bclement     Initial creation
*
*/ 
package com.raytheon.uf.edex.plugin.grib.ogc;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.regex.Matcher;

import org.hibernate.Criteria;
import org.hibernate.SessionFactory;
import org.hibernate.classic.Session;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.Restrictions;

import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcException.Code;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.ogc.common.db.SimpleDimension;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;
import com.raytheon.uf.edex.ogc.common.time.ForecastTimeUtil;

/**
 *
 * @author bclement
 * @version 1.0	
 */
public class GribRecordFinder {

	protected static String level1Key = "level1";

	protected static String level2Key = "level2";

	protected static String pertKey = "perturbation_num";

	protected static String eTypeKey = "ensemble_type";

	protected static String versKey = "version";

	public static class Level {
		public Double value;

		public String units;
	}

	public static List<GribRecord> find(LayerTransformer transformer,
			String key, String layerName, Date time,
			Map<String, String> dimensions)
			throws OgcException {
		SimpleLayer l = getLayer(transformer, layerName);
		SortedSet<DataTime> times = new ForecastTimeUtil().getDataTimes(l,
				time, dimensions);
		return findInternal(l, transformer, key, layerName, times, dimensions);
	}

	public static List<GribRecord> find(LayerTransformer transformer,
			String key, String layerName, String time,
			Map<String, String> dimensions) throws OgcException {
		SimpleLayer l = getLayer(transformer, layerName);
		SortedSet<DataTime> times = new ForecastTimeUtil().getDataTimes(l,
				time, dimensions);
		return findInternal(l, transformer, key, layerName, times, dimensions);
	}

	protected static List<GribRecord> findInternal(SimpleLayer l,
			LayerTransformer transformer, String key, String layerName,
			SortedSet<DataTime> times, Map<String, String> dimensions)
			throws OgcException {
		String level1 = getLevel(dimensions, level1Key, l);
		String level2 = getLevel(dimensions, level2Key, l);
		String ensemble = getIntDim(dimensions, eTypeKey, l);
		String pert = getIntDim(dimensions, pertKey, l);
		String version = getIntDim(dimensions, versKey, l);
		// TODO ensure consistency in which level gets returned
		// TODO add support for more dimensions
		return query(key, layerName, times, level1, level2, ensemble, pert,
				version);
	}

	public static SimpleLayer getLayer(LayerTransformer transformer,
			String layerName) throws OgcException {
		SimpleLayer rval;
		try {
			rval = transformer.find(layerName);
		} catch (DataAccessLayerException e) {
			throw new OgcException(Code.InternalServerError, e);
		}
		if (rval == null) {
			throw new OgcException(Code.LayerNotDefined);
		}
		return rval;
	}

	protected static String getIntDim(Map<String, String> dimensions,
			String key, SimpleLayer layer) throws OgcException {
		String str = dimensions.get(key);
		String rval;
		if (str != null) {
			try {
				Integer i = Integer.parseInt(str);
				rval = i.toString();
			} catch (Exception e) {
				throw new OgcException(Code.InvalidDimensionValue, key
						+ " must be a bare integer");
			}
		} else {
			SimpleDimension dim = LayerTransformer.getDimension(layer, key);
			rval = dim.getDefaultValue(layer);
		}
		return rval;
	}

	/**
	 * @param dimensions
	 * @param key
	 * @param l
	 * @return null if level value not in dimensions and not in layer
	 */
	protected static String getLevel(Map<String, String> dimensions,
			String key, SimpleLayer layer) {
		String dimKey = new String(key).toLowerCase();
		String level = dimensions.get(dimKey);
		String rval;
		if (level != null) {
			Level l = parseLevel(level);
			rval = l.value.toString();
		} else {
			SimpleDimension dim = LayerTransformer.getDimension(layer, key);
			rval = dim.getDefaultValue(layer);
		}
		return rval;
	}

	@SuppressWarnings("unchecked")
	protected static List<GribRecord> query(String key, String layer,
			SortedSet<DataTime> times, String level1, String level2,
			String eType, String pert, String version) throws OgcException {
		Session sess = null;
		try {
			PluginDao dao = PluginFactory.getInstance().getPluginDao(key);
			SessionFactory sessFact = dao.getSessionFactory();
			sess = sessFact.openSession();
			Criteria criteria = sess.createCriteria(GribRecord.class);
			criteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
			Disjunction or = Restrictions.disjunction();
			for (DataTime dt : times) {
				String uri = getDataUri(key, layer, dt, level1, level2, eType,
						pert,
						version);
				or.add(Restrictions.like("dataURI", uri));
			}
			criteria.add(or);
			List<GribRecord> res = (List<GribRecord>) criteria.list();
			return res;
		} catch (Exception e) {
			throw new OgcException(Code.InternalServerError);
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
	}

	protected static String getDataUri(String key, String layer, DataTime dt,
			String level1, String level2, String eType, String pert,
			String version) {
		// this is based how the layer name is built in the layers table
		// FIXME this is hackish
		StringBuilder sb = new StringBuilder("/").append(key);
		sb.append('/').append(dt.toString()).append("%/");
		sb.append(layer).append('/').append(level1);
		sb.append('/').append(level2).append('/');
		sb.append((eType == null ? "null" : eType)).append('/');
		sb.append((pert == null ? "null" : pert)).append('/');
		sb.append((version == null ? "null" : version));
		return sb.toString().replaceAll(" ", "_");
	}

	protected static Level parseLevel(String level) {
		if (level == null) {
			return null;
		}
		Level rval = new Level();
		Matcher m = LayerTransformer.frontDot.matcher(level);
		if (m.matches()) {
			rval.value = Double.parseDouble(m.group(1));
			rval.units = m.group(2);
		} else {
			m = LayerTransformer.backDot.matcher(level);
			if (m.matches()) {
				rval.value = Double.parseDouble(m.group(1));
				rval.units = m.group(2);
			} else {
				rval = null;
			}
		}
		return rval;
	}

}
