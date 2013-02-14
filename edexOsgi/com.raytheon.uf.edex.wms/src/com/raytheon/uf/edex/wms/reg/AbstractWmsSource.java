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
 * Jul 28, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wms.reg;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.xml.bind.DatatypeConverter;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.ogc.common.OgcLayer;
import com.raytheon.uf.edex.ogc.common.OgcStyle;
import com.raytheon.uf.edex.ogc.common.StyleLookup;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer.TimeFormat;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;
import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.WmsException.Code;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class AbstractWmsSource implements WmsSource {

	private PluginDao _dao;

	protected PluginProperties props;

	protected String key;

	protected String layerTable;

	protected LayerTransformer transformer;

	protected TimeFormat timeFormat = TimeFormat.LIST;

	protected Log log = LogFactory.getLog(this.getClass());

	protected boolean layerTableIsWrapped = false;

	protected boolean wmtsCapable = true;

	public AbstractWmsSource(PluginProperties props, String key,
			LayerTransformer transformer) {
		this.props = props;
		this.key = key;
		this.transformer = transformer;
	}

	public abstract List<OgcStyle> getStyles();

	protected PluginDao getDao() throws PluginException {
		if (_dao == null) {
			_dao = PluginFactory.getInstance().getPluginDao(
					props.getPluginName());
		}
		return _dao;
	}

	protected LayerTransformer getTransformer() throws PluginException {
		return transformer;
	}

	protected abstract StyleLookup getStyleLookup() throws WmsException;

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wms.reg.WmsSource#listLayers()
	 */
	@Override
	public List<OgcLayer> listLayers() {
		try {
			LayerTransformer transformer = getTransformer();
			OgcLayer rval = new OgcLayer();
			rval.setTitle(transformer.getKey());
			StyleLookup lookup = getStyleLookup();
			rval.setChildren(transformer.getLayersAsOgc(timeFormat, lookup));
			rval.setStyles(getStyles());
			return Arrays.asList(rval);
		} catch (Exception e) {
			log.error("Unable to layers", e);
			return new ArrayList<OgcLayer>(0);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wms.reg.WmsSource#getLayer(java.lang.String)
	 */
	@Override
	public OgcLayer getLayer(String layerName) throws WmsException {
		try {
			LayerTransformer transformer = getTransformer();
			String[] parts = OgcLayer.separateKey(layerName);
			SimpleLayer layer = transformer.find(parts[1]);
			if (layer == null) {
				return null;
			}
			StyleLookup lookup = getStyleLookup();
			return transformer.transform(layer, timeFormat, lookup);
		} catch (Exception e) {
			log.error("Problem querying for layer", e);
			throw new WmsException(Code.InternalServerError);
		}
	}

	protected static Calendar parseTimeString(String time) throws WmsException {
		try {
			return DatatypeConverter.parseDateTime(time);
		} catch (Exception e) {
			throw new WmsException(Code.InvalidFormat, "Invalid Date Format");
		}
	}

	/**
	 * Get the record associated with the given layer name, time, and if
	 * applicable elevation and dimensions.
	 * <p>
	 * More elaborate datatypes will probably need to override this method to
	 * return the correct pdo for the given arguments. The default
	 * implementation simply builds a dataURI based off the given layer and
	 * time.
	 * 
	 * @param layer
	 * @param time
	 * @param elevation
	 * @param dimensions
	 * @return
	 * @throws WmsException
	 */
	protected PluginDataObject getRecord(String layer, String time,
			String elevation, Map<String, String> dimensions,
			Map<String, String> levelUnits) throws WmsException {
		Date latest;
		if (time == null) {
			latest = getDefaultDate(layer);
		} else {
			Calendar c = parseTimeString(time);
			latest = c.getTime();
		}
		String uri = buildURI(layer, latest);
		PluginDataObject rval;
		try {
			PluginDao dao = getDao();
			rval = dao.getMetadata(uri);
		} catch (PluginException e) {
			log.error("Unable to query metdata", e);
			throw new WmsException(Code.InternalServerError);
		}
		if (rval == null) {
			throw new WmsException(Code.LayerNotDefined);
		}
		return rval;
	}

	protected String parseIncomingLayerName(String rawLayer) {
		return OgcLayer.separateKey(rawLayer)[1];
	}

	/**
	 * @param layer
	 * @param time
	 * @return
	 */
	protected String buildURI(String layer, Date time) {
		String tstamp = TimeUtil.formatToSqlTimestamp(time);
		tstamp = tstamp.replaceAll(" ", "_");
		return ("/" + key + "/" + tstamp + "/" + layer);
	}

	/**
	 * Return the default date for specified layer name
	 * 
	 * @param layerName
	 * @return
	 * @throws WmsException
	 */
	protected Date getDefaultDate(String layerName) throws WmsException {
		Date rval = null;
		try {
			LayerTransformer transformer = getTransformer();
			SimpleLayer layer = transformer.find(layerName);
			rval = layer.getDefaultTime();
		} catch (DataAccessLayerException e) {
			log.error("Unable to query layers", e);
			throw new WmsException(Code.InternalServerError);
		} catch (PluginException e) {
			log.error("Unable to get plugin dao", e);
			throw new WmsException(Code.InternalServerError);
		}
		if (rval == null) {
			throw new WmsException(Code.LayerNotDefined);
		}
		return rval;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wms.reg.WmsSource#hasUpdated()
	 */
	@Override
	public boolean hasUpdated() {
		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wms.reg.WmsSource#getKey()
	 */
	@Override
	public String getKey() {
		return key;
	}

	public TimeFormat getTimeFormat() {
		return timeFormat;
	}

	public void setTimeFormat(TimeFormat timeFormat) {
		this.timeFormat = timeFormat;
	}

	public boolean isLayerTableIsWrapped() {
		return layerTableIsWrapped;
	}

	/**
	 * @return the wmtsCapable
	 */
	@Override
	public boolean isWmtsCapable() {
		return wmtsCapable;
	}

	/**
	 * @param wmsCapable
	 *            the wmsCapable to set
	 */
	public void setWmtsCapable(boolean wmtsCapable) {
		this.wmtsCapable = wmtsCapable;
	}

}
