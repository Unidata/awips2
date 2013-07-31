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
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.xml.bind.DatatypeConverter;

import org.apache.commons.lang.StringUtils;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcLayer;
import com.raytheon.uf.edex.ogc.common.OgcStyle;
import com.raytheon.uf.edex.ogc.common.StyleLookup;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer.TimeFormat;
import com.raytheon.uf.edex.ogc.common.db.SimpleDimension;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;
import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.WmsException.Code;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class AbstractWmsSource<D extends SimpleDimension, L extends SimpleLayer<D>>
        implements WmsSource {

	private PluginDao _dao;

	protected PluginProperties props;

	protected String key;

	protected String layerTable;

    protected LayerTransformer<D, L> transformer;

	protected TimeFormat timeFormat = TimeFormat.LIST;

	protected IUFStatusHandler log = UFStatus.getHandler(this.getClass());

	protected boolean layerTableIsWrapped = false;

	protected boolean wmtsCapable = true;

	public AbstractWmsSource(PluginProperties props, String key,
            LayerTransformer<D, L> transformer) {
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

    protected LayerTransformer<D, L> getTransformer() throws PluginException {
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
            LayerTransformer<?, ?> transformer = getTransformer();
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
            LayerTransformer<D, L> transformer = getTransformer();
			String[] parts = OgcLayer.separateKey(layerName);
            L layer = transformer.find(parts[1]);
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

	protected static Date stringToDate(String time) throws WmsException {
		try {
			return DatatypeConverter.parseDateTime(time).getTime();
		} catch (Exception e) {
			throw new WmsException(Code.InvalidFormat, "Invalid Date Format");
		}
	}

	protected Date parseTimeInstance(String time, String layer)
			throws WmsException {
		Date[] times = parseTimeString(time);
		if (times.length > 1) {
			// ranges not supported
			String lname = OgcLayer.createName(key, layer);
			String msg = String.format(
					"Layer '%s' does not support time ranges", lname);
			throw new WmsException(Code.InvalidParameterValue, msg);
		}
		return times[0];
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
			Map<String, String> levelUnits)
			throws WmsException {
		Date targetDate;
		if (time == null) {
			targetDate = getDefaultDate(layer);
		} else {
			targetDate = parseTimeInstance(time, layer);
		}
		String uri = buildURI(layer, targetDate);
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

	/**
	 * @param time
	 * @return array with one entry if instance. If string is time range,
	 *         returned array will have range start at index 0 and range end at
	 *         index 1
	 * @throws WmsException
	 */
	protected Date[] parseTimeString(String time) throws WmsException {
		String[] parts = StringUtils.split(time, '/');
		Date[] rval;
		try {
			if (parts.length == 1) {
				// instance
				rval = new Date[] { stringToDate(parts[0]) };
			} else {
				// range
				Date start = stringToDate(parts[0]);
				Date end = stringToDate(parts[1]);
				// TODO check resolution
				rval = new Date[] { start, end };
			}
		} catch (IllegalArgumentException e) {
			// assume malformed time
			throw new WmsException(Code.InvalidParameterValue,
					"Invalid time string: " + time);
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
            LayerTransformer<D, L> transformer = getTransformer();
            L layer = transformer.find(layerName);
			if (layer != null) {
				rval = layer.getDefaultTime();
			}
		} catch (OgcException e) {
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
