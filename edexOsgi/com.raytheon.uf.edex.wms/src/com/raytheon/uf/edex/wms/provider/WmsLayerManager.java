/**********************************************************************
 *
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
 **********************************************************************/
/**
 * 
 */
package com.raytheon.uf.edex.wms.provider;

import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.opengis.wms.v_1_3_0.BoundingBox;
import net.opengis.wms.v_1_3_0.Dimension;
import net.opengis.wms.v_1_3_0.EXGeographicBoundingBox;
import net.opengis.wms.v_1_3_0.Keyword;
import net.opengis.wms.v_1_3_0.KeywordList;
import net.opengis.wms.v_1_3_0.Layer;
import net.opengis.wms.v_1_3_0.LegendURL;
import net.opengis.wms.v_1_3_0.OnlineResource;
import net.opengis.wms.v_1_3_0.Style;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.edex.ogc.common.OgcBoundingBox;
import com.raytheon.uf.edex.ogc.common.OgcDimension;
import com.raytheon.uf.edex.ogc.common.OgcGeoBoundingBox;
import com.raytheon.uf.edex.ogc.common.OgcLayer;
import com.raytheon.uf.edex.ogc.common.OgcOperationInfo;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.ogc.common.OgcStyle;
import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.WmsException.Code;
import com.raytheon.uf.edex.wms.WmsProvider.WmsOpType;
import com.raytheon.uf.edex.wms.reg.WmsSource;
import com.raytheon.uf.edex.wms.reg.WmsSourceAccessor;

/**
 * @author bclement
 * 
 */
public class WmsLayerManager {

	protected HashMap<String, List<Layer>> layermap = new HashMap<String, List<Layer>>();

	protected WmsSourceAccessor registry = new WmsSourceAccessor();

	protected boolean caching = false;

	protected String componentPrefix = "wms";

	private OgcOperationInfo<WmsOpType> _getLegendOp = null;

	protected Log log = LogFactory.getLog(this.getClass());

	public WmsSource getSource(String layer) throws WmsException {
		return getSource(layer, null, null);
	}

	public WmsSource getSource(String layer, String username, Set<String> roles)
			throws WmsException {
		String key = OgcLayer.getKey(layer);
		WmsSource rval = registry.getSources().get(key);
		if (rval != null && username != null) {
			// if (!OgcAuthUtils.isAuthorized(roles, getSourceRoles(key))) {
			// // act like the layer doesn't exist
			// rval = null;
			// }
		}
		if (rval == null) {
			throw new WmsException(Code.LayerNotDefined,
					"No layer registered: " + layer);
		}

		return rval;
	}

	public List<Layer> getLayers(OgcServiceInfo<WmsOpType> ogcServiceInfo) {
		if (caching) {
			return getLayersCache(ogcServiceInfo);
		} else {
			return getLayersDirect(ogcServiceInfo);
		}
	}

	protected List<Layer> getLayersCache(
			OgcServiceInfo<WmsOpType> ogcServiceInfo) {
		List<Layer> rval = new LinkedList<Layer>();
		Map<String, WmsSource> sources = registry.getSources();
		for (String key : sources.keySet()) {
			WmsSource source = sources.get(key);
			List<Layer> layers;
			if (layermap.isEmpty() || source.hasUpdated()) {
				layers = getPluginLayers(source, ogcServiceInfo);
				layermap.put(key, layers);
			} else {
				layers = layermap.get(key);
			}
			rval.addAll(layers);
		}
		return rval;
	}

	protected List<Layer> getLayersDirect(
			OgcServiceInfo<WmsOpType> ogcServiceInfo) {
		List<Layer> rval = new LinkedList<Layer>();
		Map<String, WmsSource> sources = registry.getSources();
		for (WmsSource source : sources.values()) {
			rval.addAll(getPluginLayers(source, ogcServiceInfo));
		}
		return rval;
	}

	/**
	 * @param source
	 * @param ogcServiceInfo
	 * @return
	 */
	protected List<Layer> getPluginLayers(WmsSource source,
			OgcServiceInfo<WmsOpType> ogcServiceInfo) {
		List<Layer> rval = new LinkedList<Layer>();
		for (OgcLayer layer : source.listLayers()) {
			rval.add(getLayerRecursive(layer, ogcServiceInfo));
		}
		return rval;
	}

	/**
	 * @param layer
	 * @param ogcServiceInfo
	 * @return
	 */
	protected Layer getLayerRecursive(OgcLayer layer,
			OgcServiceInfo<WmsOpType> ogcServiceInfo) {
		Layer rval = new Layer();
		rval.setTitle(layer.getTitle());
		rval.setName(layer.getName());
		rval.setAbstract(layer.getAbs());
		rval.setKeywordList(getAsKeywordList(layer.getKeywords()));
		List<String> crs = layer.getCrs();
		if (crs != null) {
			rval.setCRS(crs);
		}
		rval.setEXGeographicBoundingBox(getExBB(layer.getGeoBoundingBox()));
		rval.setBoundingBox(getBB(layer.getBoundingBox()));
		rval.setDimension(getDimensions(layer.getDimensions()));
		rval.setStyle(getAsStyles(layer.getStyles(), ogcServiceInfo));
		setScaleDenom(rval, layer.getMinScaleDenom(), layer.getMaxScaleDenom());
		List<Layer> children = new LinkedList<Layer>();
		if (layer.getChildren() != null) {
			for (OgcLayer child : layer.getChildren()) {
				children.add(getLayerRecursive(child, ogcServiceInfo));
			}
		}
		rval.setLayer(children);
		return rval;
	}

	protected void setScaleDenom(Layer layer, double min, double max) {
		if (!Double.isNaN(min)) {
			layer.setMinScaleDenominator(min);
		}
		if (!Double.isNaN(max)) {
			layer.setMaxScaleDenominator(max);
		}
	}

	/**
	 * @param styles
	 * @param ogcServiceInfo
	 * @return
	 */
	private List<Style> getAsStyles(List<OgcStyle> styles, OgcServiceInfo<WmsOpType> ogcServiceInfo) {
		List<Style> rval = new LinkedList<Style>();
		if (styles != null) {
			for (OgcStyle from : styles) {
				Style to = new Style();
				to.setAbstract(from.getAbs());
				to.setName(from.getName());
				to.setTitle(from.getTitle());
				List<LegendURL> legendUrls = getLegendUrls(from, ogcServiceInfo);
				if (legendUrls != null && !legendUrls.isEmpty()) {
					to.setLegendURL(legendUrls);
				}
				rval.add(to);
			}
		}
		return rval;
	}

	private List<LegendURL> getLegendUrls(OgcStyle from,
			OgcServiceInfo<WmsOpType> ogcServiceInfo) {
		String legendUrl = from.getLegendUrl();
		if (legendUrl == null) {
			return null;
		}
		OgcOperationInfo<WmsOpType> getLegend = getLegendOp(ogcServiceInfo);
		if (getLegend == null) {
			return null;
		}
		String url = getLegend.getHttpGetRes() + legendUrl;
		LegendURL rval = new LegendURL();
		OnlineResource or = new OnlineResource();
		or.setHref(url);
		rval.setOnlineResource(or);
		return Arrays.asList(rval);
	}

	private OgcOperationInfo<WmsOpType> getLegendOp(
			OgcServiceInfo<WmsOpType> ogcServiceInfo) {
		if (_getLegendOp == null) {
			List<OgcOperationInfo<WmsOpType>> ops = ogcServiceInfo
					.getOperations();
			for (OgcOperationInfo<WmsOpType> op : ops) {
				if (op.getType().equals(WmsOpType.GetLegendGraphic)) {
					_getLegendOp = op;
					break;
				}
			}
		}
		return _getLegendOp;
	}

	/**
	 * @param dimentions
	 * @return
	 */
	protected List<Dimension> getDimensions(List<OgcDimension> dimensions) {
		List<Dimension> rval = new LinkedList<Dimension>();
		if (dimensions != null) {
			for (OgcDimension from : dimensions) {
				Dimension to = new Dimension();
				to.setName(from.getName());
				to.setUnits(from.getUnits());
				to.setUnitSymbol(from.getUnitSymbol());
				List<String> values = from.getValues();
				to.setValue(StringUtils.join(values, ","));
				to.setDefault(from.getDefaultVal());
				rval.add(to);
			}
		}
		return rval;
	}

	/**
	 * @param bblist
	 * @return
	 */
	protected List<BoundingBox> getBB(List<OgcBoundingBox> bblist) {
		List<BoundingBox> rval = new LinkedList<BoundingBox>();
		if (bblist != null) {
			for (OgcBoundingBox from : bblist) {
				BoundingBox to = new BoundingBox();
				to.setCRS(from.getCrs());
				to.setMaxx(from.getMaxx());
				to.setMaxy(from.getMaxy());
				to.setMinx(from.getMinx());
				to.setMiny(from.getMiny());
				setXYRes(to, from.getResx(), from.getResy());
				rval.add(to);
			}
		}
		return rval;
	}

	protected void setXYRes(BoundingBox bb, double xres, double yres) {
		if (!Double.isNaN(xres)) {
			bb.setResx(xres);
		}
		if (!Double.isNaN(yres)) {
			bb.setResy(yres);
		}
	}

	/**
	 * @param geographicBoundingBox
	 * @return
	 */
	protected EXGeographicBoundingBox getExBB(OgcGeoBoundingBox ogcGBB) {
		if (ogcGBB == null) {
			return null;
		}
		EXGeographicBoundingBox rval = new EXGeographicBoundingBox();
		rval.setEastBoundLongitude(ogcGBB.getMaxx());
		rval.setNorthBoundLatitude(ogcGBB.getMaxy());
		rval.setSouthBoundLatitude(ogcGBB.getMiny());
		rval.setWestBoundLongitude(ogcGBB.getMinx());
		return rval;
	}

	protected KeywordList getAsKeywordList(List<String> keywords) {
		KeywordList rval = new KeywordList();
		List<Keyword> klist = new LinkedList<Keyword>();
		if (keywords != null) {
			for (String keyword : keywords) {
				Keyword kw = new Keyword();
				kw.setValue(keyword);
				klist.add(kw);
			}
		}
		rval.setKeyword(klist);
		return rval;
	}

	public boolean isCaching() {
		return caching;
	}

	public void setCaching(boolean caching) {
		this.caching = caching;
	}

	/**
	 * @return the componentPrefix
	 */
	public String getComponentPrefix() {
		return componentPrefix;
	}

	/**
	 * @param componentPrefix
	 *            the componentPrefix to set
	 */
	public void setComponentPrefix(String componentPrefix) {
		this.componentPrefix = componentPrefix;
	}

}
