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
 * Jun 16, 2011            jelkins     Initial creation
 *
 */
package com.raytheon.uf.edex.plugin.grib.ogc;

import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.plugin.grib.ogc.GribRecordFinder;
import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.WmsException.Code;
import com.raytheon.uf.edex.wms.reg.DefaultWmsSource;
import com.raytheon.uf.edex.wms.styling.ColormapStyleProvider;
import com.raytheon.uf.edex.wms.styling.CoverageStyleProvider;

/**
 * 
 * @author jelkins
 * @version 1.0
 */
public class GribWmsSource extends DefaultWmsSource {

	protected ColormapStyleProvider styler = new ColormapStyleProvider(
			"grib_style_library.xml", "Grid/Default");

	public GribWmsSource(PluginProperties props, LayerTransformer transformer)
			throws PluginException {
		super(props, props.getPluginName(), transformer);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wms.reg.DefaultWmsSource#getRecord(java.lang.String,
	 * java.lang.String, java.lang.String, java.util.Map)
	 */
	@Override
	protected PluginDataObject getRecord(String layer, String time,
			String elevation, Map<String, String> dimensions,
			Map<String, String> levelUnits) throws WmsException {
		LayerTransformer transformer;
		List<GribRecord> res;
		try {
			transformer = getTransformer();
			res = GribRecordFinder.find(transformer, key, layer, time,
					dimensions);
		} catch (OgcException e) {
			WmsException err = new WmsException(e);
			if (err.getCode().equals(Code.InternalServerError)) {
				log.error("Problem getting grib layer: " + layer);
			}
			throw err;
		} catch (PluginException e) {
			log.error("Unable to get transformer for grib", e);
			throw new WmsException(Code.InternalServerError);
		}
		if (res.isEmpty()) {
			throw new WmsException(Code.LayerNotDefined,
					"No layer matching all specified dimensions found");
		}
		if (res.size() > 1) {
			throw new WmsException(Code.InternalServerError,
					"Too many matches for criteria");
		}
		return res.get(0);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wms.reg.DefaultWmsSource#getStyleProvider(java.lang
	 * .String)
	 */
	@Override
	protected CoverageStyleProvider getStyleProvider(String layer)
			throws WmsException {
		return styler;
	}

}
