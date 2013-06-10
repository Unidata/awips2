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
* Mar 30, 2012            bclement     Initial creation
*
*/ 
package com.raytheon.uf.edex.wms.styling;

import java.awt.image.BufferedImage;
import java.util.Map;

import org.geotools.data.memory.MemoryFeatureCollection;
import org.geotools.styling.Style;

import com.raytheon.uf.edex.ogc.common.OgcStyle;
import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.reg.WmsImage;

/**
 * TODO Add Description
 *
 * @author bclement
 * @version 1.0	
 */
public class SingleFeatureStyleProvider extends SingleStyleProvider implements
		IFeatureStyleProvider {

	/**
	 * @param style
	 */
	public SingleFeatureStyleProvider(Style style) {
		super(style);
	}

	/**
	 * @param styleInfo
	 * @param style
	 */
	public SingleFeatureStyleProvider(OgcStyle styleInfo, Style style) {
		super(styleInfo, style);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wms.styling.IFeatureStyleProvider#styleData(org.
	 * geotools.data.collection.ListFeatureCollection, java.lang.String,
	 * java.lang.String, boolean)
	 */
	@Override
	public WmsImage styleData(MemoryFeatureCollection coll, String layer,
			String style, boolean defaultStyle) throws WmsException {
		return new WmsImage(coll, this.style);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wms.styling.CoverageStyleProvider#getLegend(java
	 * .lang.String, java.lang.String, java.util.Map, java.util.Map, int, int)
	 */
	@Override
	public BufferedImage getLegend(String layer, String style,
			Map<String, String> dimensions, Map<String, String> levelUnits,
			Integer width, Integer height) throws WmsException {
		return AbstractSldStyleProvider.getLegend(this.style, width, height);
	}


}
