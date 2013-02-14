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

import java.util.Arrays;
import java.util.List;

import org.geotools.styling.Style;

import com.raytheon.uf.edex.ogc.common.OgcStyle;
import com.raytheon.uf.edex.ogc.common.StyleLookup;

/**
 *
 * @author bclement
 * @version 1.0	
 */
public abstract class SingleStyleProvider implements StyleLookup {

	protected OgcStyle styleInfo;

	protected Style style;

	public SingleStyleProvider(Style style) {
		this(new OgcStyle("default"), style);
	}

	public SingleStyleProvider(OgcStyle styleInfo, Style style) {
		this.styleInfo = styleInfo;
		this.style = style;
	}

	/* (non-Javadoc)
	 * @see com.raytheon.uf.edex.ogc.common.StyleLookup#lookup(java.lang.String)
	 */
	@Override
	public String lookup(String layername) {
		return styleInfo.getName();
	}

	/* (non-Javadoc)
	 * @see com.raytheon.uf.edex.ogc.common.StyleLookup#getStyles()
	 */
	@Override
	public List<OgcStyle> getStyles() {
		return Arrays.asList(styleInfo);
	}

	/* (non-Javadoc)
	 * @see com.raytheon.uf.edex.wms.styling.CoverageStyleProvider#setLoader(java.lang.ClassLoader)
	 */
	@Override
	public void setLoader(ClassLoader loader) {
		// this class doesn't look anything up on the resource path
	}

}
