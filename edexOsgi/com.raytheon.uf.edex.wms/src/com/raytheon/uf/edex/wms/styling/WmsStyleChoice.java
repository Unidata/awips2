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
 * Aug 1, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wms.styling;

import org.geotools.styling.Style;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.edex.ogc.common.colormap.StyleRule;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class WmsStyleChoice {

	public enum TYPE {
		GT, UF, NULL
	}

	protected TYPE type;

	protected Style style;

	protected ColorMap cmap;

	protected StyleRule styleRule;

	public WmsStyleChoice(Style style) {
		this.style = style;
		this.type = (style == null ? TYPE.NULL : TYPE.GT);
	}

	public WmsStyleChoice(ColorMap cmap) {
		this.cmap = cmap;
		this.type = (cmap == null ? TYPE.NULL : TYPE.UF);
	}

	public TYPE getType() {
		return type;
	}

	public Style getStyle() {
		return style;
	}

	public ColorMap getCmap() {
		return cmap;
	}

	public StyleRule getStyleRule() {
		return styleRule;
	}

	public void setStyleRule(StyleRule styleRule) {
		this.styleRule = styleRule;
	}

}
