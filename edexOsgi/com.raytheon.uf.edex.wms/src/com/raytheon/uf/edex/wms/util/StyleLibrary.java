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
package com.raytheon.uf.edex.wms.util;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.geotools.styling.NamedLayer;
import org.geotools.styling.Style;
import org.geotools.styling.StyledLayer;
import org.geotools.styling.StyledLayerDescriptor;

/**
 * TODO Add Description
 * 
 * @author bclement
 * @version 1.0
 */
public class StyleLibrary {

	protected Map<String, Style> layerDefaults;

	protected Map<String, Style> allStyles;

	public StyleLibrary(StyledLayerDescriptor sld) {
		StyledLayer[] layers = sld.getStyledLayers();
		layerDefaults = new HashMap<String, Style>(layers.length);
		allStyles = new HashMap<String, Style>();
		for (StyledLayer sl : layers) {
			NamedLayer layer = (NamedLayer) sl;
			Style[] styles = layer.getStyles();
			if (styles.length == 1) {
				layerDefaults.put(layer.getName(), styles[0]);
			}
			for (Style s : styles) {
				if (s.isDefault()) {
					layerDefaults.put(layer.getName(), s);
				}
				allStyles.put(s.getName(), s);
			}
		}
	}

	public Style getAny() {
		if (allStyles.isEmpty()) {
			return null;
		}
		return allStyles.values().iterator().next();
	}

	public Set<String> getAllStyles() {
		return allStyles.keySet();
	}

	public Style getDefault(String layer) {
		return layerDefaults.get(layer);
	}

	public Style getNamedStyle(String name) {
		return allStyles.get(name);
	}
}
