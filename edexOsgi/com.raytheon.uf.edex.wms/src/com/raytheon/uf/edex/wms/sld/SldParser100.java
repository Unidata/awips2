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
 * Jul 14, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wms.sld;

import java.io.InputStream;
import java.util.Map;

import org.geotools.sld.SLDConfiguration;
import org.geotools.styling.StyledLayerDescriptor;
import org.geotools.xml.DOMParser;
import org.geotools.xml.Parser;
import org.w3c.dom.Document;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class SldParser100 extends AbstractSldParser {

	protected SLDConfiguration config = new SLDConfiguration();

	public SldParser100() {
		super("1.0.0");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wms.style.SldParser#parse(java.io.InputStream)
	 */
	@Override
	public StyledLayerDescriptor parse(InputStream in) throws Exception {
		return (StyledLayerDescriptor) new Parser(config).parse(in);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wms.sld.SldParser#parse(org.w3c.dom.Document)
	 */
	@Override
	public StyledLayerDescriptor parse(Document doc) throws Exception {
		Object obj = new DOMParser(config, doc).parse();
		if (obj instanceof StyledLayerDescriptor) {
			return (StyledLayerDescriptor) obj;
		} else if (obj instanceof Map) {
			Map<?, ?> m = (Map<?, ?>) obj;
			Object sldObj = m.get("StyledLayerDescriptor");
			if (sldObj == null) {
				return null;
			}
			if (sldObj instanceof StyledLayerDescriptor) {
				return (StyledLayerDescriptor) sldObj;
			}
		}
		// give up
		throw new Exception("Unable to find sld in document");
	}

}
