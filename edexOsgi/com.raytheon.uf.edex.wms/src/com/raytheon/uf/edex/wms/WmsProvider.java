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
package com.raytheon.uf.edex.wms;

import java.io.InputStream;

import com.raytheon.uf.edex.ogc.common.OgcResponse;

/**
 * @author bclement
 * 
 */
public interface WmsProvider {

	public static final String wmsName = "wms";

	public enum WmsOpType {
		GetCapabilities, GetMap, GetFeatureInfo, GetLegendGraphic
	}

	public OgcResponse getCapabilities(BaseRequest<WmsOpType> req);

	public OgcResponse getMap(GetMapRequest req);

	public OgcResponse getError(WmsException e, String exceptionFormat);

	public OgcResponse handlePost(InputStream in);

	public OgcResponse getFeatureInfo(GetFeatureInfoRequest req);

	public OgcResponse getLegendGraphic(GetLegendGraphicRequest req);
}
