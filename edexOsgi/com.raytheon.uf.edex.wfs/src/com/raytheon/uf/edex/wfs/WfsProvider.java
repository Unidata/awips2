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
 * Apr 22, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs;

import java.io.InputStream;

import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.wfs.request.DescFeatureTypeReq;
import com.raytheon.uf.edex.wfs.request.GetCapReq;
import com.raytheon.uf.edex.wfs.request.GetFeatureReq;
import com.raytheon.uf.edex.wfs.request.TransReq;
import com.raytheon.uf.edex.wfs.request.WfsRequest;

public interface WfsProvider {

	public enum WfsOpType {
		GetCapabilities, GetFeature, DescribeFeatureType
	}

	public OgcResponse getCapabilities(GetCapReq request,
			OgcServiceInfo<WfsOpType> serviceInfo);

	public OgcResponse describeFeatureType(DescFeatureTypeReq request,
			OgcServiceInfo<WfsOpType> serviceInfo);

	public OgcResponse getFeature(GetFeatureReq request,
			OgcServiceInfo<WfsOpType> serviceInfo);

	public WfsRequest getRequest(InputStream in);

	public OgcResponse getError(WfsException e, String exceptionFormat);

	/**
	 * @param request
	 * @return
	 */
	public OgcResponse transaction(TransReq request);
}
