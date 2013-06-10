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
 * Aug 4, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wms;

import com.raytheon.uf.edex.ogc.common.OgcResponse;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class GetFeatureInfoRequest extends GetMapRequest {

	protected String[] reqLayers;

	protected int featureCount = 1;

	protected Integer i;

	protected Integer j;

	protected String infoFormat;

	public GetFeatureInfoRequest() {
	}

	public GetFeatureInfoRequest(GetMapRequest mapRequest, String[] reqLayers,
			Integer i, Integer j, String infoFormat) {
		super(mapRequest);
		this.reqLayers = reqLayers;
		this.i = i;
		this.j = j;
		this.infoFormat = infoFormat;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wms.BaseRequest#execute(com.raytheon.uf.edex.wms
	 * .WmsProvider)
	 */
	@Override
	public OgcResponse execute(WmsProvider provider) {
		return provider.getFeatureInfo(this);
	}

	public GetMapRequest getMapRequest() {
		return this;
	}

	public int getFeatureCount() {
		return featureCount;
	}

	public void setFeatureCount(int featureCount) {
		this.featureCount = featureCount;
	}

	public Integer getI() {
		return i;
	}

	public void setI(Integer i) {
		this.i = i;
	}

	public Integer getJ() {
		return j;
	}

	public void setJ(Integer j) {
		this.j = j;
	}

	/**
	 * @return the reqLayers
	 */
	public String[] getReqLayers() {
		return reqLayers;
	}

	/**
	 * @param reqLayers
	 *            the reqLayers to set
	 */
	public void setReqLayers(String[] reqLayers) {
		this.reqLayers = reqLayers;
	}

	public String getInfoFormat() {
		return infoFormat;
	}

	public void setInfoFormat(String infoFormat) {
		this.infoFormat = infoFormat;
	}

}
