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
package com.raytheon.uf.edex.wcs.request;

public class GetDataRequest extends WcsRequest {

	private String group = "";

	private String dataset = "";

	protected String xOffset;

	protected String yOffset;

	protected String width;

	protected String height;

	public GetDataRequest() {
		super(Type.GetData);
	}

	public String getGroup() {
		return group;
	}

	public void setGroup(String group) {
		this.group = group;
	}

	public String getDataset() {
		return dataset;
	}

	public void setDataset(String dataset) {
		this.dataset = dataset;
	}

	public String getxOffset() {
		return xOffset;
	}

	public void setxOffset(String xOffset) {
		this.xOffset = xOffset;
	}

	public String getyOffset() {
		return yOffset;
	}

	public void setyOffset(String yOffset) {
		this.yOffset = yOffset;
	}

	public String getWidth() {
		return width;
	}

	public void setWidth(String width) {
		this.width = width;
	}

	public String getHeight() {
		return height;
	}

	public void setHeight(String height) {
		this.height = height;
	}

	public int[] getMinIndex() {
		int[] minIndex = { 0, 0 };

		minIndex[0] = Integer.parseInt(xOffset);
		minIndex[1] = Integer.parseInt(yOffset);

		return minIndex;
	}

	public int[] getMaxIndex() {
		int[] maxIndex = { 0, 0 };

		maxIndex[0] = Integer.parseInt(xOffset) + Integer.parseInt(width);
		maxIndex[1] = Integer.parseInt(yOffset) + Integer.parseInt(height);

		return maxIndex;
	}
}
