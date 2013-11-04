/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
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
