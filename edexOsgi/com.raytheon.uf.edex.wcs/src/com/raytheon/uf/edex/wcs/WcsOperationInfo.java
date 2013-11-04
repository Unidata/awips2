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
package com.raytheon.uf.edex.wcs;

import java.util.LinkedList;
import java.util.List;

public class WcsOperationInfo {

	public enum Type {
		GetCapabilities, GetCoverage, DescribeCoverage
	}

	protected Type type;
	
	protected String httpPostRes;

	protected String httpGetRes;

	protected List<String> formats = new LinkedList<String>();

	/**
	 * 
	 */
	public WcsOperationInfo(Type type) {
		this.type = type;
	}

	public WcsOperationInfo(Type type, String httpPostRes, String httpGetRes) {
		this(type);
		this.httpGetRes = httpGetRes;
		this.httpPostRes = httpPostRes;
	}

	public void addFormat(String format) {
		formats.add(format);
	}

	public boolean hasHttpPost() {
		return httpPostRes != null;
	}

	public boolean hasHttpGet() {
		return httpGetRes != null;
	}

	/**
	 * @return the httpPostRes
	 */
	public String getHttpPostRes() {
		return httpPostRes;
	}

	/**
	 * @param httpPostRes
	 *            the httpPostRes to set
	 */
	public void setHttpPostRes(String httpPostRes) {
		this.httpPostRes = httpPostRes;
	}

	/**
	 * @return the httpGetRes
	 */
	public String getHttpGetRes() {
		return httpGetRes;
	}

	/**
	 * @param httpGetRes
	 *            the httpGetRes to set
	 */
	public void setHttpGetRes(String httpGetRes) {
		this.httpGetRes = httpGetRes;
	}

	/**
	 * @return the type
	 */
	public Type getType() {
		return type;
	}

	/**
	 * @param type
	 *            the type to set
	 */
	public void setType(Type type) {
		this.type = type;
	}

	/**
	 * @return the formats
	 */
	public List<String> getFormats() {
		return formats;
	}

	/**
	 * @param formats
	 *            the formats to set
	 */
	public void setFormats(List<String> formats) {
		this.formats = formats;
	}
}
