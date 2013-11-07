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
package com.raytheon.uf.edex.ogc.common;

/**
 * Contains style metadata used to populate capability and description OGC
 * documents. Separate from OGC JAXB objects to support different versions of
 * OGC services.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class OgcStyle {

	protected String name;

	protected String title;

	protected String abs;

	protected String legendUrl;

	protected boolean isDefault = false;

	public OgcStyle() {
	}

	public OgcStyle(String name, String title, String abs) {
		this.name = name;
		this.title = title;
		this.abs = abs;
	}

	public OgcStyle(String name, String title) {
		this(name, title, null);
	}

	public OgcStyle(String name) {
		this(name, null, null);
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getAbs() {
		return abs;
	}

	public void setAbs(String abs) {
		this.abs = abs;
	}

	/**
	 * @return the legendUrl
	 */
	public String getLegendUrl() {
		return legendUrl;
	}

	/**
	 * @param legendUrl
	 *            the legendUrl to set
	 */
	public void setLegendUrl(String legendUrl) {
		this.legendUrl = legendUrl;
	}

	/**
	 * @return the isDefault
	 */
	public boolean isDefault() {
		return isDefault;
	}

	/**
	 * @param isDefault
	 *            the isDefault to set
	 */
	public void setDefault(boolean isDefault) {
		this.isDefault = isDefault;
	}

}
