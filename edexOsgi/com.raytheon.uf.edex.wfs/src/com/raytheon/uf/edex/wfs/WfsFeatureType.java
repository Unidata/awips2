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
package com.raytheon.uf.edex.wfs;

import java.util.LinkedList;
import java.util.List;

import com.raytheon.uf.edex.ogc.common.OgcGeoBoundingBox;
import com.raytheon.uf.edex.wfs.request.QualifiedName;

/**
 * Feature type metadata object used for capabilities document. Independent from
 * JAXB object to support multiple WFS versions.
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
public class WfsFeatureType {

	protected QualifiedName name;

	protected String title;

	protected String abs;

	protected String defaultSRS;

	protected List<String> keywords = new LinkedList<String>();

	protected List<String> otherSRS = new LinkedList<String>();

	protected OgcGeoBoundingBox bbox;

	public WfsFeatureType(QualifiedName name, String title, String defaultSRS,
			OgcGeoBoundingBox bbox) {
		super();
		this.name = name;
		this.title = title;
		this.defaultSRS = defaultSRS;
		this.bbox = bbox;
	}

	public void addKeyword(String keyword) {
		this.keywords.add(keyword);
	}

	public void addOtherSRS(String srs) {
		this.otherSRS.add(srs);
	}

	public QualifiedName getName() {
		return name;
	}

	public void setName(QualifiedName name) {
		this.name = name;
	}

	/**
	 * @return the title
	 */
	public String getTitle() {
		return title;
	}

	/**
	 * @param title
	 *            the title to set
	 */
	public void setTitle(String title) {
		this.title = title;
	}

	/**
	 * @return the abs
	 */
	public String getAbs() {
		return abs;
	}

	/**
	 * @param abs
	 *            the abs to set
	 */
	public void setAbs(String abs) {
		this.abs = abs;
	}

	/**
	 * @return the defaultSRS
	 */
	public String getDefaultSRS() {
		return defaultSRS;
	}

	/**
	 * @param defaultSRS
	 *            the defaultSRS to set
	 */
	public void setDefaultSRS(String defaultSRS) {
		this.defaultSRS = defaultSRS;
	}

	/**
	 * @return the keywords
	 */
	public List<String> getKeywords() {
		return keywords;
	}

	/**
	 * @param keywords
	 *            the keywords to set
	 */
	public void setKeywords(List<String> keywords) {
		this.keywords = keywords;
	}

	/**
	 * @return the otherSRS
	 */
	public List<String> getOtherSRS() {
		return otherSRS;
	}

	/**
	 * @param otherSRS
	 *            the otherSRS to set
	 */
	public void setOtherSRS(List<String> otherSRS) {
		this.otherSRS = otherSRS;
	}

	/**
	 * @return the bbox
	 */
	public OgcGeoBoundingBox getBbox() {
		return bbox;
	}

	/**
	 * @param bbox
	 *            the bbox to set
	 */
	public void setBbox(OgcGeoBoundingBox bbox) {
		this.bbox = bbox;
	}

}
