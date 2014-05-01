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

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.StringUtils;


/**
 * Contains layer metadata used to populate capability and description OGC
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
public class OgcLayer {

	protected OgcLayer parent;

	protected List<OgcLayer> children;

	protected String name;

	protected String title;

	protected List<String> keywords;

	protected String abs;

	protected List<OgcStyle> styles;

	protected OgcGeoBoundingBox geoBoundingBox;

	protected List<OgcBoundingBox> boundingBox;

	protected List<String> crs;

	protected double minScaleDenom = Double.NaN;

	protected double maxScaleDenom = Double.NaN;

	protected boolean opaque;

	protected int sizeRecord = 0;

	protected List<OgcDimension> dimensions;

	public static String keySeparator = "/";

	public void addCRS(String crs) {
		this.crs = addToList(this.crs, crs);
	}

	protected <T> List<T> addToList(List<T> l, T item) {
		if (l == null) {
			l = new ArrayList<T>();
		}
		l.add(item);
		return l;
	}

	public void addBoundingBox(OgcBoundingBox bbox) {
		this.boundingBox = addToList(boundingBox, bbox);
	}

	public void addStyle(OgcStyle style) {
		this.styles = addToList(styles, style);
	}

	public void addChildLayer(OgcLayer child) {
		this.children = addToList(children, child);
	}

	public void addDimension(OgcDimension dimention) {
		this.dimensions = addToList(dimensions, dimention);
	}

	public void addKeyword(String keyword) {
		this.keywords = addToList(keywords, keyword);
	}

	public String getKey() {
		return getKey(name);
	}

	public String[] separateKey() {
		return separateKey(name);
	}

	/**
	 * @return the unique key for the source of the layer
	 */
	public static String getKey(String layerName) {
		if (layerName == null) {
			return null;
		}
		return separateKey(layerName)[0];
	}

    public static String decodeLayerName(String layerName) {
        if (layerName == null) {
            return null;
        }
        try {
            return URLDecoder.decode(layerName, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
    }

	public static String[] separateKey(String layerName) {
        String lname = decodeLayerName(layerName);
        if (lname == null) {
            return null;
        }
        lname = StringUtils.strip(lname, OgcLayer.keySeparator);
		return lname.split(OgcLayer.keySeparator, 2);
	}

	public String getFullTitle() {
		return getKey() + keySeparator + title;
	}

	public static String createName(String key, String name) {
		try {
			return URLEncoder.encode(key + keySeparator + name, "UTF-8");
		} catch (UnsupportedEncodingException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * @return the dimentions
	 */
	public List<OgcDimension> getDimensions() {
		return dimensions;
	}

	/**
	 * @param dimensions
	 *            the dimentions to set
	 */
	public void setDimensions(List<OgcDimension> dimensions) {
		this.dimensions = dimensions;
	}

	public OgcLayer getParent() {
		return parent;
	}

	public void setParent(OgcLayer parent) {
		this.parent = parent;
	}

	public List<OgcLayer> getChildren() {
		return children;
	}

	public void setChildren(List<OgcLayer> children) {
		this.children = children;
	}

	public List<OgcStyle> getStyles() {
		return styles;
	}

	public void setStyles(List<OgcStyle> styles) {
		this.styles = styles;
	}

	/**
	 * @return the geoBoundingBox
	 */
	public OgcGeoBoundingBox getGeoBoundingBox() {
		return geoBoundingBox;
	}

	/**
	 * @param geoBoundingBox
	 *            the geoBoundingBox to set
	 */
	public void setGeoBoundingBox(OgcGeoBoundingBox geoBoundingBox) {
		this.geoBoundingBox = geoBoundingBox;
	}

	public List<String> getCrs() {
		return crs;
	}

	public void setCrs(List<String> crs) {
		this.crs = crs;
	}

	/**
	 * @return the boundingBox
	 */
	public List<OgcBoundingBox> getBoundingBox() {
		return boundingBox;
	}

	/**
	 * @param boundingBox
	 *            the boundingBox to set
	 */
	public void setBoundingBox(List<OgcBoundingBox> boundingBox) {
		this.boundingBox = boundingBox;
	}

	public double getMinScaleDenom() {
		return minScaleDenom;
	}

	public void setMinScaleDenom(double minScaleDenom) {
		this.minScaleDenom = minScaleDenom;
	}

	public double getMaxScaleDenom() {
		return maxScaleDenom;
	}

	public void setMaxScaleDenom(double maxScaleDenom) {
		this.maxScaleDenom = maxScaleDenom;
	}

	public boolean isOpaque() {
		return opaque;
	}

	public void setOpaque(boolean opaque) {
		this.opaque = opaque;
	}

	public String getName() {
		return name;
	}

	/**
	 * @param key
	 *            a key that is used in all layers from the layer's source
	 * @param name
	 */
	public void setName(String key, String name) {
		this.name = createName(key, name);
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public List<String> getKeywords() {
		return keywords;
	}

	public void setKeywords(List<String> keywords) {
		this.keywords = keywords;
	}

	public String getAbs() {
		return abs;
	}

	public void setAbs(String abs) {
		this.abs = abs;
	}

	public int getSizeRecord() {
		return sizeRecord;
	}

	public void setSizeRecord(int sizeRecord) {
		this.sizeRecord = sizeRecord;
	}

}
