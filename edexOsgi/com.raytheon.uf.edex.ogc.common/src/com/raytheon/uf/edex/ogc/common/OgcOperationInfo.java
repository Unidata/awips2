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
/**
 * 
 */
package com.raytheon.uf.edex.ogc.common;

import java.util.LinkedList;
import java.util.List;


/**
 * Contains operations metadata used to populated OGC capabilities documents.
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
 * @param <T>
 */
public class OgcOperationInfo<T> {

	protected T type;

	protected String httpPostRes;

	protected String httpGetRes;
	
	protected String postEncoding;

    protected String httpBaseHostname;

	protected List<String> formats = new LinkedList<String>();

	protected List<String> versions = new LinkedList<String>();
	
	protected List<String> acceptversions = new LinkedList<String>();

	protected List<String> services = new LinkedList<String>();
	
	protected List<String> identifiers = new LinkedList<String>();
	
	protected List<String> interpolationtypes = new LinkedList<String>();
	
	/**
	 * 
	 */
	public OgcOperationInfo(T type) {
		this.type = type;
	}

    public OgcOperationInfo(T type, String httpPostRes, String httpGetRes,
            String httpBaseHostname) {
		this(type);
		this.httpGetRes = httpGetRes;
		this.httpPostRes = httpPostRes;
        this.httpBaseHostname = httpBaseHostname;
	}

	public void addFormat(String format) {
		formats.add(format);
	}

	public void addVersion(String version) {
		versions.add(version);
	}

	public void addAcceptVersions(String version) {
		acceptversions.add(version);
	}
	
	public void addService(String service) {
		services.add(service);
	}
	
	public void addIdentifier(String identifier) {
		identifiers.add(identifier);
	}
	
	public void addInterpolationType(String interpolationtype) {
		interpolationtypes.add(interpolationtype);
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
	public T getType() {
		return type;
	}

	/**
	 * @param type
	 *            the type to set
	 */
	public void setType(T type) {
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

	/**
	 * @return the versions
	 */
	public List<String> getVersions() {
		return versions;
	}

	/**
	 * @param versions
	 *            the versions to set
	 */
	public void setVersions(List<String> versions) {
		this.versions = versions;
	}

	public List<String> getAcceptversions() {
		return acceptversions;
	}

	public void setAcceptversions(List<String> acceptversions) {
		this.acceptversions = acceptversions;
	}

	public List<String> getServices() {
		return services;
	}

	public void setServices(List<String> services) {
		this.services = services;
	}

	public List<String> getIdentifiers() {
		return identifiers;
	}

	public void setIdentifiers(List<String> identifiers) {
		this.identifiers = identifiers;
	}

	public List<String> getInterpolationtypes() {
		return interpolationtypes;
	}

	public void setInterpolationtypes(List<String> interpolationtypes) {
		this.interpolationtypes = interpolationtypes;
	}

    /**
     * @return the postEncoding
     */
    public String getPostEncoding() {
        return postEncoding;
    }

    /**
     * @param postEncoding
     *            the postEncoding to set
     */
    public void setPostEncoding(String postEncoding) {
        this.postEncoding = postEncoding;
    }

    public String getHttpBaseHostname() {
        return httpBaseHostname;
    }

    public void setHttpBaseHostname(String httpBaseHostname) {
        this.httpBaseHostname = httpBaseHostname;
    }

}
