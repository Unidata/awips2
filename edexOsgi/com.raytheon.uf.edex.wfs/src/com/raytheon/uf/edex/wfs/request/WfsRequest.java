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
package com.raytheon.uf.edex.wfs.request;

import com.raytheon.uf.common.http.MimeType;
import com.raytheon.uf.edex.ogc.common.OgcResponse;

/**
 * Abstract base for WFS request wrappers
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class WfsRequest {

	public enum Type {
        GetFeature, DescribeFeature, GetCapabilities, Transaction, ERROR, ListStoredQueries, DescribeStoredQueries, GetPropertyValue
	}

    protected Object rawrequest;

	protected Type type;

	protected String username;

	protected String[] roles;

    private MimeType exceptionFormat = OgcResponse.TEXT_XML_MIME;

	public WfsRequest(Type type) {
		super();
		this.type = type;
	}

	public Type getType() {
		return type;
	}

	public void setType(Type type) {
		this.type = type;
	}

	/**
	 * @return the rawrequest
	 */
	public Object getRawrequest() {
		return rawrequest;
	}

	/**
	 * @param rawrequest
	 *            the rawrequest to set
	 */
    public void setRawrequest(Object rawrequest) {
		this.rawrequest = rawrequest;
	}

	/**
	 * @return the username
	 */
	public String getUsername() {
		return username;
	}

	/**
	 * @param username
	 *            the username to set
	 */
	public void setUsername(String username) {
		this.username = username;
	}

	/**
	 * @return the roles
	 */
	public String[] getRoles() {
		return roles;
	}

	/**
	 * @param roles
	 *            the roles to set
	 */
	public void setRoles(String[] roles) {
		this.roles = roles;
	}

	/**
	 * @param exceptionFormat the exceptionFormat to set
	 */
    public void setExceptionFormat(MimeType exceptionFormat) {
		this.exceptionFormat = exceptionFormat;
	}

	/**
	 * @return the exceptionFormat
	 */
    public MimeType getExceptionFormat() {
		return exceptionFormat;
	}

}
