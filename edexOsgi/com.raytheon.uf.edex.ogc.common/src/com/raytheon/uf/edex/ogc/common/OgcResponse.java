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

import com.raytheon.uf.edex.ogc.common.http.MimeType;

/**
 * Response wrapper for OGC web services
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
public class OgcResponse {
    public static final MimeType TEXT_XML_MIME = new MimeType("text/xml");

	public static final MimeType TEXT_HTML_MIME = new MimeType("text/html");

	public static final MimeType APP_VND_OGC_SE_XML = new MimeType(
            "application/vnd.ogc.se_xml");

	public enum TYPE {
		TEXT, IMAGE, BYTE, MULTIPART
	};

	public enum ErrorType {
		NONE, BAD_REQ, INT_ERR, NOT_IMPLEMENTED
	};

    protected MimeType mimetype;

	protected Object body;

	protected boolean multipart = false;

	protected TYPE type;

	protected ErrorType error = ErrorType.NONE;

    private MimeType exceptionFormat = OgcResponse.TEXT_XML_MIME;

    public OgcResponse(Object body, MimeType mimetype, TYPE type) {
		this.body = body;
		this.mimetype = mimetype;
		this.type = type;
	}

    public MimeType getMimetype() {
		return mimetype;
	}

    public void setMimetype(MimeType mimetype) {
		this.mimetype = mimetype;
	}

	public Object getBody() {
		return body;
	}

	public void setBody(Object body) {
		this.body = body;
	}

	public boolean isMultipart() {
		return multipart;
	}

	public void setMultipart(boolean multipart) {
		this.multipart = multipart;
	}

	/**
	 * @param exceptionFormat
	 *            the exceptionFormat to set
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

	/**
	 * @return the type
	 */
	public TYPE getType() {
		return type;
	}

	/**
	 * @param type
	 *            the type to set
	 */
	public void setType(TYPE type) {
		this.type = type;
	}

	/**
	 * @return the error
	 */
	public ErrorType getError() {
		return error;
	}

	/**
	 * @param error
	 *            the error to set
	 */
	public void setError(ErrorType error) {
		this.error = error;
	}

}
