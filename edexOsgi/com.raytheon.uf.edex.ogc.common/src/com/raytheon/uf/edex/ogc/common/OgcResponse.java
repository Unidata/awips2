/**********************************************************************
 *
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
 **********************************************************************/
package com.raytheon.uf.edex.ogc.common;

public class OgcResponse {
	public static final String TEXT_XML_MIME = "text/xml";

	public static final String TEXT_HTML_MIME = "text/html";

	public static final String APP_VND_OGC_SE_XML = "application/vnd.ogc.se_xml";

	public enum TYPE {
		TEXT, IMAGE, BYTE, MULTIPART
	};

	public enum ErrorType {
		NONE, BAD_REQ, INT_ERR, NOT_IMPLEMENTED
	};

	protected String mimetype;

	protected Object body;

	protected boolean multipart = false;

	protected TYPE type;

	protected ErrorType error = ErrorType.NONE;

	private String exceptionFormat = OgcResponse.TEXT_XML_MIME;

	public OgcResponse(Object body, String mimetype, TYPE type) {
		this.body = body;
		this.mimetype = mimetype;
		this.type = type;
	}

	public String getMimetype() {
		return mimetype;
	}

	public void setMimetype(String mimetype) {
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
	public void setExceptionFormat(String exceptionFormat) {
		this.exceptionFormat = exceptionFormat;
	}

	/**
	 * @return the exceptionFormat
	 */
	public String getExceptionFormat() {
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
