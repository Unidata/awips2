/*****************************************************************************************
 * COPYRIGHT (c), 2006, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

package com.raytheon.uf.edex.wcs.request;

import com.raytheon.uf.edex.ogc.common.OgcResponse;

/**
 * TODO - Class comment here
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 
 * </pre>
 * 
 * @author
 * @version 1
 */

public class WcsRequest {

	public enum Type {
		GetCoverage, DescribeCoverage, GetCapabilities, GetData, ERROR
	}

	protected Type type;

	protected Object request;

	protected String exceptionFormat = OgcResponse.TEXT_XML_MIME;

	public WcsRequest(Type type) {
		this.type = type;
	}

	public Type getType() {
		return type;
	}

	public void setType(Type type) {
		this.type = type;
	}

	public Object getRequest() {
		return request;
	}

	public void setRequest(Object request) {
		this.request = request;
	}

	/**
	 * @return the exceptionFormat
	 */
	public String getExceptionFormat() {
		return exceptionFormat;
	}

	/**
	 * @param exceptionFormat
	 *            the exceptionFormat to set
	 */
	public void setExceptionFormat(String exceptionFormat) {
		this.exceptionFormat = exceptionFormat;
	}

}
