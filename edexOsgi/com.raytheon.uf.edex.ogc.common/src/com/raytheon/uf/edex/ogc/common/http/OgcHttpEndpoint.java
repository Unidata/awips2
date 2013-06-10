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

package com.raytheon.uf.edex.ogc.common.http;

import java.io.InputStream;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;

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
public class OgcHttpEndpoint implements Processor {

	protected OgcHttpPool pool;

	/**
	 * 
	 */
	public OgcHttpEndpoint(OgcHttpPool pool) {
		this.pool = pool;
	}

	@Override
	public void process(Exchange ex) throws Exception {

		Map<String, Object> headers = ex.getIn().getHeaders();

		HttpServletResponse response = ex.getIn().getBody(
				HttpServletResponse.class);
		HttpServletRequest httpRequest = ex.getIn().getBody(
				HttpServletRequest.class);

		long id = Thread.currentThread().getId();
		OgcHttpHandler handler = (OgcHttpHandler) pool.borrowObject(id);
		OgcHttpRequest ogcReq = new OgcHttpRequest(httpRequest, response,
				headers);
		if (httpRequest.getMethod().equalsIgnoreCase("POST")) {
			ogcReq.setInputStream(ex.getIn().getBody(InputStream.class));
		}
		handler.handle(ogcReq);
		pool.returnObject(id, handler);
	}

}
