/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.apache.qpid.management.servlet;

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.muse.util.xml.XmlUtils;
import org.apache.qpid.management.Messages;
import org.apache.qpid.management.wsdm.muse.engine.WSDMAdapterIsolationLayer;
import org.apache.qpid.qman.debug.XmlDebugger;
import org.apache.qpid.transport.util.Logger;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 * QMan Adapter facade.
 * This is the main requestor entry point of the WS-DM connector / adapter.
 * All WSDM requests will be handled (at higher level) by this servlet.
 * 
 * @author Andrea Gazzarini
 */
public class WSDMAdapter extends HttpServlet 
{
	private static final long serialVersionUID = 6149614872902682208L;
	private final static Logger LOGGER = Logger.get(WSDMAdapter.class);
	private WSDMAdapterIsolationLayer _isolationLayer;
	
	@Override
	public void init() throws ServletException {
        LOGGER.debug(Messages.QMAN_000026_WSDM_ADAPTER_STARTS);
        
        _isolationLayer = new WSDMAdapterIsolationLayer(getServletContext());
        _isolationLayer.initialize();
        
        LOGGER.debug(Messages.QMAN_000027_WSDM_ADAPTER_STARTED);
        
 	}
	
	/**
	 * Accepts http requests containing a soap envelope (request) and therefore
	 * acts as the main entry point of whole WS process.
	 * 
	 * @param request the http request.
	 * @param response the http response.
	 * @throws ServletException in case of application failure.
	 * @throws IOException in case of generic I/O failure.
	 */
	@Override
	protected void doPost(HttpServletRequest request,HttpServletResponse response) throws ServletException,IOException 
	{		
		PrintWriter writer = response.getWriter();

		Document soapEnvelopeRequest = null;
		String soapEnvelopeResposeAsString = null;
		
		try 
		{
			soapEnvelopeRequest = XmlUtils.createDocument(request.getInputStream());
			
			Document soapEnvelopeResponse = _isolationLayer.handleRequest(soapEnvelopeRequest);
			soapEnvelopeResposeAsString = XmlUtils.toString(soapEnvelopeResponse,false,true);
			
			response.setContentType("text/xml");
			
			writer.write(soapEnvelopeResposeAsString);
		} catch (SAXException exception) 
		{
			LOGGER.error(
					exception,
					Messages.QMAN_100019_REQ_OR_RES_MALFORMED);
			throw new ServletException(exception);
		} finally {
			writer.flush();	
			
			XmlDebugger.debug(soapEnvelopeRequest);
			try {
				XmlDebugger.debug(soapEnvelopeResposeAsString);
			} catch(Exception exception) {
				LOGGER.error(
						exception,
						Messages.QMAN_100019_REQ_OR_RES_MALFORMED);
			}
		}
	}
}