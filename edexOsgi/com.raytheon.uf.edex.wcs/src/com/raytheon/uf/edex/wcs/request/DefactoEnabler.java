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
package com.raytheon.uf.edex.wcs.request;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.camel.Exchange;
import org.apache.camel.Message;
import org.apache.camel.Processor;
import org.apache.camel.component.http.HttpMessage;

/**
 * TODO Add Description
 * 
 * @author jelkins
 * @version 1.0
 */
public class DefactoEnabler implements Processor {

	public static final String HEADER_KEY = "replyStandard";
	public static final String HEADER_VALUE = "defacto";

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.apache.camel.Processor#process(org.apache.camel.Exchange)
	 */
	@Override
	public void process(Exchange exchange) throws Exception {
		Message in = exchange.getIn();
		in.getHeaders().put(HEADER_KEY, HEADER_VALUE);
		exchange.getIn().setBody(
				new HttpMessage(exchange, in.getBody(HttpServletRequest.class),
						in.getBody(HttpServletResponse.class)));
	}
}
