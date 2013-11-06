/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.soap;

import java.util.Arrays;

import javax.servlet.http.HttpServletRequest;
import javax.xml.ws.WebServiceContext;
import javax.xml.ws.handler.MessageContext;

import net.opengis.ows.v_1_1_0.ExceptionReport;
import net.opengis.ows.v_1_1_0.ExceptionType;

import com.raytheon.uf.edex.ogc.common.http.EndpointInfo;

/**
 * Abstract base class for OGC web services. Provides common utility methods.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 31, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class AbstractOwsService {

    protected abstract WebServiceContext getContext();

    protected EndpointInfo getInfo() {
        MessageContext mc = getContext().getMessageContext();
        HttpServletRequest req = (HttpServletRequest) mc
                .get(MessageContext.SERVLET_REQUEST);
        EndpointInfo rval = new EndpointInfo(req.getServerName(),
                req.getServerPort(), req.getPathInfo());
        rval.setEncoding("SOAP");
        return rval;
    }

    protected ServiceExceptionReport getReport(String code, String message,
            String version) {
        ExceptionReport report = new ExceptionReport();
        report.setVersion(version);
        ExceptionType et = new ExceptionType();
        et.setExceptionCode(code);
        et.setExceptionText(Arrays.asList(message));
        report.setException(Arrays.asList(et));
        return new ServiceExceptionReport(message, report);
    }

}
