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
package com.raytheon.uf.edex.registry.ebxml.services;

import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.message.Message;
import org.apache.cxf.phase.AbstractPhaseInterceptor;
import org.apache.cxf.phase.Phase;
import org.apache.cxf.transport.http.AbstractHTTPDestination;

import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * 
 * Service interceptor for logging web service and rest calls
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 9/5/2013     1538        bphillip    Initial implementation
 * 2/27/2014    2769        bphillip    Changed verbose output to debug level
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistryServiceInInterceptor extends
        AbstractPhaseInterceptor<Message> {
    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryServiceInInterceptor.class);

    public RegistryServiceInInterceptor() {
        super(Phase.RECEIVE);
    }

    @SuppressWarnings("unchecked")
    @Override
    public void handleMessage(Message message) throws Fault {
        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            StringBuilder logMessage = new StringBuilder();
            HttpServletRequest request = (HttpServletRequest) message
                    .get(AbstractHTTPDestination.HTTP_REQUEST);
            Map<String, List<String>> headers = (Map<String, List<String>>) message
                    .get(Message.PROTOCOL_HEADERS);
            List<String> callingRegistryList = headers
                    .get(RegistryUtil.CALLING_REGISTRY_SOAP_HEADER_NAME);
            if (request.getRequestURI().startsWith("/rest")) {
                logMessage.append("REST: ");
            } else {
                logMessage.append("WS: ");
            }
            logMessage.append("Request from [");
            if (CollectionUtil.isNullOrEmpty(callingRegistryList)) {
                logMessage.append(request.getRemoteAddr()).append("]: ")
                        .append(request.getMethod()).append(" ")
                        .append(request.getRequestURI());
            } else {
                logMessage.append(callingRegistryList.get(0)).append("]: ")
                        .append(request.getMethod()).append(" ")
                        .append(request.getRequestURI());
            }

            statusHandler.debug(logMessage.toString());
        }
    }
}
