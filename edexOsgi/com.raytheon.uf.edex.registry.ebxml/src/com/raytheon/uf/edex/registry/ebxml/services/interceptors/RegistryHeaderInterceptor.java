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
package com.raytheon.uf.edex.registry.ebxml.services.interceptors;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.cxf.helpers.CastUtils;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.message.Message;
import org.apache.cxf.phase.AbstractPhaseInterceptor;
import org.apache.cxf.phase.Phase;

import com.raytheon.uf.common.registry.ebxml.RegistryUtil;

/**
 * <pre>
 * 
 * This class attaches the Calling Registry header to all outgoing REST and SOAP services calls
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 5/11/2015    4448        bphillip    Initial creation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
public class RegistryHeaderInterceptor extends
        AbstractPhaseInterceptor<Message> {

    /** Map of headers to be added */
    protected static final Map<String, List<String>> REQUEST_HEADERS;

    static {
        REQUEST_HEADERS = new HashMap<String, List<String>>(1);
        REQUEST_HEADERS.put(RegistryUtil.CALLING_REGISTRY_SOAP_HEADER_NAME,
                Arrays.asList(RegistryUtil.LOCAL_REGISTRY_ADDRESS));
    }

    /**
     * Creates a new RegistryHeaderInterceptor
     */
    public RegistryHeaderInterceptor() {
        super(Phase.PRE_PROTOCOL);
    }

    @Override
    public void handleMessage(Message message) throws Fault {

        /*
         * Get the existing headers for the message. If the headers do not yet
         * exist on the message, create them and add the custom headers
         */
        Map<String, List<String>> headers = CastUtils.cast((Map<?, ?>) message
                .get(Message.PROTOCOL_HEADERS));
        if (headers == null) {
            headers = new TreeMap<String, List<String>>(
                    String.CASE_INSENSITIVE_ORDER);
        } else if (headers instanceof HashMap) {
            Map<String, List<String>> headers2 = new TreeMap<String, List<String>>(
                    String.CASE_INSENSITIVE_ORDER);
            headers2.putAll(headers);
            headers = headers2;
        }
        headers.putAll(REQUEST_HEADERS);
        message.put(Message.PROTOCOL_HEADERS, headers);

    }
}
