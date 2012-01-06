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
package com.raytheon.uf.edex.esb.camel;

import java.util.Map;

import org.apache.camel.Exchange;
import org.apache.camel.Expression;
import org.apache.camel.component.bean.DefaultParameterMappingStrategy;

import com.raytheon.edex.esb.Headers;

/**
 * Camel Parameter Mapping Strategy to support receiving message headers in bean
 * method invocations in addition to the message object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2009             chammack    Initial creation
 * 
 * </pre>
 * 
 * @version 1.0
 */

public class EDEXParameterMappingStrategy extends
        DefaultParameterMappingStrategy {

    private static final String strategyName = "headers";

    public EDEXParameterMappingStrategy() {
        super();
    }

    /**
     * 
     * Note : Suppress warnings for non-generic Expression usage.
     */
    @SuppressWarnings("unchecked")
    @Override
    public void loadDefaultRegistry() {

        super.loadDefaultRegistry();

        addParameterMapping(Headers.class, new Expression() {
            @Override
            public <T> T evaluate(Exchange exchange, Class<T> c) {
                Headers edexHeaders = new Headers();

                Map<String, Object> camelHeaders = exchange.getIn()
                        .getHeaders();
                if (camelHeaders != null) {
                    for (String key : camelHeaders.keySet()) {
                        if ("CamelFileName".equalsIgnoreCase(key)) {
                            edexHeaders.put("traceId", camelHeaders.get(key));
                        } else {
                            edexHeaders.put(key, camelHeaders.get(key));
                        }

                    }
                }
                return (T) edexHeaders;
            }

            @Override
            public String toString() {
                return strategyName;
            }
        });
    }

}
