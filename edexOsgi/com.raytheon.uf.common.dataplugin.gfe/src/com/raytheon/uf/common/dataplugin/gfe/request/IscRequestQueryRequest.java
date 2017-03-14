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
package com.raytheon.uf.common.dataplugin.gfe.request;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Request to retrieve ISC configuration (requested parms) and information about
 * the ISC neighbors for the specified site.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2009 3058       rjpeter     Initial creation
 * Nov 30, 2015 5129       dgilling    Add IscQueryResponse.
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

@DynamicSerialize
public class IscRequestQueryRequest extends AbstractGfeRequest {

    @DynamicSerialize
    public static class IscQueryResponse {

        @DynamicSerializeElement
        private Map<String, Map<String, List<Map<String, String>>>> domainDict;

        @DynamicSerializeElement
        private Map<String, Map<String, String>> serverDictT2S;

        @DynamicSerializeElement
        private Collection<String> requestedParms;

        public IscQueryResponse() {
            this.domainDict = Collections.emptyMap();
            this.serverDictT2S = Collections.emptyMap();
            this.requestedParms = Collections.emptyList();
        }

        public IscQueryResponse(
                Map<String, Map<String, List<Map<String, String>>>> domainDict,
                Map<String, Map<String, String>> serverDictT2S,
                Collection<String> requestedParms) {
            this.domainDict = domainDict;
            this.serverDictT2S = serverDictT2S;
            this.requestedParms = requestedParms;
        }

        public Collection<String> getRequestedParms() {
            return requestedParms;
        }

        public void setRequestedParms(Collection<String> requestedParms) {
            this.requestedParms = requestedParms;
        }

        public Map<String, Map<String, List<Map<String, String>>>> getDomainDict() {
            return domainDict;
        }

        public void setDomainDict(
                Map<String, Map<String, List<Map<String, String>>>> domainDict) {
            this.domainDict = domainDict;
        }

        public Map<String, Map<String, String>> getServerDictT2S() {
            return serverDictT2S;
        }

        public void setServerDictT2S(
                Map<String, Map<String, String>> serverDictT2S) {
            this.serverDictT2S = serverDictT2S;
        }
    }
}
