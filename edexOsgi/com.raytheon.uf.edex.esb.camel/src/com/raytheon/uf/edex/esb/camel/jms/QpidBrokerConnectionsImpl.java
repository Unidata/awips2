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
package com.raytheon.uf.edex.esb.camel.jms;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.http.client.methods.HttpGet;

import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.comm.HttpClient.HttpClientResponse;
import com.raytheon.uf.common.json.geo.BasicJsonService;

/**
 * Qpid implementation of IBrokerConnectionsProvider
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 04, 2014  #2694     randerso    Converted python implementation to Java
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class QpidBrokerConnectionsImpl implements IBrokerConnectionsProvider {
    @Override
    public List<String> getConnections() throws Exception {
        // Use rest services to pull connection clientId
        // http://brokerHost:port/rest/connection/edex
        // port needs to be passed as a parameter
        // parse json response for clientId, recommend using a hash of some kind

        String url = System.getenv("JMS_CONNECTIONS_URL");

        HttpGet request = new HttpGet(url);
        HttpClientResponse response = HttpClient.getInstance().executeRequest(
                request);
        if (!response.isSuccess()) {
            String msg = String.format("Broker returned %d %s", response.code,
                    new String(response.data));

            throw new Exception(msg);
        }

        String jsonStr = new String(response.data);

        BasicJsonService json = new BasicJsonService();
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> jsonObjList = (List<Map<String, Object>>) json
                .deserialize(jsonStr, Object.class);

        List<String> resultSet = new ArrayList<String>();
        for (Map<String, Object> statDict : jsonObjList) {
            String clientId = (String) statDict.get("clientId");
            if (clientId != null) {
                resultSet.add(clientId);
            }
        }
        return resultSet;
    }
}
