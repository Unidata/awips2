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
package com.raytheon.rcm.mqsrvr;

import org.apache.activemq.ActiveMQConnectionFactory;
import org.apache.activemq.broker.BrokerFactory;
import org.apache.activemq.broker.BrokerService;

import com.raytheon.rcm.config.EndpointConfig;
import com.raytheon.rcm.server.RadarServer;
import com.raytheon.rcm.server.dataarchive.DataArchiveEndpoint;

public class MQServer {

    /**
     * @param args
     */
    public static void main(String[] args) {

        RadarServer server = null;
        try {
            server = RadarServer.createServer();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        server.addDefaultListeners();

        server.addListener(new DataArchiveEndpoint(server));

        BrokerService broker = null;

        // configure the broker
        try {
            String brokerURL = null;
            String brokerHost = "0.0.0.0";

            EndpointConfig ecConfig = server.getConfiguration()
                    .getEndpointConfig();
            if (ecConfig != null) {
                String s;
                s = ecConfig.getRadarServerBrokerURL();
                if (s != null)
                    brokerURL = s;
                else {
                    s = ecConfig.getRadarServerBrokerHost();
                    if (s != null)
                        brokerHost = s;
                }
            }

            if (brokerURL == null)
                brokerURL = String.format(
                        "broker:(tcp://%1$s:8813,stomp://%1$s:8814)/radarServer?"
                                + "persistent=false&", brokerHost);
            broker = BrokerFactory.createBroker(brokerURL);
            broker.start();
        } catch (Exception e) {
            System.err.println("Cannot start broker service:");
            e.printStackTrace(System.err);
        }

        ActiveMQConnectionFactory connFac = new ActiveMQConnectionFactory(
                broker.getVmConnectorURI());
        MsgServ ms = new MsgServ(server);
        server.addListener(ms);
        ms.start(connFac, connFac);

        server.run();
    }
}
