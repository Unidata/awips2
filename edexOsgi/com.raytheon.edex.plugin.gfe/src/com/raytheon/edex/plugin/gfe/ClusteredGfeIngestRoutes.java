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

package com.raytheon.edex.plugin.gfe;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "gfe-spring.xml", context
 * "clusteredGfeIngestRoutes"
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2024-08-20   2037701    aford       Initial creation (from auto-generated)
 *
 * </pre>
 */

//@formatter:off
/* Original XML definition:

    <camelContext id="clusteredGfeIngestRoutes" xmlns="http://camel.apache.org/schema/spring"
        errorHandlerRef="errorHandler">

        <!-- Smart Init Routes -->
        <!-- main route now handled through the gfeIngestNotification -->
        <route id="manualSmartInit">
            <from uri="jms-durable:queue:manualSmartInit?threadName=smartInitManual" />
            <doTry>
                <bean ref="smartInitQueue" method="addManualInit"/>
                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to
                        uri="log:smartinit?level=ERROR"/>
                </doCatch>
            </doTry>
        </route>

        <route id="gfeVtecChangeNotification">
            <from uri="jms-generic:topic:edex.alerts.vtec?threadName=gfe-edex.alerts.vtec"/>
            <doTry>
                <bean ref="serializationUtil" method="transformFromThrift"/>
                <bean ref="vtecChangeListener" method="handleNotification"/>
                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to
                        uri="log:vtecChangeListener?level=ERROR"/>
                </doCatch>
            </doTry>
        </route>

        <!-- Convert the topic into a queue so only one consumer gets each message and we still have competing consumers. -->
        <route id="gfePurgeNotificationQueueRoute">
            <from uri="jms-generic:topic:pluginPurged"/>
            <doTry>
                <to uri="jms-generic:queue:gfePurgeNotification"/>
                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to
                        uri="log:ifpServer?level=ERROR"/>
                </doCatch>
            </doTry>
        </route>
    </camelContext>
*/
//@formatter:on
public class ClusteredGfeIngestRoutes extends EDEXRouteBuilder {

    public ClusteredGfeIngestRoutes() {
    }

    @Override
    public void configure() throws Exception {
        // @formatter:off

        // Smart Init Routes
        // main route now handled through the gfeIngestNotification
        from("jms-durable:queue:manualSmartInit?threadName=smartInitManual")
          .doTry()
              .bean("smartInitQueue", "addManualInit")
          .doCatch(Throwable.class)
              .to("log:smartinit?level=ERROR")
          .endDoTry()
          .end()
          .setId("manualSmartInit");

        from("jms-generic:topic:edex.alerts.vtec?threadName=gfe-edex.alerts.vtec")
          .doTry()
              .bean("serializationUtil", "transformFromThrift")
              .bean("vtecChangeListener", "handleNotification")
          .doCatch(Throwable.class)
              .to("log:vtecChangeListener?level=ERROR")
          .endDoTry()
          .end()
          .setId("gfeVtecChangeNotification");

        // Convert the topic into a queue so only one consumer gets each message
        // and we still have competing consumers.
        from("jms-generic:topic:pluginPurged")
          .doTry()
              .to("jms-generic:queue:gfePurgeNotification")
          .doCatch(Throwable.class)
              .to("log:ifpServer?level=ERROR")
          .endDoTry()
          .end()
          .setId("gfePurgeNotificationQueueRoute");
        // @formatter:on
    }
}
