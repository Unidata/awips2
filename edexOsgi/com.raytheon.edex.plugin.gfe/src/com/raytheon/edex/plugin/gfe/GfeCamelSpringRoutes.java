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
 * Camel routes converted from file "gfe-spring.xml", context "gfe-camel-spring"
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

    <camelContext id="gfe-camel-spring" xmlns="http://camel.apache.org/schema/spring" errorHandlerRef="errorHandler">
        <route id="SPCWatch">
            <from uri="vm:gfe.spcWatch"/>
            <doTry>
                <bean ref="spcWatch" method="handleWatch"/>
                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to
                        uri="log:gfeWatch?level=ERROR"/>
                </doCatch>
            </doTry>
        </route>

        <route id="TPCWatch">
            <from uri="vm:gfe.tpcWatch"/>
            <doTry>
                <bean ref="tpcWatch" method="handleWatch"/>
                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to
                        uri="log:gfeWatch?level=ERROR"/>
                </doCatch>
            </doTry>
        </route>

        <route id="WCLWatch">
            <from uri="direct-vm:wclWatch"/>
            <doTry>
                <bean ref="wclWatch" method="handleWclWatch"/>
                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to
                        uri="log:gfeWatch?level=ERROR"/>
                </doCatch>
            </doTry>
        </route>

        <route id="smartInitTrigger">
            <from uri="timer://smartInitTimer?fixedRate=true&amp;period=30000"/>
            <bean ref="smartInitQueue" method="fireSmartInit"/>
        </route>

        <route id="gfeIngestNotification">
            <!-- Data from plugin notification -->
            <from
                uri="jms-durable:queue:gfeDataURINotification"/>
            <doTry>
                <bean ref="serializationUtil" method="transformFromThrift"/>
                <bean ref="ifpServer" method="filterDataURINotifications"/>

                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to
                        uri="log:ifpServer?level=ERROR"/>
                </doCatch>
            </doTry>
        </route>

        <route id="ifpServerPurgeNotification">
            <from uri="jms-generic:queue:gfePurgeNotification"/>
            <doTry>
                <bean ref="ifpServer" method="pluginPurged"/>
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
public class GfeCamelSpringRoutes extends EDEXRouteBuilder {

    public GfeCamelSpringRoutes() {
    }

    @Override
    public void configure() throws Exception {
        // @formatter:off
        from("seda:gfe.spcWatch")
          .doTry()
              .bean("spcWatch", "handleWatch")
          .doCatch(Throwable.class)
              .to("log:gfeWatch?level=ERROR")
          .endDoTry()
          .end()
          .setId("SPCWatch");

        from("seda:gfe.tpcWatch")
          .doTry()
              .bean("tpcWatch", "handleWatch")
          .doCatch(Throwable.class)
              .to("log:gfeWatch?level=ERROR")
          .endDoTry()
          .end()
          .setId("TPCWatch");

        from("direct:wclWatch")
          .doTry()
              .bean("wclWatch", "handleWclWatch")
          .doCatch(Throwable.class)
              .to("log:gfeWatch?level=ERROR")
          .endDoTry()
          .end()
          .setId("WCLWatch");

        from("timer://smartInitTimer?fixedRate=true&period=30000")
          .bean("smartInitQueue", "fireSmartInit")
          .setId("smartInitTrigger");

        // Data from plugin notification
        from("jms-durable:queue:gfeDataURINotification")
          .doTry()
              .bean("serializationUtil", "transformFromThrift")
              .bean("ifpServer", "filterDataURINotifications")
          .doCatch(Throwable.class)
              .to("log:ifpServer?level=ERROR")
          .endDoTry()
          .end()
          .setId("gfeIngestNotification");

        from("jms-generic:queue:gfePurgeNotification")
          .doTry()
              .bean("ifpServer", "pluginPurged")
          .doCatch(Throwable.class)
              .to("log:ifpServer?level=ERROR")
          .endDoTry()
          .end()
          .setId("ifpServerPurgeNotification");
        // @formatter:on
    }
}
