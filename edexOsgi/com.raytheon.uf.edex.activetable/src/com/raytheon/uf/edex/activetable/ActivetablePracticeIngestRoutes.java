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

package com.raytheon.uf.edex.activetable;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "activetable-ingest.xml", context
 * "activetable-practice-ingest"
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

    <camelContext id="activetable-practice-ingest" xmlns="http://camel.apache.org/schema/spring" errorHandlerRef="errorHandler">
        <route id="practiceVtecRoute">
            <from uri="jms-generic:queue:practiceVtec"/>
            <doTry>
                <pipeline>
                    <bean ref="serializationUtil" method="transformFromThrift"/>
                    <setHeader name="notifygfe">
                            <simple>${body?.notifyGFE}</simple>
                    </setHeader>
                    <setHeader name="drtstring">
                            <simple>${body?.drtString}</simple>
                    </setHeader>

                    <bean ref="practiceVtecDecoder" method="decode"/>
                    <bean ref="wwaGeometryCompleter" method="addMissingGeometries"/>
                    <bean ref="index" method="auditMissingPiecesForDatabaseOnlyPdos"/>
                    <bean ref="index" method="index"/>
                    <bean ref="processUtil" method="log"/>
                    <multicast parallelProcessing="false">
                        <filter>
                            <simple>${header?.notifygfe.booleanValue}</simple>
                            <to uri="direct-vm:stageNotification"/>
                        </filter>
                        <filter>
                            <method ref="vtecFilter" method="hasVTEC"/>
                            <bean ref="activeTableSrv" method="practiceVtecArrived"/>
                            <bean ref="toDataURI" method="toPracticeNotificationMsg"/>
                            <bean ref="serializationUtil" method="transformToThrift"/>
                            <to uri="jms-generic:topic:edex.alerts.practicewarning?timeToLive=60000&amp;deliveryPersistent=false"/>
                        </filter>
                    </multicast>
                </pipeline>
                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to uri="log:practiceActiveTable?level=ERROR"/>
                </doCatch>
            </doTry>
        </route>
    </camelContext>
*/
//@formatter:on
public class ActivetablePracticeIngestRoutes extends EDEXRouteBuilder {

    public ActivetablePracticeIngestRoutes() {
    }

    @Override
    public void configure() throws Exception {
        // @formatter:off
        from("jms-generic:queue:practiceVtec")
          .doTry()
              .pipeline()
                  .bean("serializationUtil", "transformFromThrift")
                  .setHeader("notifygfe", simple("${body?.notifyGFE}"))
                  .setHeader("drtstring", simple("${body?.drtString}"))
                  .bean("practiceVtecDecoder", "decode")
                  .bean("wwaGeometryCompleter", "addMissingGeometries")
                  .bean("index", "auditMissingPiecesForDatabaseOnlyPdos")
                  .bean("index", "index")
                  .bean("processUtil", "log")
                  .multicast()
                      .filter(simple("${header?.notifygfe.booleanValue}"))
                          .to("direct:stageNotification")
                          .end()
                      .filter(method("vtecFilter", "hasVTEC"))
                          .bean("activeTableSrv", "practiceVtecArrived")
                          .bean("toDataURI", "toPracticeNotificationMsg")
                          .bean("serializationUtil", "transformToThrift")
                          .to("jms-generic:topic:edex.alerts.practicewarning?timeToLive=60000&deliveryPersistent=false")
                          .end()
                      .end()
          .endDoTry()
          .doCatch(Throwable.class)
              .to("log:practiceActiveTable?level=ERROR")
          .endDoTry()
          .end()
          .setId("practiceVtecRoute");
        // @formatter:on
    }
}
