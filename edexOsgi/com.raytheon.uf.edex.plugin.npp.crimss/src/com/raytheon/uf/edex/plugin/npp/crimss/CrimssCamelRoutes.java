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

package com.raytheon.uf.edex.plugin.npp.crimss;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "crimss-ingest.xml", context "crimss-camel"
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2024-07-29   2037701    lisa.singh   Initial creation (from auto-generated)
 *
 * </pre>
 */

// @formatter:off
/* Original XML definition:
 *  <camelContext id="crimss-camel"
        xmlns="http://camel.apache.org/schema/spring"
        errorHandlerRef="errorHandler">

        <!-- Begin crimss routes -->
        <route id="crimssIngestRoute">
            <from uri="jms-durable:queue:Ingest.crimss"/>
            <setHeader name="pluginName">
                <constant>crimss</constant>
            </setHeader>
            <doTry>
                <pipeline>
                    <bean ref="stringToFile" />
                    <bean ref="extractWMOHeader" method="remove"/>
                    <bean ref="crimssDecoder" method="decode" />
                    <to uri="direct-vm:persistIndexAlert" />
                </pipeline>
                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to uri="log:crimss?level=ERROR"/>
                </doCatch>
            </doTry>
        </route>
    </camelContext>
 */
// @formatter:on

public class CrimssCamelRoutes extends EDEXRouteBuilder {

    public CrimssCamelRoutes() {
    }

    @Override
    public void configure() throws Exception {
        // @formatter:off
        from("jms-durable:queue:Ingest.crimss")
          .setHeader("pluginName", constant("crimss"))
              .doTry()
                  .pipeline()
                      .bean("stringToFile")
                      .bean("extractWMOHeader", "remove")
                      .bean("crimssDecoder", "decode")
                      .to("direct:persistIndexAlert")
              .endDoTry()
              .doCatch(Throwable.class)
                  .to("log:crimss?level=ERROR")
              .endDoTry()
          .end()
          .setId("crimssIngestRoute");
        // @formatter:on
    }
}
