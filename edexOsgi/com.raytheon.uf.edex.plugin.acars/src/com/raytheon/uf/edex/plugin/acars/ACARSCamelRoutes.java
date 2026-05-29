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

package com.raytheon.uf.edex.plugin.acars;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "acars-ingest.xml", context "acars-camel"
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
   <camelContext id="acars-camel"
        xmlns="http://camel.apache.org/schema/spring"
        errorHandlerRef="errorHandler">

        <!-- Begin ACARS routes -->
        <route id="acarsIngestRoute">
            <from uri="jms-durable:queue:Ingest.acars"/>
            <setHeader name="pluginName">
                <constant>acars</constant>
            </setHeader>
            <bean ref="stringToFile" />
            <doTry>
                <pipeline>
                    <bean ref="acarsDecoder" method="decode" />
                    <multicast>
                       <to uri="direct-vm:indexAlert" />
                       <to uri="jms-durable:queue:acarsPersistObs" />
                    </multicast>
                </pipeline>
                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to uri="log:acars?level=ERROR"/>
                </doCatch>
            </doTry>
            <!-- bean ref="processUtil" method="delete" / -->
        </route>

    </camelContext>
 */
// @formatter:on

public class ACARSCamelRoutes extends EDEXRouteBuilder {

    public ACARSCamelRoutes() {
    }

    @Override
    public void configure() throws Exception {
      // @formatter:off
        from("jms-durable:queue:Ingest.acars")
          .setHeader("pluginName", constant("acars"))
              .bean("stringToFile")
              .doTry()
                  .pipeline()
                      .bean("acarsDecoder", "decode")
                      .multicast()
                          .to("direct:indexAlert")
                          .to("jms-durable:queue:acarsPersistObs")
                      .end()
              .endDoTry()
          .doCatch(Throwable.class)
              .to("log:acars?level=ERROR")
          .endDoTry()
          .end()
          .setId("acarsIngestRoute");
     // @formatter:on
    }
}
