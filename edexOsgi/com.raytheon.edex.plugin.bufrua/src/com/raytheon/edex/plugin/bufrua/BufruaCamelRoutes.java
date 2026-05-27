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

package com.raytheon.edex.plugin.bufrua;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "bufrua-ingest.xml", context "bufrua-camel"
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

    <camelContext id="bufrua-camel"
        xmlns="http://camel.apache.org/schema/spring"
        errorHandlerRef="errorHandler">

        <!-- Begin BUFRUA routes -->
        <route id="bufruaIngestRoute">
            <from uri="jms-durable:queue:Ingest.bufrua"/>
            <setHeader name="pluginName">
                <constant>bufrua</constant>
            </setHeader>
            <doTry>
                <pipeline>
                    <bean ref="stringToFile" />
                    <bean ref="bufruaDecoder" method="decode"/>
                    <to uri="direct-vm:persistIndexAlert" />
                </pipeline>
                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to uri="log:bufrua?level=ERROR"/>
                </doCatch>
            </doTry>
        </route>
    </camelContext>
*/
//@formatter:on
public class BufruaCamelRoutes extends EDEXRouteBuilder {

    public BufruaCamelRoutes() {
    }

    @Override
    public void configure() throws Exception {
        // @formatter:off
        from("jms-durable:queue:Ingest.bufrua")
          .setHeader("pluginName", constant("bufrua"))
          .doTry()
              .pipeline()
                  .bean("stringToFile")
                  .bean("bufruaDecoder", "decode")
                  .to("direct:persistIndexAlert")
          .endDoTry()
          .doCatch(Throwable.class)
              .to("log:bufrua?level=ERROR")
          .endDoTry()
          .end()
          .setId("bufruaIngestRoute");
        // @formatter:on
    }
}
