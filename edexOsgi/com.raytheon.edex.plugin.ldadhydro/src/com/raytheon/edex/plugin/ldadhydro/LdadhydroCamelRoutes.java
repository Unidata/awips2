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

package com.raytheon.edex.plugin.ldadhydro;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "ldadhydro-ingest.xml", context
 * "ldadhydro-camel"
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

    <camelContext id="ldadhydro-camel"
        xmlns="http://camel.apache.org/schema/spring"
        errorHandlerRef="errorHandler">
        <route id="ldadhydroIngestRoute">
            <from uri="jms-durable:queue:Ingest.ldadhydro"/>
            <doTry>
                <pipeline>
                    <bean ref="stringToFile" />
                    <bean ref="hydroDecoder" method="decode" />
                    <bean ref="dupElim" />
                    <bean ref="ldadhydroPointData" method="toPointData" />
                    <to uri="direct-vm:persistIndexAlert" />
                </pipeline>
                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to uri="log:ldadhydro?level=ERROR"/>
                </doCatch>
            </doTry>
        </route>
    </camelContext>
*/
//@formatter:on
public class LdadhydroCamelRoutes extends EDEXRouteBuilder {

    public LdadhydroCamelRoutes() {
    }

    @Override
    public void configure() throws Exception {
        // @formatter:off
        from("jms-durable:queue:Ingest.ldadhydro")
          .doTry()
              .pipeline()
                  .bean("stringToFile")
                  .bean("hydroDecoder", "decode")
                  .bean("dupElim")
                  .bean("ldadhydroPointData", "toPointData")
                  .to("direct:persistIndexAlert")
          .endDoTry()
          .doCatch(Throwable.class)
              .to("log:ldadhydro?level=ERROR")
          .endDoTry()
          .end()
          .setId("ldadhydroIngestRoute");
        // @formatter:on
    }
}
