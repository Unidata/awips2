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

package com.raytheon.edex.plugin.goessounding;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "goessounding-ingest.xml", context
 * "goessounding-camel"
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

    <camelContext id="goessounding-camel"
        xmlns="http://camel.apache.org/schema/spring"
        errorHandlerRef="errorHandler">

        <!-- Begin GOES Sounding routes -->
        <route id="goessndgIngestRoute">
            <from uri="jms-durable:queue:Ingest.goessounding"/>
            <setHeader name="pluginName">
                <constant>goessounding</constant>
            </setHeader>
            <doTry>
                <pipeline>
                    <bean ref="stringToFile" />
                    <split streaming="true">
                        <method ref="goesSeparatorFactory" method="getSeparator"/>
                        <doTry>
                            <pipeline>
                                <bean ref="goessoundingDecoder" method="decode" />
                                <to uri="direct-vm:persistIndexAlert" />
                            </pipeline>
                            <doCatch>
                                <exception>java.lang.Throwable</exception>
                                <to uri="log:goessounding?level=ERROR"/>
                            </doCatch>
                        </doTry>
                    </split>
                </pipeline>
                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to uri="log:goessounding?level=ERROR"/>
                </doCatch>
            </doTry>
        </route>
    </camelContext>
*/
//@formatter:on
public class GoessoundingCamelRoutes extends EDEXRouteBuilder {

    public GoessoundingCamelRoutes() {
    }

    @Override
    public void configure() throws Exception {
        // @formatter:off
        from("jms-durable:queue:Ingest.goessounding")
          .setHeader("pluginName", constant("goessounding"))
          .doTry()
              .pipeline()
                  .bean("stringToFile")
                  .split(method("goesSeparatorFactory", "getSeparator")).streaming()
                  .doTry()
                      .pipeline()
                          .bean("goessoundingDecoder", "decode")
                          .to("direct:persistIndexAlert")
                  .endDoTry()
                  .doCatch(Throwable.class)
                      .to("log:goessounding?level=ERROR")
                  .endDoTry()
                  .end()
              .end()
          .endDoTry()
          .doCatch(Throwable.class)
              .to("log:goessounding?level=ERROR")
          .endDoTry()
          .end()
          .setId("goessndgIngestRoute");
        // @formatter:on
    }
}
