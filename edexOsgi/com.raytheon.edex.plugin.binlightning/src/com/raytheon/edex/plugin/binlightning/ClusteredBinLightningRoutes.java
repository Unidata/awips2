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

package com.raytheon.edex.plugin.binlightning;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "binlightning_ep-ingest.xml", context
 * "clusteredBinLightningRoutes"
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

    <camelContext id="clusteredBinLightningRoutes" xmlns="http://camel.apache.org/schema/spring"
        errorHandlerRef="errorHandler">

        <!-- Begin binlightning routes -->
        <route id="binlightningIngestRoute">
            <from uri="jms-durable:queue:Ingest.binlightning" />
            <setHeader name="pluginName">
                <constant>binlightning</constant>
            </setHeader>
            <doTry>
                <pipeline>
                    <bean ref="stringToFile" />
                    <choice>
                        <when>
                            <simple>${in.header.header} regex '^SFPA42 KWBC.*'</simple>
                            <bean ref="totalLightningDecoder" method="decode" />
                        </when>
                        <otherwise>
                            <bean ref="binlightningDecoder" method="decode" />
                        </otherwise>
                    </choice>

                    <to uri="direct-vm:persistIndexAlert" />

                </pipeline>
                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to uri="log:binlightning?level=ERROR" />
                </doCatch>
            </doTry>
            <!-- bean ref="processUtil" method="delete" / -->
        </route>
    </camelContext>
*/
//@formatter:on
public class ClusteredBinLightningRoutes extends EDEXRouteBuilder {

    public ClusteredBinLightningRoutes() {
    }

    @Override
    public void configure() throws Exception {
        // @formatter:off
        from("jms-durable:queue:Ingest.binlightning")
          .setHeader("pluginName", constant("binlightning"))
          .doTry()
              .pipeline()
                  .bean("stringToFile")
                  .choice()
                      .when(simple("${in.header.header} regex '^SFPA42 KWBC.*'"))
                          .bean("totalLightningDecoder", "decode")
                      .otherwise()
                          .bean("binlightningDecoder", "decode")
                      .end()
                  .to("direct:persistIndexAlert")
          .endDoTry()
          .doCatch(Throwable.class)
              .to("log:binlightning?level=ERROR")
          .endDoTry()
          .end()
          .setId("binlightningIngestRoute");
        // @formatter:on
    }
}
