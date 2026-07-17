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

package com.raytheon.edex.plugin.shef;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "shef-ingest.xml", context "shef-camel"
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
/* Original XML context:
 <!-- End add for manual input -->
    <camelContext id="shef-camel" xmlns="http://camel.apache.org/schema/spring"
        errorHandlerRef="errorHandler">

        <!-- Begin shef routes -->
        <route id="shefIngestRoute">
            <from
                uri="jms-durable:queue:Ingest.Shef"/>
            <setHeader name="pluginName">
                <constant>shef</constant>
            </setHeader>
            <pipeline>
                <bean ref="stringToFile" />
                <to uri="direct-vm:shefProcessing" />
            </pipeline>
        </route>
        <route id="shefStagedRoute">
            <from
                uri="jms-durable:queue:Ingest.ShefStaged"/>
            <setHeader name="pluginName">
                <constant>shef</constant>
            </setHeader>
            <to uri="direct-vm:shefProcessing" />
        </route>

        <!-- direct-vm will be run under original thread, should this be moved to 
            a queue?? but will cause message to be serialized to the jms queue... -->
        <route id="synopticToShefRoute">
            <from uri="direct-vm:synopticToShef" />
            <bean ref="synopticToShefFilter" method="filter" />
            <pipeline>
                <split streaming="true">
                    <method ref="synopticToShef" method="iterate" />
                    <bean ref="synopticToShef" method="transform" />
                    <to uri="jms-durable:queue:Ingest.ShefStaged" />
                </split>
            </pipeline>
        </route>

        <!-- direct-vm will be run under original thread, should this be moved to 
            a queue?? but will cause message to be serialized to the jms queue... -->
        <route id="metarToShefRoute">
            <from uri="direct-vm:metarToShef" />
            <bean ref="metarToShefFilter" method="filter" />
            <pipeline>
                <split streaming="true">
                    <method ref="metarToShef" method="iterate" />
                    <bean ref="metarToShef" method="transformMetar" />
                    <to
                        uri="jms-durable:queue:Ingest.ShefStaged"/>
                </split>
            </pipeline>
        </route>

        <route id="shefProcessingRoute">
            <from uri="direct-vm:shefProcessing" />
            <doTry>
                <pipeline>
                    <bean ref="shefDecoder" method="decode" />
                    <bean ref="processUtil" method="log"/>
                </pipeline>
                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to
                        uri="log:shef?level=ERROR" />
                </doCatch>
            </doTry>
        </route>

        <route id="shefManualIngestRoute">
            <from
                uri="jms-durable:queue:Ingest.ShefManual"/>
            <setHeader name="pluginName">
                <constant>shef</constant>
            </setHeader>
            <doTry>
                <pipeline>
                    <bean ref="stringToFile" />
                    <bean ref="shefDecoder" method="decode" />
                    <bean ref="processUtil" method="log"/>
                </pipeline>
                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to
                        uri="log:shef?level=ERROR" />
                </doCatch>
            </doTry>
        </route>

    </camelContext>
*/
// @formatter:on

public class ShefCamelRoutes extends EDEXRouteBuilder {

    public ShefCamelRoutes() {
    }

    @Override
    public void configure() throws Exception {
        // @formatter:off
        // Begin shef routes
        from("jms-durable:queue:Ingest.Shef")
          .setHeader("pluginName", constant("shef"))
              .pipeline()
                  .bean("stringToFile")
                  .to("direct:shefProcessing")
              .end()
          .setId("shefIngestRoute");
        from("jms-durable:queue:Ingest.ShefStaged")
          .setHeader("pluginName", constant("shef"))
              .to("direct:shefProcessing")
          .setId("shefStagedRoute");
        
        // direct-vm will be run under original thread, should this be moved to 
        // a queue?? but will cause message to be serialized to the jms queue...
        from("direct:synopticToShef")
          .bean("synopticToShefFilter", "filter")
              .pipeline()
                  .split(method("synopticToShef", "iterate")).streaming()
                      .bean("synopticToShef", "transform")
                      .to("jms-durable:queue:Ingest.ShefStaged")
                  .end()
              .end()
          .setId("synopticToShefRoute");
        
        // direct-vm will be run under original thread, should this be moved to 
        // a queue?? but will cause message to be serialized to the jms queue...
        from("direct:metarToShef")
          .bean("metarToShefFilter", "filter")
              .pipeline()
                  .split(method("metarToShef", "iterate")).streaming()
                      .bean("metarToShef", "transformMetar")
                      .to("jms-durable:queue:Ingest.ShefStaged")
                  .end()
              .end()
          .setId("metarToShefRoute");
        
        from("direct:shefProcessing")
          .doTry()
              .pipeline()
                  .bean("shefDecoder", "decode")
                  .bean("processUtil", "log")
          .endDoTry()
          .doCatch(Throwable.class)
              .to("log:shef?level=ERROR")
          .endDoTry()
          .end()
          .setId("shefProcessingRoute");
        
        from("jms-durable:queue:Ingest.ShefManual")
          .setHeader("pluginName", constant("shef"))
              .doTry()
                  .pipeline()
                      .bean("stringToFile")
                      .bean("shefDecoder", "decode")
                      .bean("processUtil", "log")
              .endDoTry()
              .doCatch(Throwable.class)
                  .to("log:shef?level=ERROR")
              .endDoTry()
          .end()
          .setId("shefManualIngestRoute");
        // @formatter:on
    }
}
