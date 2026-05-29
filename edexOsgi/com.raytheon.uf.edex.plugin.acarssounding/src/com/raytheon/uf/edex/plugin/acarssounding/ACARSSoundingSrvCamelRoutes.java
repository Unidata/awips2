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

package com.raytheon.uf.edex.plugin.acarssounding;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "acarssounding-ingest.xml", context
 * "acarsSoundingSrv-camel"
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
/* Original XML Definition
   <camelContext id="acarsSoundingSrv-camel"
        xmlns="http://camel.apache.org/schema/spring"
        errorHandlerRef="errorHandler">

        <endpoint id="scheduledSoundingCron" uri="clusteredquartz://acars/createSounding/?cron=${acarssounding.cron}"/>

        <!-- Schedule sounding creation -->
        <route id="scheduledSounding">
            <!-- Set the minutes explicitly -->
            <!-- Deployed time slots -->
            <from uri="scheduledSoundingCron" />

            <!-- Development time slots
            <from uri="clusteredquartz://acars/createSounding/?cron=00+0,5,10,15,20,25,30,35,40,45,50,55+*+*+*+?" />
            -->
           <split streaming="true">
              <method ref="acarsSoundingSplitter" method="getSeparator"/>
              <doTry>
                 <pipeline>
                    <setHeader name="pluginName">
                        <constant>acarssounding</constant>
                    </setHeader>
                    <setHeader name="dequeueTime">
                        <method ref="acarsSounding" method="getQueueTime" />
                    </setHeader>
                    <to uri="jms-durable:queue:acarssounding" />
                 </pipeline>
                 <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to uri="log:acarssounding?level=ERROR"/>
                 </doCatch>
              </doTry>
           </split>
        </route>

        <route id="acarsSoundingProcessing">
           <from uri="jms-durable:queue:acarssounding" />
           <doTry>
              <pipeline>
                 <bean ref="acarsSounding" method="processSounding" />
                 <to uri="direct-vm:indexAlert" />
              </pipeline>
              <doCatch>
                 <exception>java.lang.Throwable</exception>
                 <to uri="log:acarssounding?level=ERROR"/>
              </doCatch>
           </doTry>
        </route>
    </camelContext>
 */
// @formatter:on

public class ACARSSoundingSrvCamelRoutes extends EDEXRouteBuilder {

    private final String acarssoundingCron;

    public ACARSSoundingSrvCamelRoutes(String acarssoundingCron) {
        this.acarssoundingCron = acarssoundingCron;
    }

    @Override
    public void configure() throws Exception {
        // @formatter:off
        
        // Schedule sounding creation.
        // Set the minutes explicitly
        // Deployed time slots
        from("clusteredcron://acars/createSounding/?schedule=" + this.acarssoundingCron)
          .split(method("acarsSoundingSplitter", "getSeparator")).streaming()
              .doTry()
                  .pipeline()
                      .setHeader("pluginName", constant("acarssounding"))
                      .setHeader("dequeueTime").method("acarsSounding", "getQueueTime")
                      .to("jms-durable:queue:acarssounding")
              .endDoTry()
              .doCatch(Throwable.class)
                  .to("log:acarssounding?level=ERROR")
              .endDoTry()
          .end()
          .end()
          .setId("scheduledSounding");
        
        from("jms-durable:queue:acarssounding")
          .doTry()
              .pipeline()
                  .bean("acarsSounding", "processSounding")
                  .to("direct:indexAlert")
          .endDoTry()
          .doCatch(Throwable.class)
              .to("log:acarssounding?level=ERROR")
          .endDoTry()
          .end()
          .setId("acarsSoundingProcessing");
        // @formatter:on
    }
}
