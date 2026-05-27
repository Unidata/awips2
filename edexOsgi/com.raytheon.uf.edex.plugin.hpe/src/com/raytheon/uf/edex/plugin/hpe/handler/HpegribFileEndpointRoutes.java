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

package com.raytheon.uf.edex.plugin.hpe.handler;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "hpe-file-endpoint.xml", context
 * "hpegrib-file-endpoint"
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

    <camelContext id="hpegrib-file-endpoint" xmlns="http://camel.apache.org/schema/spring"
        errorHandlerRef="errorHandler">

        <endpoint id="hpeGribFileEndpoint" uri="file:${edex.home}/data/local/hpegrib?delete=true&amp;delay=5000&amp;maxMessagesPerPoll=1000&amp;exclusiveReadLockStrategy=#hpeFileChangedStrategy&amp;recursive=false" />

         <route id="hpeGribFileConsumerRoute">
             <from uri="hpeGribFileEndpoint" />
             <doTry>
                <bean ref="hpeFilenameProcessor" />
                <bean ref="manualProc" method="copyFileToArchive" />
                <bean ref="manualProc" />
                <to uri="jms-durable:queue:Ingest.GribSplit" />
                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to uri="log:hpeGribFileEndpoint?level=ERROR"/>
                </doCatch>
            </doTry>
        </route>

    </camelContext>
*/
//@formatter:on
public class HpegribFileEndpointRoutes extends EDEXRouteBuilder {

    private final String edexHome;

    public HpegribFileEndpointRoutes(String edexHome) {
        this.edexHome = edexHome;
    }

    @Override
    public void configure() throws Exception {
        // @formatter:off
        from("file:" + this.edexHome + "/data/local/hpegrib" +
            "?delete=true" +
            "&delay=5000" +
            "&maxMessagesPerPoll=1000" +
            "&exclusiveReadLockStrategy=#bean:hpeFileChangedStrategy" +
            "&recursive=false")
          .doTry()
              .bean("hpeFilenameProcessor")
              .bean("manualProc", "copyFileToArchive")
              .bean("manualProc")
              .to("jms-durable:queue:Ingest.GribSplit")
          .doCatch(Throwable.class)
              .to("log:hpeGribFileEndpoint?level=ERROR")
          .endDoTry()
          .end()
          .setId("hpeGribFileConsumerRoute");
        // @formatter:on
    }
}