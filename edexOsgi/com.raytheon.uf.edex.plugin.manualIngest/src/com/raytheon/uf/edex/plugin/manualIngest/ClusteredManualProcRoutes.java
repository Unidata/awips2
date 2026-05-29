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

package com.raytheon.uf.edex.plugin.manualIngest;

import org.apache.camel.Endpoint;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "manualIngest-spring.xml", context
 * "clusteredManualProc"
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

    <camelContext id="clusteredManualProc"
        xmlns="http://camel.apache.org/schema/spring"
        errorHandlerRef="errorHandler">
        <endpoint id="manualFileEndpoint"
            uri="file:${manualIngest.dropBoxPath}?
                 delay=${manualIngest.delay}&amp;
                 useFixedDelay=false&amp;
                 maxMessagesPerPoll=${manualIngest.maxMessagesPerPoll}&amp;
                 noop=true&amp;
                 idempotent=false&amp;
                 exclusiveReadLockStrategy=#fileChangedStrategy&amp;
                 inProgressRepository=#inProgressRepository&amp;
                 recursive=true"/>

        <endpoint id="manualIngest"
            uri="vm:manualIngestQueue?
                 size=${manualIngest.vmQueueSize}&amp;
                 concurrentConsumers=${manualIngest.threads}&amp;
                 blockWhenFull=true" />

        <route id="manualFileScan">
            <from uri="manualFileEndpoint" />
            <to uri="manualIngest" />
        </route>

        <route id="manualIngestRoute">
            <from uri="manualIngest"/>
            <bean ref="manualProc" method="moveFileToArchive"/>
            <bean ref="manualProc" />
            <to uri="jms-durable:queue:external.dropbox"/>
        </route>
    </camelContext>
*/
//@formatter:on
public class ClusteredManualProcRoutes extends EDEXRouteBuilder {

    private final String manualIngestDropBoxPath;

    private final String manualIngestDelay;

    private final String manualIngestMaxMessagesPerPoll;

    private final String manualIngestVmQueueSize;

    private final String manualIngestThreads;

    public ClusteredManualProcRoutes(String manualIngestDropBoxPath,
            String manualIngestDelay, String manualIngestMaxMessagesPerPoll,
            String manualIngestVmQueueSize, String manualIngestThreads) {
        this.manualIngestDropBoxPath = manualIngestDropBoxPath;
        this.manualIngestDelay = manualIngestDelay;
        this.manualIngestMaxMessagesPerPoll = manualIngestMaxMessagesPerPoll;
        this.manualIngestVmQueueSize = manualIngestVmQueueSize;
        this.manualIngestThreads = manualIngestThreads;
    }

    @Override
    public void configure() throws Exception {
        // @formatter:off
        Endpoint manualIngestQueue = endpoint("seda:manualIngestQueue" +
                "?size=" + this.manualIngestVmQueueSize +
                "&concurrentConsumers=" + this.manualIngestThreads +
                "&blockWhenFull=true");

        from("file:" + this.manualIngestDropBoxPath +
                "?delay=" + this.manualIngestDelay +
                "&useFixedDelay=false" +
                "&maxMessagesPerPoll=" + this.manualIngestMaxMessagesPerPoll +
                "&noop=true" +
                "&idempotent=false" +
                "&exclusiveReadLockStrategy=#bean:fileChangedStrategy" +
                "&inProgressRepository=#bean:inProgressRepository" +
                "&recursive=true")
          .to(manualIngestQueue)
          .setId("manualFileScan");

        from(manualIngestQueue)
          .bean("manualProc", "moveFileToArchive")
          .bean("manualProc")
          .to("jms-durable:queue:external.dropbox")
          .setId("manualIngestRoute");
        // @formatter:on
    }
}
