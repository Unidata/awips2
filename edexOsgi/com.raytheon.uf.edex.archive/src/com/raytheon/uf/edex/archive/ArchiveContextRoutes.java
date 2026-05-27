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

package com.raytheon.uf.edex.archive;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "archive-spring.xml", context
 * "archive-context"
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

    <camelContext id="archive-context"
        xmlns="http://camel.apache.org/schema/spring" errorHandlerRef="errorHandler">

        <endpoint id="archiveCron"
            uri="clusteredquartz://archive/archiveScheduled/?cron=${archive.cron}"/>

        <endpoint id="archivePurgeCron"
            uri="clusteredquartz://archive/archivePurgeScheduled/?cron=${archive.purge.cron}" />

        <!-- Archive on Scheduled timer -->
        <route id="archiveScheduled">
            <from uri="archiveCron" />
            <doTry>
                <to uri="jms-generic:queue:archiveScheduledWork" />
                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to
                        uri="log:archive?level=ERROR" />
                </doCatch>
            </doTry>
        </route>

        <route id="archiveScheduledWork">
            <from uri="jms-generic:queue:archiveScheduledWork" />
            <doTry>
                <bean ref="dataArchiver" method="archivePlugins" />
                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to
                        uri="log:archive?level=ERROR" />
                </doCatch>
            </doTry>
        </route>

        <!-- Run archivePurge on Scheduled timer -->
        <route id="archivePurgeScheduled">
            <from uri="archivePurgeCron" />
            <to uri="jms-generic:queue:archivePurgeScheduledWork" />
        </route>

        <route id="archivePurgeScheduledWork">
            <from uri="jms-generic:queue:archivePurgeScheduledWork" />
            <doTry>
                <bean ref="archivePurge" method="purge" />
                <doCatch>
                    <exception>java.lang.Throwable</exception>
                    <to
                        uri="log:archivePurge?level=ERROR" />
                </doCatch>
            </doTry>
        </route>
    </camelContext>
*/
//@formatter:on
public class ArchiveContextRoutes extends EDEXRouteBuilder {

    private final String archiveCron;

    private final String archivePurgeCron;

    public ArchiveContextRoutes(String archiveCron, String archivePurgeCron) {
        this.archiveCron = archiveCron;
        this.archivePurgeCron = archivePurgeCron;
    }

    @Override
    public void configure() throws Exception {
        // @formatter:off

        // Archive on Scheduled timer
        from("clusteredcron://archive/archiveScheduled/?schedule=" + this.archiveCron)
          .doTry()
              .to("jms-generic:queue:archiveScheduledWork")
          .doCatch(Throwable.class)
              .to("log:archive?level=ERROR")
          .endDoTry()
          .end()
          .setId("archiveScheduled");

        from("jms-generic:queue:archiveScheduledWork")
          .doTry()
              .bean("dataArchiver", "archivePlugins")
          .doCatch(Throwable.class)
              .to("log:archive?level=ERROR")
          .endDoTry()
          .end()
          .setId("archiveScheduledWork");

        // Run archivePurge on Scheduled timer
        from("clusteredcron://archive/archivePurgeScheduled/?schedule=" + this.archivePurgeCron)
          .to("jms-generic:queue:archivePurgeScheduledWork")
          .setId("archivePurgeScheduled");

        from("jms-generic:queue:archivePurgeScheduledWork")
          .doTry()
              .bean("archivePurge", "purge")
          .doCatch(Throwable.class)
              .to("log:archivePurge?level=ERROR")
          .endDoTry()
          .end()
          .setId("archivePurgeScheduledWork");
        // @formatter:on
    }
}
