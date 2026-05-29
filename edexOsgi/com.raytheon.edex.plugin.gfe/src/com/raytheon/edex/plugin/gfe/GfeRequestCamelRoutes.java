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

package com.raytheon.edex.plugin.gfe;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "gfe-request.xml", context
 * "gfe-request-camel"
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2024-07-11   2037702    aford       Initial creation (from auto-generated)
 *
 * </pre>
 */

public class GfeRequestCamelRoutes extends EDEXRouteBuilder {

    private final String gfeCron;

    private final String purgeGfeLogsCron;

    private final String purgeGfeProductsIscCron;

    private final String purgeGfeProductsAtblCron;

    private final String purgeSvcbuLogsCron;

    private final String clearGfeOrphanedLocksCron;

    private final String iscdatarecThreads;

    public GfeRequestCamelRoutes(String gfeCron, String purgeGfeLogsCron,
            String purgeGfeProductsIscCron, String purgeGfeProductsAtblCron,
            String purgeSvcbuLogsCron, String clearGfeOrphanedLocksCron,
            String iscdatarecThreads) {
        this.gfeCron = gfeCron;
        this.purgeGfeLogsCron = purgeGfeLogsCron;
        this.purgeGfeProductsIscCron = purgeGfeProductsIscCron;
        this.purgeGfeProductsAtblCron = purgeGfeProductsAtblCron;
        this.purgeSvcbuLogsCron = purgeSvcbuLogsCron;
        this.clearGfeOrphanedLocksCron = clearGfeOrphanedLocksCron;
        this.iscdatarecThreads = iscdatarecThreads;
    }

    @Override
    public void configure() throws Exception {
        //@formatter:off
        from("clusteredcron://gfe/exportDigitalData/?schedule=" + this.gfeCron)
          .to("jms-generic:queue:exportDigitalDataWork")
          .setId("exportDigitalData");
        from("jms-generic:queue:exportDigitalDataWork")
          .doTry()
              .bean("ExportGridsRequestHandler", "exportGridsCron")
          .doCatch(Throwable.class)
              .to("log:svcBackup?level=ERROR")
          .endDoTry()
          .end()
          .setId("exportDigitalDataWork");
        from("clusteredcron://gfe/purgeGfeLogs/?schedule=" + this.purgeGfeLogsCron)
          .to("jms-generic:queue:purgeGfeLogWork")
          .setId("purgeGfeLogs");
        from("jms-generic:queue:purgeGfeLogWork")
          .doTry()
              .bean("logPurger", "purge")
          .doCatch(Throwable.class)
              .to("log:svcBackup?level=ERROR")
          .endDoTry()
          .end()
          .setId("purgeGfeLogWork");
        from("clusteredcron://gfe/purgeGfeProductsIsc/?schedule=" + this.purgeGfeProductsIscCron)
          .to("jms-generic:queue:purgeGfeProductIscWork")
          .setId("purgeGfeProductsIsc");
        from("jms-generic:queue:purgeGfeProductIscWork")
          .doTry()
              .bean("productIscPurger", "purge")
          .doCatch(Throwable.class)
              .to("log:svcBackup?level=ERROR")
          .endDoTry()
          .end()
          .setId("purgeGfeProductIscWork");
        from("clusteredcron://gfe/purgeGfeProductsAtbl/?schedule=" + this.purgeGfeProductsAtblCron)
          .to("jms-generic:queue:purgeGfeProductAtblWork")
          .setId("purgeGfeProductsAtbl");
        from("jms-generic:queue:purgeGfeProductAtblWork")
          .doTry()
              .bean("productAtblPurger", "purge")
          .doCatch(Throwable.class)
              .to("log:svcBackup?level=ERROR")
          .endDoTry()
          .end()
          .setId("purgeGfeProductAtblWork");
        from("clusteredcron://gfe/purgeSvcbuLogs/?schedule=" + this.purgeSvcbuLogsCron)
          .to("jms-generic:queue:purgeSvcbuLogWork")
          .setId("purgeSvcbuLogs");
        from("jms-generic:queue:purgeSvcbuLogWork")
          .doTry()
              .bean("svcBuLogPurger", "purge")
          .doCatch(Throwable.class)
              .to("log:svcBackup?level=ERROR")
          .endDoTry()
          .end()
          .setId("purgeSvcbuLogWork");
        from("clusteredcron://gfe/clearGfeOrhpanedLocks/?schedule=" + this.clearGfeOrphanedLocksCron)
          .to("jms-generic:queue:clearOrphanedLocksWork")
          .setId("clearGfeOrphanedLocks");
        from("jms-generic:queue:clearOrphanedLocksWork")
          .doTry()
              .bean("ClearGfeOrphanedLocks", "clearLocksCron")
          .doCatch(Throwable.class)
              .to("log:svcBackup?level=ERROR")
          .endDoTry()
          .end()
          .setId("clearOrphanedLocksWork");
        from("jms-durable:queue:gfeIscDataReceive?concurrentConsumers=" + this.iscdatarecThreads)
          .doTry()
              .pipeline()
                  .bean("serializationUtil", "transformFromThrift")
                  .bean("iscReceiveSrv", "processRequest")
          .endDoTry()
          .doCatch(Throwable.class)
              .to("log:iscDataRec?level=ERROR")
          .endDoTry()
          .end()
          .setId("iscReceiveRoute");
        from("jms-generic:topic:iscMosaicStatusNotify")
          .bean("serializationUtil", "transformFromThrift")
          .to("bean:iscMosaicJobManager?method=handleStatusMessage(${body})")
          .setId("iscMosaicStatusNotifyRoute");
        //@formatter:on
    }
}
