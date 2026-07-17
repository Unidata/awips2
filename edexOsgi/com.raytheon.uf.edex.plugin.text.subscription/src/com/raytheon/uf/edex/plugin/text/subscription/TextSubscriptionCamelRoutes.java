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

package com.raytheon.uf.edex.plugin.text.subscription;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "text-subscription-ingest.xml", context
 * "text-subscription-camel"
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

public class TextSubscriptionCamelRoutes extends EDEXRouteBuilder {

    private final String purgeTextTriggerFilesCron;

    private final String subscriptionCron;

    public TextSubscriptionCamelRoutes(String purgeTextTriggerFilesCron,
            String subscriptionCron) {
        this.purgeTextTriggerFilesCron = purgeTextTriggerFilesCron;
        this.subscriptionCron = subscriptionCron;
    }

    @Override
    public void configure() throws Exception {
        //@formatter:off
        from("clusteredcron://textSubscription/purgeTextTriggerFiles/?schedule=" + this.purgeTextTriggerFilesCron)
          .doTry()
              .bean("textTriggerFilePurger", "purge")
          .doCatch(Throwable.class)
              .to("log:subscription?level=ERROR")
          .endDoTry()
          .end()
          .setId("purgeTextTriggerFiles");

        // AutoFax route
        from("seda:autoFaxRoute")
          .doTry()
              .bean("autoFaxManager", "processEvent")
          .doCatch(Throwable.class)
              .to("log:autoFax?level=ERROR")
          .endDoTry()
          .end()
          .setId("autoFaxRoute");

        // Quartz Timer triggered script runner
        from("cron:runner/runnerScheduled?schedule=" + this.subscriptionCron)
          .doTry()
              .bean("timerScriptRunner", "runScripts")
          .doCatch(Throwable.class)
              .to("log:subscription?level=ERROR")
          .endDoTry()
          .end()
          .setId("runnerScheduled");
        from("direct:textToWatchWarn")
          .bean("textDecoder", "transformToProductIds")
          .to("jms-durable:queue:watchwarn")
          .setId("textToWatchWarnRoute");

        // Watch/Warn triggered script runner
        from("jms-durable:queue:watchwarn")
          .doTry()
              .bean("textScriptRunner", "runScripts")
          .doCatch(Throwable.class)
              .to("log:subscription?level=ERROR")
          .endDoTry()
          .end()
          .setId("watchWarn");
        //@formatter:on
    }
}
