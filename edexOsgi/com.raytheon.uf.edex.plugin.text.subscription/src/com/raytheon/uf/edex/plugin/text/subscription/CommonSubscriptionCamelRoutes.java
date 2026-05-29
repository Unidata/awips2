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
 * Camel routes converted from file "text-subscription-common.xml", context "common-subscription-camel"
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


public class CommonSubscriptionCamelRoutes extends EDEXRouteBuilder {

    public CommonSubscriptionCamelRoutes() {
    }

    @Override
    public void configure() throws Exception {
        from("direct:watchWarnDirect")
          .doTry()
              .bean("textScriptRunner", "runScripts")
          .doCatch(Throwable.class)
              .to("log:subscription?level=ERROR")
          .endDoTry()
          .end()
          .setId("watchWarnDirect");
        from("jms-generic:topic:autofax.notify")
          .doTry()
              .bean("autoFaxDao", "updateCache")
          .doCatch(Throwable.class)
              .to("log:autoFax?level=ERROR")
          .endDoTry()
          .end()
          .setId("autoFaxNotify");
        from("jms-generic:topic:subscription.notify")
          .doTry()
              .bean("subscriptionDao", "updateCache")
          .doCatch(Throwable.class)
              .to("log:subscription?level=ERROR")
          .endDoTry()
          .end()
          .setId("subscriptionNotify");
    }
}
