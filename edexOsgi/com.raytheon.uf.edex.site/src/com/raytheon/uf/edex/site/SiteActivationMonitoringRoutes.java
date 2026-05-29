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
package com.raytheon.uf.edex.site;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "site-request.xml", context "siteActivationMonitoring"
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


public class SiteActivationMonitoringRoutes extends EDEXRouteBuilder {

    public SiteActivationMonitoringRoutes() {
    }

    @Override
    public void configure() throws Exception {
        from("jms-generic:topic:siteActivateNotification")
          .bean("serializationUtil", "transformFromThrift")
          .filter(method("siteActivateNotifyFilter", "isSiteActivateNotification"))
              .bean("siteActivationMonitor", "handleNotification")
              .filter(simple("${body} == null"))
                  .stop()
              .end()
              .bean("serializationUtil", "transformToThrift")
              .to("jms-generic:topic:edex.alerts.siteActivate?timeToLive=60000")
          .end()
          .setId("siteActivateNotify");
    }
}
