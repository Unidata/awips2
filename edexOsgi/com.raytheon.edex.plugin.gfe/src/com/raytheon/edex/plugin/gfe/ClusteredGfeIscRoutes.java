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
 * Camel routes converted from file "gfe-request.xml", context "clusteredGfeIscRoutes"
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


public class ClusteredGfeIscRoutes extends EDEXRouteBuilder {

    public ClusteredGfeIscRoutes() {
    }

    @Override
    public void configure() throws Exception {
        from("jms-durable:queue:iscSendNotification")
          .doTry()
              .bean("serializationUtil", "transformFromThrift")
              .bean("iscSendQueue", "addSendJobs")
          .doCatch(Throwable.class)
              .to("log:iscSendQueue?level=ERROR")
          .endDoTry()
          .end()
          .setId("iscSendJobQueueAggr");
        from("timer://iscSendTimer?fixedRate=true&period=5000")
          .bean("iscSendQueue", "fireSendJobs")
          .setId("iscSendTrigger");
    }
}

