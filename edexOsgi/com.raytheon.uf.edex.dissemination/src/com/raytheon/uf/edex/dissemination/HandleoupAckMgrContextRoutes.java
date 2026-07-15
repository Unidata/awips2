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

package com.raytheon.uf.edex.dissemination;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "dissemination-request.xml", context "handleoupAckMgrContext"
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


public class HandleoupAckMgrContextRoutes extends EDEXRouteBuilder {

    public HandleoupAckMgrContextRoutes() {
    }

    @Override
    public void configure() throws Exception {
        from("jms-durable:queue:Ingest.handleoup")
          .doTry()
              .bean("stringToFile")
              .bean("manualProc")
              .to("jms-durable:queue:handleoup.dropbox")
          .doCatch(Throwable.class)
              .to("log:oup?level=ERROR&showBody=true")
          .endDoTry()
          .end()
          .setId("handleoupFilePush");
        from("jms-generic:topic:mhs.ackmgr")
          .doTry()
              .bean("oupAckMgr", "processAck")
          .doCatch(Throwable.class)
              .to("log:oup?level=INFO")
          .endDoTry()
          .end()
          .setId("oupAckMGrRoute");
    }
}
