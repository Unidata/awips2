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

package com.raytheon.uf.edex.ohd.camel;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "pointDataRetrievel-spring.xml", context
 * "pointDataWhfs-context"
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2024-08-13   2037701    lisa.singh   Initial creation (from auto-generated)
 *
 * </pre>
 */

public class PointDataWhfsContextRoutes extends EDEXRouteBuilder {

    private final String pointdataretrievalCron;

    public PointDataWhfsContextRoutes(String pointdataretrievalCron) {
        this.pointdataretrievalCron = pointdataretrievalCron;
    }

    @Override
    public void configure() throws Exception {
        //@formatter:off
        from("clusteredcron://whfs/pointDataWhfsScheduled/?schedule=" + this.pointdataretrievalCron)
          .to("jms-generic:queue:pointDataScheduledWork")
          .setId("pointDataScheduled");
        from("jms-generic:queue:pointDataScheduledWork")
          .doTry()
              .bean("pdcSrv", "execute")
          .doCatch(Throwable.class)
              .to("log:pointData?level=ERROR")
          .endDoTry()
          .end()
          .setId("pointDataScheduledWork");
        //@formatter:on
    }
}
