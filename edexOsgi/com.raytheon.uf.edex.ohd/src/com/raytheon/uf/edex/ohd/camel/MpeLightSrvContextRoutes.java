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
 * Camel routes converted from file "mpeLightningSrv-ingest.xml", context
 * "mpeLightSrv-context"
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

public class MpeLightSrvContextRoutes extends EDEXRouteBuilder {

    private final String mpelightningsrvCron;

    public MpeLightSrvContextRoutes(String mpelightningsrvCron) {
        this.mpelightningsrvCron = mpelightningsrvCron;
    }

    @Override
    public void configure() throws Exception {
        //@formatter:off
        from("clusteredcron://pproc/mpeLightSrvScheduled/?schedule=" + this.mpelightningsrvCron)
          .to("jms-generic:queue:mpeLightSrvScheduledWork")
          .setId("mpeLightSrvScheduled");
        from("jms-generic:queue:mpeLightSrvScheduledWork")
          .to("bean:mpeLightSrv?method=runOnSchedule")
          .setId("mpeLightSrvScheduledWork");
        //@formatter:on
    }
}
