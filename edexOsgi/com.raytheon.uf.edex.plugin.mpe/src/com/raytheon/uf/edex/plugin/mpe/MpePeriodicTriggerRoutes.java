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

package com.raytheon.uf.edex.plugin.mpe;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "./mpe-periodic-triggered.xml", context
 * "mpePeriodicTrigger-context"
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2024-08-21   2037701    lisa.singh   Initial creation (from auto-generated)
 *
 * </pre>
 */

public class MpePeriodicTriggerRoutes extends EDEXRouteBuilder {

    private final String biasmesgenCron;

    private final String alarmwhfsCron;

    private final String mpefieldgenCron;

    public MpePeriodicTriggerRoutes(String biasmesgenCron, String alarmwhfsCron,
            String mpefieldgenCron) {
        this.biasmesgenCron = biasmesgenCron;
        this.alarmwhfsCron = alarmwhfsCron;
        this.mpefieldgenCron = mpefieldgenCron;
    }

    @Override
    public void configure() throws Exception {
        /*
         * This will be the initial location for all MPE processes that are
         * triggered based on a timer. TODO: as more of the conversion is
         * completed, ideally we will be able to transition more MPE processes
         * to be triggered based on data arrival rather than a timer. The
         * objective will be to use many of the cron times already defined in:
         * com.raytheon.uf.edex.ohd/resources/com.raytheon.uf.edex.ohd.
         * properties.
         */
        // @formatter:off
        from("clusteredcron://mpe/mpefieldgenPeriodic/?schedule=" + this.biasmesgenCron)
          .doTry()
              .bean("biasmesgen", "execute")
          .doCatch(Throwable.class)
              .to("log:mpefieldgen?level=ERROR")
          .endDoTry()
          .end()
          .setId("mpefieldgenPeriodic");
        
        from("clusteredcron://mpe/alarmWhfsPeriodic/?schedule=" + this.alarmwhfsCron)
          .doTry()
              .bean("hpeFieldgen", "execute")
              .bean("rocChecker", "execute")
          .doCatch(Throwable.class)
              .to("log:alarmWhfs?level=ERROR")
          .endDoTry()
          .end()
          .setId("alarmWhfsPeriodic");
        
        from("clusteredcron://mpe/mpeBuildHourlyScheduled/?schedule=" + this.mpefieldgenCron)
          .doTry()
              .bean("buildHourly", "runBuildHourly")
          .doCatch(Throwable.class)
              .to("log:mpeBuildHourly?level=ERROR")
          .endDoTry()
          .end()
          .setId("mpeBuildHourlyScheduled");
        // @formatter:on
    }
}
