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
 * Camel routes converted from file "alarmWhfs-spring.xml", context
 * "alarmWhfs-context"
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2024-08-01   2037701    lisa.singh  Initial creation (from auto-generated)
 * 2024-08-25   2037700    tgurney     Fix endParent code generation bug
 *
 * </pre>
 */

public class AlarmWhfsContextRoutes extends EDEXRouteBuilder {

    private final String alarmwhfsCron;

    public AlarmWhfsContextRoutes(String alarmwhfsCron) {
        this.alarmwhfsCron = alarmwhfsCron;
    }

    @Override
    public void configure() throws Exception {
        //@formatter:off
        from("clusteredcron://whfs/alarmWhfsScheduled/?schedule=" + this.alarmwhfsCron)
          .to("jms-generic:queue:alarmWhfsScheduledWork")
          .setId("alarmScheduled");
        from("jms-generic:queue:alarmWhfsScheduledWork")
          .multicast()
              .doTry()
                  .bean("alarmSrv", "execute")
              .doCatch(Throwable.class)
                  .to("log:alarmWhfs?level=ERROR")
                  .endDoTry()
              .end()
              .doTry()
                  .bean("runReportAlarmSrv", "executeRunReportAlarm")
              .doCatch(Throwable.class)
                  .to("log:alarmWhfs?level=ERROR")
                  .endDoTry()
              .end()
          .end()
          .setId("alarmWhfsScheduledWork");
        //@formatter:on
    }
}
