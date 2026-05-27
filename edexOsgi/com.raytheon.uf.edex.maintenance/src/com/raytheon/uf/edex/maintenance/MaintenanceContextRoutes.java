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

package com.raytheon.uf.edex.maintenance;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "maintenance-ingest.xml", context
 * "maintenanceContext"
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

public class MaintenanceContextRoutes extends EDEXRouteBuilder {

    private final String repackCron;

    public MaintenanceContextRoutes(String repackCron) {
        this.repackCron = repackCron;
    }

    @Override
    public void configure() throws Exception {
        //@formatter:off
        // Repack on Scheduled timer
        from("clusteredcron://repack/repackScheduled/?schedule=" + this.repackCron)
          .doTry()
              .bean("dataStoreRepacker", "repack")
          .doCatch(Throwable.class)
              .to("log:repack?level=ERROR")
          .endDoTry()
          .end()
          .setId("repackScheduled");
        //@formatter:on
    }
}
