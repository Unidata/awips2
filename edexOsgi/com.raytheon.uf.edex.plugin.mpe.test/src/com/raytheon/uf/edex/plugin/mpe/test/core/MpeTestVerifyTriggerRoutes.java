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

package com.raytheon.uf.edex.plugin.mpe.test.core;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "mpe-convert-verify.xml", context
 * "mpeTestVerifyTrigger-context"
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

public class MpeTestVerifyTriggerRoutes extends EDEXRouteBuilder {

    public MpeTestVerifyTriggerRoutes() {
    }

    @Override
    public void configure() throws Exception {
        /*
         * MPE Verification Routes. Runs a process to verify that data produced
         * by the new route matches data produced by the native route. Setup to
         * run every hour for the previous hour.
         */
        // @formatter:off
        from("clusteredcron://mpe/mpeTestVerifyScheduled/?schedule=0+0+*+*+*+?")
          .doTry()
              .bean("mpeTestDriver", "runTests")
          .doCatch(Throwable.class)
              .to("log:mpeTestVerify?level=ERROR")
          .endDoTry()
          .end()
          .setId("mpeTestVerifyScheduled");
        // @formatter:on
    }
}
