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
import com.raytheon.uf.edex.ohd.pproc.SatPreDataGenerator;

/**
 * Camel routes converted from file "satpre-spring.xml", context "satPreDataGenerator-context"
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2024-08-13   2037701    lisa.singh   Initial creation (from auto-generated)
 * </pre>
 */


public class SatPreDataGeneratorContextRoutes extends EDEXRouteBuilder {
    
    private final SatPreDataGenerator satPreDataGenerator;

    public SatPreDataGeneratorContextRoutes(SatPreDataGenerator satPreDataGenerator) {
        this.satPreDataGenerator = satPreDataGenerator;
    }

    @Override
    public void configure() throws Exception {
        from("jms-durable:queue:satGridAutospeFilter")
          .doTry()
              .pipeline()
                  .bean("serializationUtil", "transformFromThrift")
                  .bean("satPreDataGenerator", "process")
          .endDoTry()
          .doCatch(Throwable.class)
              .to("log:satPre?level=ERROR")
          .endDoTry()
          .end()
          .setId("satPreIngestRoute");
        from("jms-durable:queue:satGoes16RRQPEFilter")
          .doTry()
              .pipeline()
              .setHeader("pluginName", constant("precip"))
              .setHeader("dequeueTimeTime").method(this.satPreDataGenerator, "getQueueTime")
              .bean("serializationUtil", "transformFromThrift")
              .bean("satPreDataGenerator", "process")
              .to("direct:persistIndexAlert")
          .endDoTry()
          .doCatch(Throwable.class)
              .to("log:satPre?level=ERROR")
          .endDoTry()
          .end()
          .setId("satGoes16RRQPEIngestRoute");
    }
}
