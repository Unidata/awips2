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

package com.raytheon.uf.edex.plugin.bufrobs;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "bufrobs-ingest.xml", context
 * "bufrobs-camel"
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2024-07-29   2037701    lisa.singh  Initial creation (from auto-generated)
 * 2024-10-02   2037700    tgurney     Fix ambiguous method call
 *
 * </pre>
 */

public class BufrobsCamelRoutes extends EDEXRouteBuilder {

    public BufrobsCamelRoutes() {
    }

    @Override
    public void configure() throws Exception {
        //@formatter:off
        from("jms-durable:queue:Ingest.bufrobs")
          .setHeader("pluginName", constant("bufrobs"))
          .bean("stringToFile")
          .doTry()
          .split(method("bufrFileSeparator", "separate")).streaming()
          .doTry()
          .pipeline()
          .bean("bufrobsProcessor", "process")
          .bean("bufrObsRecordPopulator", "populate")
          .to("direct:persistIndexAlert")
          .endDoTry()
          .doCatch(Throwable.class)
          .to("log:bufrobs?level=ERROR")
          .endDoTry()
          .end()
          .endDoTry()
          .doCatch(Throwable.class)
          .to("log:bufrobs?level=ERROR")
          .endDoTry()
          .doFinally()
          .bean("bufrFileSeparator", "clean")
          .endDoTry()
          .end()
          .setId("bufrobsIngestRoute");
        //@formatter:on
    }
}
