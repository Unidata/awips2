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

package com.raytheon.uf.edex.plugin.vaa;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "vaa-ingest.xml", context "vaa-camel"
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


public class VAACamelRoutes extends EDEXRouteBuilder {

    public VAACamelRoutes() {
    }

    @Override
    public void configure() throws Exception {
        from("jms-durable:queue:Ingest.vaa")
          .setHeader("pluginName", constant("vaa"))
              .pipeline()
                  .doTry()
                      .pipeline()
                          .bean("stringToFile")
                          .bean("vaaDecoder", "decode")
                          .to("direct:indexAlert")
                  .endDoTry()
                  .doCatch(com.raytheon.uf.common.dataplugin.exception.MalformedDataException.class)
                      .to("direct:logFailureAsInfo")
                  .endDoTry()
                  .doCatch(Throwable.class)
                      .to("direct:logFailedData")
                  .endDoTry()
              .end()
          .end()
          .setId("vaaIngestRoute");
    }
}
