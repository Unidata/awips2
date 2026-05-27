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
 * Camel routes converted from file "hpeDHRDecoder-spring.xml", context "nonClusteredDhrDspRoutes"
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2024-08-01   2037701    lisa.singh   Initial creation (from auto-generated)
 *
 * </pre>
 */


public class NonClusteredDhrDspRoutes extends EDEXRouteBuilder {

    public NonClusteredDhrDspRoutes() {
    }

    @Override
    public void configure() throws Exception {
        /* DHR and DSP files both come in from Ingest.dhr URI, and are then sent
           to a single legacyDhrDspIngestRoute URI (@see ClusteredDhrDspRoutes) for legacy processing and
           separate dxxIngestRoute URIs for Java processing 
           (all handled in ClusteredDhrDspRoutes and NonClusteredDhrDspRoutes) */
        
        from("jms-durable:queue:Ingest.dhr")
          .setHeader("pluginName", constant("dhr"))
          .doTry()
              .pipeline()
                  .bean("setIngestHeaderFields")
                  .bean("stringToFile")
                  .bean("dhrRadarDecompressor", "decompressWithHeader")
                  .bean("dhrDecodeSrv", "filter")
          .endDoTry()
          .doCatch(Throwable.class)
              .to("log:dhr?level=INFO")
          .endDoTry()
          .end()
          .setId("dhrDspIngestFilter");
    }
}
