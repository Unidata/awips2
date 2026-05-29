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

package com.raytheon.edex.plugin.radar;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "radar-ingest.xml", context "radar-camel"
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

public class RadarCamelRoutes extends EDEXRouteBuilder {

    private final String radarDecodeSbnThreads;

    private final String radarDecodeLocalThreads;

    public RadarCamelRoutes(String radarDecodeSbnThreads,
            String radarDecodeLocalThreads) {
        this.radarDecodeSbnThreads = radarDecodeSbnThreads;
        this.radarDecodeLocalThreads = radarDecodeLocalThreads;
    }

    @Override
    public void configure() throws Exception {
        // @formatter:off
        from("jms-durable:queue:Ingest.Radar?concurrentConsumers=" + this.radarDecodeSbnThreads)
          .setHeader("dataType", constant("radar-sbn"))
              .to("direct:radarcommon")
              .setId("radarIngestRoute");
        from("jms-durable:queue:Ingest.RadarRadarServer?concurrentConsumers=" + this.radarDecodeLocalThreads)
          .setHeader("dataType", constant("radar-local"))
              .to("direct:radarcommon")
              .setId("radarRadarServerIngestRoute");
        from("direct:radarcommon")
          .setHeader("pluginName", constant("radar"))
              .doTry()
                  .pipeline()
                      .bean("stringToFile")
                      .bean("radarDecompressor", "decompress")
                      .bean("radarDecoder", "decode")
                      .to("direct:persistIndexAlert")
              .endDoTry()
              .doCatch(com.raytheon.uf.common.dataplugin.exception.MalformedDataException.class)
                  .to("direct:logFailureAsInfo")
              .endDoTry()
              .doCatch(Throwable.class)
                  .to("direct:logFailedData")
              .endDoTry()
          .end()
          .setId("radarCommonIngestRoute");
        from("seda:storeRadarTextProduct")
          .to("direct:textDirectDecodedIngestRoute")
          .setId("radarTextProductRoute");
        // @formatter:on
    }
}
