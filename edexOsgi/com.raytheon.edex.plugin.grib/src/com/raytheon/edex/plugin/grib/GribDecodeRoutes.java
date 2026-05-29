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

package com.raytheon.edex.plugin.grib;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "grib-decode.xml", context "grib-decode"
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2024-07-11   2037701    aford       Initial creation (from auto-generated)
 * 2024-09-05   2037700    tgurney     Rename purge route to avoid name collision
 *
 * </pre>
 */

public class GribDecodeRoutes extends EDEXRouteBuilder {

    private final String gribSplitThreads;

    private final String gribDecodeThreads;

    private final String gridPostprocessThreads;

    public GribDecodeRoutes(String gribSplitThreads, String gribDecodeThreads,
            String gridPostprocessThreads) {
        this.gribSplitThreads = gribSplitThreads;
        this.gribDecodeThreads = gribDecodeThreads;
        this.gridPostprocessThreads = gridPostprocessThreads;
    }

    @Override
    public void configure() throws Exception {
        // @formatter:off

        // Begin Grib Decode Route
        from("jms-durable:queue:Ingest.GribSplit?concurrentConsumers=" + this.gribSplitThreads)
              .doTry()
                      .pipeline()
                              .setHeader("pluginName", constant("grid"))
                              .bean("stringToFile")
                              .split(method("gribSplitter", "split")).streaming()
                              .to("jms-durable:queue:Ingest.GribDecode")
                      .end()
              .endDoTry()
              .doCatch(Throwable.class)
                      .to("log:grib?level=ERROR")
              .endDoTry()
              .end()
              .setId("gribSplitIngestRoute");

        from("jms-durable:queue:Ingest.GribDecode?concurrentConsumers=" + this.gribDecodeThreads)
              .doTry()
                      .pipeline()
                              .bean("gribGridPointLock", "reserve")
                              .bean("gribDecoder")
                              // send for processing
                              .bean("gribPostProcessor", "process")
                              .to("direct:gridPersistIndexAlert")
              .endDoTry()
              .doCatch(Throwable.class)
                      .to("log:grib?level=ERROR")
              .doFinally()
                      .bean("gribGridPointLock", "release")
              .endDoTry()
              .end()
              .setId("gribDecodeIngestRoute");

        // Copy of persist route with special grid duplicate elimination.
        from("direct:gridPersistIndexAlert")
              .bean("persist", "persist")
              // Eliminate duplicates for the case of stitched grids
              .bean("gridPersistUtils", "eliminateAndAuditDuplicates")
              .bean("index", "index")
              .bean("processUtil", "log")
              .to("direct:stageNotification")
              .setId("gridPersistIndexAlert");

        // Does a second round of post processing to generate new records
        // derived from recently persisted records.
        from("jms-durable:queue:Grid.PostProcess?concurrentConsumers=" + this.gridPostprocessThreads)
              .doTry()
                      .pipeline()
                              .bean("serializationUtil", "transformFromThrift")
                              // send for processing
                              .bean("gribPostProcessor", "processPersisted")
                              .to("direct:gridPersistIndexAlert")
              .endDoTry()
              .doCatch(Throwable.class)
                      .to("log:grib?level=ERROR")
              .endDoTry()
              .end()
              .setId("gridPostProcessRoute");

        // Handles purging from the gridcoverage caches in local instance of GridCoverageLookup
        from("jms-generic:topic:purgeGridCoverageCaches")
              .bean("gribSpatialCache", "purgeCaches")
              .setId("purgeGridCoverageCachesLocal");

        // @formatter:on
    }
}
