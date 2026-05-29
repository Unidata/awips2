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

package com.raytheon.uf.edex.plugin.modelsounding;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "modelsounding-ingest.xml", context "modelsounding-camel"
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


public class ModelsoundingCamelRoutes extends EDEXRouteBuilder {

    public ModelsoundingCamelRoutes() {
    }

    @Override
    public void configure() throws Exception {
        from("jms-durable:queue:Ingest.modelsounding")
        // Begin Model Sounding routes
          .setHeader("pluginName", constant("modelsounding"))
              .doTry()
                  .pipeline()
                      .bean("stringToFile")
                      .bean("modelsoundingDecoder", "decode")
                      //  model sounding decoder has dedicated store thread, only need to forward to log 
                      .bean("processUtil", "log")
              .endDoTry()
              .doCatch(Throwable.class)
                  .to("log:modelsounding?level=ERROR")
              .endDoTry()
          .end()
          .setId("modelsndgIngestRoute");
        
        // Thread runs for life of context
        from("timer://modelSoundingPersistenceManager?repeatCount=1")
          .bean("modelsoundingPersistenceManager", "run")
          .setId("modelSoundingPersistThread");
        
        /*
         Copy of persist route without the log call.
         This route must come after the timer route for proper startup/shutdown order.
        */
        from("direct:modelSoundingPersistIndexAlert")
          .bean("persist", "persist")
          .bean("index", "index")
          .to("direct:stageNotification")
          .setId("modelSoundingPersistIndexAlert");
    }
}
