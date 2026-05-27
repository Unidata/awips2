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

package com.raytheon.uf.edex.plugin.text;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "text-ingest.xml", context "text-ingest-camel"
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

public class TextCamelRoutes extends EDEXRouteBuilder {

    public TextCamelRoutes() {
    }

    @Override
    public void configure() throws Exception {
        /*
         * Text routes. If an internal route is being sent data from
         * another internal route in the same context it needs to come after the
         * route that sends it data for proper startup/shutdown order.
         */
        from("jms-durable:queue:Ingest.Text?concurrentConsumers=2")
                .setHeader("pluginName", constant("text"))
                .doTry()
                    .pipeline()
                    .bean("stringToFile")
                    .bean("textDecoder", "decodeFile")
                    .bean("processUtil", "log")
                    .multicast()
                        .to("direct:textSerializeRoute")
                        .to("direct:stageNotification")
                    .end()
                .endDoTry()
                .doCatch(Throwable.class)
                    .to("log:text?level=INFO")
                .endDoTry()
                .end()
                .setId("textUndecodedIngestRoute");
        
        from("direct:textDirectDecodedIngestRoute")
                .setHeader("pluginName", constant("text"))
                .doTry()
                    .pipeline()
                        .bean("textDecoder", "writeTextProduct")
                        .bean("processUtil", "log")
                        .multicast()
                            .to("direct:textSerializeRoute")
                            .to("direct:stageNotification")
                        .end()
                .endDoTry()
                .doCatch(Throwable.class)
                    .to("log:text?level=INFO")
                .endDoTry()
                .end()
                .setId("textDirectDecodedIngestRoute");
        
        from("direct:textDirectUndecodedIngestRoute")
                .setHeader("pluginName", constant("text"))
                .doTry()
                    .pipeline()
                        .bean("textDecoder", "decode")
                        .bean("processUtil", "log")
                        .multicast()
                            .to("direct:textSerializeRoute")
                            .to("direct:stageNotification")
                        .end()
                .endDoTry()
                .doCatch(Throwable.class)
                    .to("log:text?level=INFO")
                .endDoTry()
                .end().setId("textDirectUndecodedIngestRoute");
        
        from("direct:textSerializeRoute")
                .split(method("textDecoder", "separator"))
                    .streaming()
                    .bean("textDecoder", "transformToSimpleString")
                    .bean("serializationUtil", "transformToThrift")
                    .to("jms-generic:topic:edex.alarms.msg?timeToLive=60000")
                .end()
                .setId("textSerializationRoute");
        
        from("jms-durable:queue:textToStageNotification")
                .bean("textDecoder", "transformStringToTextRecord")
                .to("direct:stageNotification")
                .setId("textToStageNotificationRoute");
    }
}
