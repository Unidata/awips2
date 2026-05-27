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

package com.raytheon.uf.edex.plugin.nswrc;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "nswrc-ingest.xml", context
 * "nswrc-fileRoutes"
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2024-07-29   2037701    lisa.singh   Initial creation (from auto-generated)
 * 2024-09-24   2037700    tgurney      Fix injection of strategies from XML
 *
 * </pre>
 */

public class NSWRCFileRoutes extends EDEXRouteBuilder {

    private final String edexHome;

    public NSWRCFileRoutes(String edexHome) {
        this.edexHome = edexHome;
    }

    @Override
    public void configure() throws Exception {
        from("file:" + this.edexHome
                + "/data/sbn/nswrc/radial?delete=true&delay=5000&maxMessagesPerPoll=1000&exclusiveReadLockStrategy=#bean:nswrcRadial_FileChangeStrategy")
                        .bean("fileToString")
                        .setHeader("pluginName", constant("nswrc_radial"))
                        .to("jms-durable:queue:Ingest.nswrcRadial")
                        .setId("nswrcRadialFileConsumerRoute");
        from("file:" + this.edexHome
                + "/data/sbn/nswrc/gridded?delete=true&delay=5000&maxMessagesPerPoll=1000&exclusiveReadLockStrategy=#bean:nswrcGridded_FileChangeStrategy")
                        .bean("fileToString")
                        .setHeader("pluginName", constant("grid"))
                        .to("jms-durable:queue:Ingest.nswrcGridded")
                        .setId("nswrcGriddedFileConsumerRoute");
    }
}
