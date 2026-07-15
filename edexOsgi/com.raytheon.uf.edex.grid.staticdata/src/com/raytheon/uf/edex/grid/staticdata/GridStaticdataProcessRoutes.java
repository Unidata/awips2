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

package com.raytheon.uf.edex.grid.staticdata;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "grid-staticdata-process.xml", context
 * "grid-staticdata-process"
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2024-07-11   2037701    aford       Initial creation (from auto-generated)
 *
 * </pre>
 */

public class GridStaticdataProcessRoutes extends EDEXRouteBuilder {

    public GridStaticdataProcessRoutes() {
    }

    @Override
    public void configure() throws Exception {
        // @formatter:off
        from("jms-durable:grid-staticdata-generate")
              .doTry()
                      .bean("serializationUtil", "transformFromThrift")
                      .bean("staticDataGenerator", "processNotification")
                      .to("direct:stageNotification")
              .doCatch(Throwable.class)
                      .to("log:grid-staticdata?level=ERROR")
              .endDoTry()
              .end()
              .setId("gridStaticDataGenerationRoute");
        // @formatter:on
    }
}
