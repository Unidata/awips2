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

package com.raytheon.edex.plugin.warning.gis;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "warning-request.xml", context
 * "geospatialDataGenerator-camel"
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2024-07-11   2037702    aford       Initial creation (from auto-generated)
 *
 * </pre>
 */

public class GeospatialDataGeneratorCamelRoutes extends EDEXRouteBuilder {

    private final String geospatialUpdaterCron;

    public GeospatialDataGeneratorCamelRoutes(String geospatialUpdaterCron) {
        this.geospatialUpdaterCron = geospatialUpdaterCron;
    }

    @Override
    public void configure() throws Exception {
        //@formatter:off
        from("timer://generateWarngenGeometries?repeatCount=1")
          .bean("geospatialDataGenerator", "generateUniqueGeospatialMetadataGeometries")
          .setId("geospatialDataGeneratorRoute");
        from("clusteredcron://warning/checkGeospatialData/?schedule=" + this.geospatialUpdaterCron)
          .doTry()
              .bean("geospatialDataGenerator", "generateUniqueGeospatialMetadataGeometries")
          .doCatch(Throwable.class)
              .to("log:geospatialDataUpdater?level=ERROR")
          .endDoTry()
          .end()
          .setId("geospatialDataUpdaterRoute");
        from("seda:edex.geospatialUpdateNotification")
          .bean("serializationUtil", "transformToThrift")
          .to("jms-generic:topic:edex.geospatialUpdate.msg")
          .setId("geospatialUpdateNotify");
        //@formatter:on
    }
}
