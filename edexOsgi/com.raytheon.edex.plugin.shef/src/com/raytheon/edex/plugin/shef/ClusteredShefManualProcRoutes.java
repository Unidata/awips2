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

package com.raytheon.edex.plugin.shef;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "shef-ingest.xml", context
 * "clusteredShefManualProc"
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

// @formatter:off
/* Original XML context
 *     <camelContext id="clusteredShefManualProc" xmlns="http://camel.apache.org/schema/spring"
        errorHandlerRef="errorHandler">
        <endpoint id="shefManualFileEndpoint"
            uri="file:${edex.home}/data/share/hydroapps/shefdecode/input?delete=true&amp;maxMessagesPerPoll=1000&amp;delay=15000&amp;exclusiveReadLockStrategy=#shefFileChangedStrategy" />

        <route id="shefManualFileScan">
            <from uri="shefManualFileEndpoint" />
            <bean ref="manualProc" method="copyFileToArchive" />
            <bean ref="manualProc" />
            <to
                uri="jms-durable:queue:Ingest.ShefManual"/>
        </route>
    </camelContext>
 */
// @formatter:on

public class ClusteredShefManualProcRoutes extends EDEXRouteBuilder {

    private final String edexHome;

    public ClusteredShefManualProcRoutes(String edexHome) {
        this.edexHome = edexHome;
    }

    @Override
    public void configure() throws Exception {
        // @formatter:off
        from("file:" + this.edexHome + 
                "/data/share/hydroapps/shefdecode/input?delete=true&maxMessagesPerPoll=1000&delay=15000&exclusiveReadLockStrategy=#bean:shefFileChangedStrategy")
          .bean("manualProc", "copyFileToArchive")
          .bean("manualProc")
          .to("jms-durable:queue:Ingest.ShefManual")
          .setId("shefManualFileScan");
        // @formatter:on
    }
}
