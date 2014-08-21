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

import com.raytheon.uf.common.dataplugin.warning.gis.GenerateGeospatialTimeSetRequest;
import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialTimeSet;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Generate geospatial time set data on demand.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 19, 2014 3353       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class GenerateGeospatialTimeSetRequestHandler implements
        IRequestHandler<GenerateGeospatialTimeSetRequest> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public GeospatialTimeSet handleRequest(
            GenerateGeospatialTimeSetRequest request) throws Exception {
        return GeospatialDataGenerator.getGeospatialTimeset(request.getSite());
    }
}
