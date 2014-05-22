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

import com.raytheon.uf.common.dataplugin.warning.gis.GenerateGeospatialDataRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Generates geospatial data on demand.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 22, 2011            rjpeter     Initial creation.
 * May 19, 2014 2726       rjpeter     Updated call to GeospatialDataGenerator.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class GenerateGeospatialDataRequestHandler implements
        IRequestHandler<GenerateGeospatialDataRequest> {

    final GeospatialDataGenerator dataGenerator;

    public GenerateGeospatialDataRequestHandler(
            GeospatialDataGenerator dataGenerator) {
        this.dataGenerator = dataGenerator;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public Object handleRequest(GenerateGeospatialDataRequest request)
            throws Exception {
        return dataGenerator.generateGeoSpatialList(request.getSite(),
                request.getMetaData());
    }

}
