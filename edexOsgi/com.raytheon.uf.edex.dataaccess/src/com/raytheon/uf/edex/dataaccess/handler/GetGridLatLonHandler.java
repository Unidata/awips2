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
package com.raytheon.uf.edex.dataaccess.handler;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.CRS;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.referencing.FactoryException;

import com.raytheon.uf.common.dataaccess.request.GetGridLatLonRequest;
import com.raytheon.uf.common.dataaccess.response.GetGridLatLonResponse;
import com.raytheon.uf.common.geospatial.LatLonReprojection;
import com.raytheon.uf.common.geospatial.LatLonWrapper;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * 
 * Handler for {@link GetGridLatLonRequest}.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Oct 18, 2016  5916     bsteffen  Initial creation
 * 
 * </pre>
 *
 * @author bsteffen
 */
public final class GetGridLatLonHandler
        implements IRequestHandler<GetGridLatLonRequest> {

    @Override
    public GetGridLatLonResponse handleRequest(GetGridLatLonRequest request)
            throws MismatchedDimensionException, FactoryException {
        GridEnvelope2D range = new GridEnvelope2D(0, 0, request.getNx(),
                request.getNy());
        ReferencedEnvelope envelope = new ReferencedEnvelope(
                request.getEnvelope(), CRS.parseWKT(request.getCrsWkt()));
        GridGeometry2D gridGeometry = new GridGeometry2D(range, envelope);
        GetGridLatLonResponse response = new GetGridLatLonResponse();
        LatLonWrapper latLonData = LatLonReprojection.getLatLons(gridGeometry);
        response.setNx(request.getNx());
        response.setNy(request.getNy());
        response.setLats(latLonData.getLats());
        response.setLons(latLonData.getLons());
        return response;
    }
}
