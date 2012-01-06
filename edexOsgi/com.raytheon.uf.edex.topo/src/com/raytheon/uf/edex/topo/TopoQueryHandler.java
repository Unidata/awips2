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
package com.raytheon.uf.edex.topo;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.topo.TopoQueryRequest;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TopoQueryHandler implements IRequestHandler<TopoQueryRequest> {

    @Override
    public Object handleRequest(TopoQueryRequest request) throws Exception {
        TopoQuery query = TopoQuery.getInstance(request.getLevel());
        Coordinate[] coords = request.getCoordinates();
        GridGeometry2D gridGeom = request.getGridGeometry();
        if (coords != null) {
            if (coords.length == 1) {
                return query.getHeight(coords[0]);
            } else {
                return query.getHeight(coords);
            }
        } else if (gridGeom != null) {
            return query.getHeight(gridGeom);
        } else {
            return null;
        }
    }

}
