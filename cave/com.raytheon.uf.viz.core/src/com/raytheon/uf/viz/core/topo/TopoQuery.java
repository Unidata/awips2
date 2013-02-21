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
package com.raytheon.uf.viz.core.topo;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.topo.ITopoQuery;
import com.raytheon.uf.common.topo.TopoQueryRequest;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Viz client class to mimic functionality of edex TopoQuery, uses ThriftClient
 * instead of direct hdf5 access if DataMode == THRIFT otherwise uses the
 * com.raytheon.edex.topo.TopoQuery
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 12, 2009            mschenke     Initial creation
 * Jan 14, 2013 1469       bkowal       The hdf5 root directory is no longer passed
 *                                      as an argument to the common TopoQuery constructor.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TopoQuery implements ITopoQuery {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TopoQuery.class);

    private TopoQueryRequest request;

    /**
     * @return Initialized TopoQuery instance
     */
    public static synchronized ITopoQuery getInstance() {
        return com.raytheon.uf.edex.topo.TopoQuery.getInstance(0);
    }

    /**
     * @return Initialized TopoQuery instance
     */
    public static synchronized ITopoQuery getInstance(int topoLevel,
            boolean useCaching) {
        return com.raytheon.uf.edex.topo.TopoQuery.getInstance(0);
    }

    private TopoQuery(int level, boolean useCaching) {
        request = new TopoQueryRequest();
        request.setLevel(level);
        request.setUseCaching(useCaching);
    }

    /**
     * Retrieves topo height in meters above mean sea level for the specified
     * coordinate.
     * 
     * @param coord
     *            should contain lon/lat in degrees
     * @return
     */
    public double getHeight(Coordinate coord) {
        request.setCoordinates(new Coordinate[] { coord });
        request.setGridGeometry(null);
        try {
            return (Double) ThriftClient.sendRequest(request);
        } catch (VizException e) {
            statusHandler.handle(Priority.CRITICAL,
                    "Unable to perform topo query", e);
            return Double.NaN;
        }
    }

    /**
     * Retrieves topo height in meters above mean sea level for the specified
     * coordinates.
     * 
     * @param coords
     *            should contain lon/lat in degrees
     * @return
     */
    public double[] getHeight(Coordinate[] coords) {
        request.setCoordinates(coords);
        request.setGridGeometry(null);
        try {
            return (double[]) ThriftClient.sendRequest(request);
        } catch (VizException e) {
            statusHandler.handle(Priority.CRITICAL,
                    "Unable to perform topo query", e);
            return null;
        }
    }

    /**
     * Retrieves topo height in meters above mean sea level reprojected and
     * interpolated to the specified grid geometry.
     * 
     * @param targetGeom
     * @return the topo data array in row major order
     */
    public float[] getHeight(GridGeometry2D targetGeom) {
        request.setCoordinates(null);
        request.setGridGeometry(targetGeom);
        try {
            return (float[]) ThriftClient.sendRequest(request);
        } catch (VizException e) {
            statusHandler.handle(Priority.CRITICAL,
                    "Unable to perform topo query", e);
            return null;
        }
    }
}
