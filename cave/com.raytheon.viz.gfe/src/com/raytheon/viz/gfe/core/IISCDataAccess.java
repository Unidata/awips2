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

package com.raytheon.viz.gfe.core;

import java.awt.Point;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.edittool.GridID;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * ISCDataAccess interface
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 07/14/09      1995       bphillip    Initial release
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public interface IISCDataAccess {

    /**
     * Returns the corresponding ISC grid id based on the incoming grid id. If
     * exact match is true, then the returned GridID is based on the incoming
     * time of the gridID. If exact match is false, then the returned GridID is
     * based on the inventory information from the primary and isc parms. In
     * this case, it will return the exact match if a grid exists, or the isc
     * grid that overlaps the incoming grid the most.l
     * 
     * @param id
     *            The Grid id of the non-ISC grid
     * @param exactMatch
     *            if the exact match should be returned
     * @return The GridID of the corresponding ISC grid
     */
    public GridID getISCGridID(GridID id, boolean exactMatch);

    /**
     * Returns the corresponding ISC parmID based on the incoming parmID. Does
     * not load the parm.
     * 
     * @param parmID
     *            The ParmID of the non-ISC grid
     * @return The corresponding ISC parmID
     */
    public ParmID getISCParmID(ParmID parmID);

    /**
     * Returns the corresponding ISC parm based on the incoming parm.
     * 
     * @param p
     *            The incoming parm
     * @return The corresponding ISC parm
     */
    public Parm getISCParm(Parm p);

    /**
     * Returns the site associated with the particular grid coordinate.
     * 
     * @param coord
     *            The grid coordinate
     * @param gid
     *            The GridID of the grid
     * @return The site associated with the grid coordinate
     */
    public String getISCSite(Point coord, GridID gid);

    /**
     * Returns true if the coordinate (and grid id) is in our own domain. The
     * officeType for the parm and our officeType must be the same to be
     * considered "inOurSite".
     * 
     * @param loc
     *            The coordinate to check
     * @param gid
     *            The gridID associated with the coordinate
     * @return true if coordinate is in our domain and the parm and office type
     *         match
     */
    public boolean inOurSite(Point loc, GridID gid);

    /**
     * Returns the data point for the gridid. Will return either the data point
     * from the specified grid, or its corresponding isc grid, depending upon
     * the location and the isc mode.
     * 
     * @param gridID
     *            The grid id of the coordinate to check
     * @param worldLoc
     *            The coordinate to check
     * @return The datap point for the gridid and coordinate
     * @throws GFEServerException
     *             if problems occur
     */
    public WxValue getDataPoint(GridID gridID, Coordinate worldLoc)
            throws GFEServerException;

    /**
     * Returns the data point for the gridid. Will return either the data point
     * from the specified grid, or its corresponding isc grid, depending upon
     * the location and the isc mode. Also returns the grid coordinate from the
     * world coord to grid location conversion.
     * 
     * @param gridID
     *            The grid id of the coordinate to check
     * @param worldLoc
     *            The world location of the conversion
     * @param gloc
     *            The grid location to check
     * @return The data point from the specified grid, or it's corresponding isc
     *         grid, depending upon the location and the isc mode
     * @throws GFEServerException
     *             If problems occur
     */
    public WxValue getDataPoint(GridID gridID, Coordinate worldLoc,
            Coordinate gloc) throws GFEServerException;

    /**
     * Returns a composite grid consisting of the primary grid and any
     * corresponding ISC grid, blended together based on the mask information
     * derived from the Grid Data History. Primary grid must exist. Returns the
     * set of points that are valid in the output grid. (Note the output grid
     * consists of the primary grid and isc grid. Any "invalid" points, indicate
     * those areas that have no isc data and are outside the home site's region.
     * The returned Grid2D will have the primary data in that area.) If the
     * primary grid is an ISC grid, then the data is not blended and the local
     * site id is only included if there is data in the ISC grid for the local
     * site.
     * 
     * @param gid
     *            The grid id
     * @param exactMatch
     *            If exact match is requested
     * @param gridSlice
     *            The gridSlice
     * @return The composite grid
     */
    public Grid2DBit getCompositeGrid(GridID gid, boolean exactMatch,
            ScalarGridSlice slice);

    /**
     * Returns a composite grid consisting of the primary grid and any
     * corresponding ISC grid, blended together based on the mask information
     * derived from the Grid Data History. Primary grid must exist. Returns the
     * set of points that are valid in the output grid. (Note the output grid
     * consists of the primary grid and isc grid. Any "invalid" points, indicate
     * those areas that have no isc data and are outside the home site's region.
     * The returned Grid2D will have the primary data in that area.) If the
     * primary grid is an ISC grid, then the data is not blended and the local
     * site id is only included if there is data in the ISC grid for the local
     * site.
     * 
     * @param gid
     *            The grid id
     * @param exactMatch
     *            Exact match is requested
     * @param gridSlice
     *            The vector grid gridSlice
     * @return The composite grid
     */
    public Grid2DBit getCompositeGrid(GridID gid, boolean exactMatch,
            VectorGridSlice slice);

    /**
     * Returns a composite grid consisting of the primary grid and any
     * corresponding ISC grid, blended together based on the mask information
     * derived from the Grid Data History. Primary grid must exist. Returns the
     * set of points that are valid in the output grid. (Note the output grid
     * consists of the primary grid and isc grid. Any "invalid" points, indicate
     * those areas that have no isc data and are outside the home site's region.
     * The returned Grid2D will have the primary data in that area.) If the
     * primary grid is an ISC grid, then the data is not blended and the local
     * site id is only included if there is data in the ISC grid for the local
     * site.
     * 
     * @param gid
     *            The grid id
     * @param exactMatch
     *            Exact match is requested
     * @param slice
     *            The grid gridSlice
     * @return The composite grid
     */
    public Grid2DBit getCompositeGrid(GridID gid, boolean exactMatch,
            DiscreteGridSlice slice);

    /**
     * Returns a composite grid consisting of the primary grid and any
     * corresponding ISC grid, blended together based on the mask information
     * derived from the Grid Data History. Primary grid must exist. Returns the
     * set of points that are valid in the output grid. (Note the output grid
     * consists of the primary grid and isc grid. Any "invalid" points, indicate
     * those areas that have no isc data and are outside the home site's region.
     * The returned Grid2D will have the primary data in that area.) If the
     * primary grid is an ISC grid, then the data is not blended and the local
     * site id is only included if there is data in the ISC grid for the local
     * site.
     * 
     * @param gid
     *            The grid id
     * @param exactMatch
     *            Exact match is requested
     * @param slice
     *            The grid gridSlice
     * @return The composite grid
     */
    public Grid2DBit getCompositeGrid(GridID gid, boolean exactMatch,
            WeatherGridSlice slice);
}
