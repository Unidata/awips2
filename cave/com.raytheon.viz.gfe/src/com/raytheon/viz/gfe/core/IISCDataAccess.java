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
import com.raytheon.uf.common.util.Pair;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.edittool.GridID;
import org.locationtech.jts.geom.Coordinate;

/**
 * ISCDataAccess interface
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------------------------------
 * Jul 14, 2009  1995     bphillip  Initial release
 * Sep 01, 2014  3572     randerso  Removed unused inOurSite method
 * Jan 04, 2018  7178     randerso  Changes to use IDataObject. Code cleanup
 *
 * </pre>
 *
 * @author bphillip
 */
public interface IISCDataAccess {

    /**
     * Returns the corresponding ISC grid id based on the incoming grid id. If
     * exact match is true, then the returned GridID is based on the incoming
     * time of the gridID. If exact match is false, then the returned GridID is
     * based on the inventory information from the primary and ISC parms. In
     * this case, it will return the exact match if a grid exists, or the ISC
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
     * Returns the data point for the gridID. Will return either the data point
     * from the specified grid, or its corresponding ISC grid, depending upon
     * the location and the ISC mode.
     *
     * @param gridID
     *            The grid id of the coordinate to check
     * @param worldLoc
     *            The coordinate to check
     * @return The datap point for the gridID and coordinate
     * @throws GFEServerException
     *             if problems occur
     */
    public WxValue getDataPoint(GridID gridID, Coordinate worldLoc)
            throws GFEServerException;

    /**
     * Returns the data point for the gridID. Will return either the data point
     * from the specified grid, or its corresponding ISC grid, depending upon
     * the location and the ISC mode. Also returns the grid coordinate from the
     * world coord to grid location conversion.
     *
     * @param gridID
     *            The grid id of the coordinate to check
     * @param worldLoc
     *            The world location of the conversion
     * @param gloc
     *            The grid location to check
     * @return The data point from the specified grid, or it's corresponding ISC
     *         grid, depending upon the location and the ISC mode
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
     * consists of the primary grid and ISC grid. Any "invalid" points, indicate
     * those areas that have no ISC data and are outside the home site's region.
     * The returned Grid2D will have the primary data in that area.) If the
     * primary grid is an ISC grid, then the data is not blended and the local
     * site id is only included if there is data in the ISC grid for the local
     * site.
     *
     * @param gid
     *            The grid id
     * @param exactMatch
     *            If exact match is requested
     * @return The composite grid
     */
    public Pair<Grid2DBit, IGridData> getCompositeGrid(GridID gid, boolean exactMatch);

}
