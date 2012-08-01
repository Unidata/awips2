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
package com.raytheon.viz.gfe.edittool;

import java.awt.Point;
import java.awt.geom.Point2D;
import java.util.Collection;

import org.eclipse.jface.action.IMenuManager;
import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.IReferenceSetManager.RefSetMode;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.operation.polygonize.Polygonizer;
import com.vividsolutions.jts.operation.valid.IsValidOp;

/**
 * This tool allows the user to hand draw/modify an edit area
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 16, 2008		#1075	randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SelectPointsTool extends AbstractFreeformTool {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SelectPointsTool.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.edittool.AbstractFreeformTool#handleMouseUp(java
     * .awt.geom.Point2D, com.vividsolutions.jts.geom.Coordinate)
     */
    @Override
    @SuppressWarnings("unchecked")
    protected void handleEndDrag(int button, Point2D point2D,
            Coordinate coordinate) {
        // must have at least 3 points to define a polygon
        if (this.currentCoordinates.size() < 3) {
            return;
        }

        Coordinate[] coords = processPolygon();

        // create a line string
        GeometryFactory gf = new GeometryFactory();
        LineString ls = gf.createLineString(coords);

        // node the line string (insert vertices where lines cross)
        com.vividsolutions.jts.geom.Point pt = gf.createPoint(ls
                .getCoordinate());
        Geometry nodedLines = ls.union(pt);

        // create the polygon(s) from the noded line
        Polygonizer polygonizer = new Polygonizer();
        polygonizer.add(nodedLines);
        Collection<Polygon> polygons = polygonizer.getPolygons();

        // Collection<?> dangles = polygonizer.getDangles();
        // if (dangles != null && dangles.size() > 0) {
        // StringBuilder s = new StringBuilder(
        // "Edit area contains dangling lines.");
        // for (Object g : dangles) {
        // s.append("\n" + g);
        // }
        // UFStatus.handle(Priority.PROBLEM, Activator.PLUGIN_ID,
        // StatusConstants.CATEGORY_GFE, null, s.toString());
        // }
        //
        // Collection<?> cutEdges = polygonizer.getCutEdges();
        // if (cutEdges != null && cutEdges.size() > 0) {
        // StringBuilder s = new StringBuilder("Edit area contains cut edges.");
        // for (Object g : cutEdges) {
        // s.append("\n" + g);
        // }
        // UFStatus.handle(Priority.PROBLEM, Activator.PLUGIN_ID,
        // StatusConstants.CATEGORY_GFE, null, s.toString());
        // }

        // create a multipolygon from the collection of polygons
        Geometry g = gf.createMultiPolygon(polygons
                .toArray(new Polygon[polygons.size()]));

        // clean up self intersections/overlaps
        g = g.buffer(0.0);

        MultiPolygon mp = null;
        if (g instanceof MultiPolygon) {
            mp = (MultiPolygon) g;
        } else if (g instanceof Polygon) {
            mp = gf.createMultiPolygon(new Polygon[] { (Polygon) g });
        }

        if (mp != null) {
            if (mp.isValid()) {
                // create the reference data
                ReferenceData refData = new ReferenceData(this.dataManager
                        .getParmManager().compositeGridLocation(),
                        new ReferenceID("-Area-"), mp,
                        ReferenceData.CoordinateType.GRID);

                // add the new reference data to the reference manager
                if (button == 2) {
                    this.dataManager.getRefManager().incomingRefSet(refData,
                            RefSetMode.SUBTRACT);
                } else {
                    this.dataManager.getRefManager().incomingRefSet(refData,
                            RefSetMode.USE_CURRENT);
                }
            } else {
                StringBuilder s = new StringBuilder();
                s.append("Edit area contains invalid polygons.\n");
                IsValidOp validOp = new IsValidOp(mp);
                s.append(validOp.getValidationError());
                for (int i = 0; i < mp.getNumGeometries(); i++) {
                    Polygon p = (Polygon) mp.getGeometryN(i);
                    if (!p.isValid()) {
                        s.append("\n" + p);
                    }
                }
                statusHandler.handle(Priority.PROBLEM, s.toString());
            }
        }
    }

    /**
     * 
     */
    private Coordinate[] processPolygon() {
        Parm parm = null;
        IGridData gridData = this.getGrid();
        if (gridData != null) {
            parm = gridData.getParm();
        } else {
            Parm[] parms = this.dataManager.getParmManager()
                    .getDisplayedParms();
            if (parms.length == 0) {
                statusHandler.handle(Priority.VERBOSE,
                        "No displayed parms. Cannot select points.");
                return new Coordinate[0];
            } else {
                parm = parms[0];
            }
        }

        GridLocation gloc = parm.getGridInfo().getGridLoc();
        Point gridSize = gloc.gridSize();
        Envelope domain = new Envelope(0, gridSize.x - 1, 0, gridSize.y - 1);

        int last = this.currentCoordinates.size() - 1;
        Coordinate[] points = this.currentCoordinates
                .toArray(new Coordinate[this.currentCoordinates.size() + 3]);

        // close the polygon
        for (int i = last + 1; i < last + 4; i++) {
            points[i] = points[0];
        }
        MapUtil.latLonToGridCoordinate(points, PixelOrientation.CENTER, gloc);

        if (domain.contains(points[0]) || domain.contains(points[last])) {
            return points;
        }

        Coordinate upperLeft = new Coordinate(-0.5, -0.5);
        Coordinate upperRight = new Coordinate(gridSize.x - 0.5, -0.5);
        Coordinate lowerRight = new Coordinate(gridSize.x - 0.5,
                gridSize.y - 0.5);
        Coordinate lowerLeft = new Coordinate(-0.5, gridSize.y - 0.5);

        double top = 0;
        double bottom = gridSize.y - 1;
        double right = gridSize.x - 1;
        double left = 0;

        // First point at bottom edge
        if (points[0].y > bottom) {
            if (points[last].x < left) // left edge
            {
                points[last + 1] = lowerLeft;
            } else if (points[last].x > right) // right edge
            {
                points[last + 1] = lowerRight;
            } else if (points[last].y < top) {
                if (points[0].x + points[last].x < gridSize.x) // to top
                {
                    points[last + 1] = upperLeft; // along left edge
                    points[last + 2] = lowerLeft;
                } else {
                    points[last + 1] = upperRight; // along right edge
                    points[last + 2] = lowerRight;
                }
            }
        }
        // First point at left edge
        else if (points[0].x < left) {
            if (points[last].y > bottom) // to bottom
            {
                points[last + 1] = lowerLeft;
            } else if (points[last].y < top) // to top
            {
                points[last + 1] = upperLeft;
            } else if (points[last].x > right) {
                if (points[0].y + points[last].y > gridSize.y) {
                    points[last + 1] = lowerRight; // along bottom edge
                    points[last + 2] = lowerLeft;
                } else {
                    points[last + 1] = upperRight; // along top edge
                    points[last + 2] = upperLeft;
                }
            }
        }
        // First point at top edge
        else if (points[0].y < top) {
            if (points[last].x < left) // to left
            {
                points[last + 1] = upperLeft;
            } else if (points[last].x > right) // to right
            {
                points[last + 1] = upperRight;
            } else if (points[last].y > bottom) {
                if (points[0].x + points[last].x < gridSize.x) {
                    points[last + 1] = lowerLeft; // along left edge
                    points[last + 2] = upperLeft;
                } else {
                    points[last + 1] = lowerRight; // along right edge
                    points[last + 2] = upperRight;
                }
            }
        }
        // First point at right edge
        else if (points[0].x > right) {
            if (points[last].y < top) // to top
            {
                points[last + 1] = upperRight;
            } else if (points[last].y > bottom) // to bottom
            {
                points[last + 1] = lowerRight;
            } else if (points[last].x < left) {
                if (points[0].y + points[last].y > gridSize.y) {
                    points[last + 1] = lowerLeft; // along bottom edge
                    points[last + 2] = lowerRight;
                } else {
                    points[last + 1] = upperLeft; // along top edge
                    points[last + 2] = upperRight;
                }
            }
        }

        return points;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.edittool.AbstractGFEEditTool#getToolType()
     */
    @Override
    protected ToolType getToolType() {
        return ToolType.GENERAL;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.cmenu.IRightClickCapableResource#addContextMenuItems
     * (org.eclipse.jface.action.IMenuManager)
     */
    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        // no items to add
    }

}
