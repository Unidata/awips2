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
package com.raytheon.viz.hydrocommon.actions;

import java.awt.Point;
import java.awt.Rectangle;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.hydro.spatial.HRAP;
import com.raytheon.uf.common.hydro.spatial.HRAPCoordinates;
import com.raytheon.uf.common.hydro.spatial.HRAPSubGrid;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.tools.AbstractTool;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 9, 2008            randerso     Initial creation
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SetProjection extends AbstractTool {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SetProjection.class);

    public static enum Projection {
        FLAT, POLAR, HRAP;

        public static Projection fromString(String projection) {
            for (Projection p : Projection.values()) {
                if (p.name().equalsIgnoreCase(projection)) {
                    return p;
                }
            }
            return null;
        }
    }

    private static final double NMI_PER_DEG = 60.0;

    private static final UnitConverter converter = NonSI.NAUTICAL_MILE
            .getConverterTo(SI.METER);

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        super.execute(arg0);

        String projection = arg0.getParameter("projection");
        String prefix = arg0.getParameter("prefix");

        setProjection(editor, Projection.fromString(projection), prefix);
        return null;
    }

    public static void setDefaultProjection(IDisplayPaneContainer editor,
            String prefix) {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        String projection = appsDefaults.getToken(prefix + "_map_projection",
                "FLAT");
        setProjection(editor, Projection.fromString(projection), prefix);
    }

    /**
     * @param editor
     *            editor on which to set the projection
     * @param projection
     *            should be one of "FLAT", "POLAR", or "HRAP"
     */
    public static void setProjection(IDisplayPaneContainer editor,
            Projection projection, String prefix) {
        if (projection == null) {
            projection = Projection.FLAT;
        }
        // get these from the token file
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        double centerLat = appsDefaults.getDouble(prefix + "_center_lat", 0.0);
        double centerLon = appsDefaults.getDouble(prefix + "_center_lon", 0.0);
        double widthInNmi = appsDefaults
                .getDouble(prefix + "_map_width", 320.0);

        // RWA: I have no idea what the value of the zoom out limit means.
        // double zoomLimit = appsDefaults.getDouble("hv_zoom_out_limit", 20.0);
        double zoomLimit = 4.0;

        try {
            CoordinateReferenceSystem crs;
            GridGeometry2D gridGeometry = null;
            switch (projection) {
            case FLAT:
                crs = MapUtil.LATLON_PROJECTION;

                double height_in_degrees = widthInNmi / NMI_PER_DEG * zoomLimit;

                gridGeometry = MapDescriptor.createGridGeometry(crs,
                        new Coordinate(centerLon, centerLat),
                        height_in_degrees, height_in_degrees);
                break;
            case POLAR:
                crs = MapUtil.constructNorthPolarStereo(
                        MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                        60, centerLon);

                double width_in_meters = converter.convert(widthInNmi)
                        * zoomLimit;

                gridGeometry = MapDescriptor.createGridGeometry(crs,
                        new Coordinate(centerLon, centerLat), width_in_meters,
                        width_in_meters);
                break;
            case HRAP:
                HRAP hrap = HRAP.getInstance();

                Point p = hrap.getGridPointUR();
                p.x++;
                p.y++;

                Rectangle extent = HRAPCoordinates.getHRAPCoordinates();
                HRAPSubGrid subGrid = new HRAPSubGrid(extent);
                int nx = (subGrid.getNx() + ((subGrid.getNx()) * 6));
                int ny = (subGrid.getNy() + ((subGrid.getNy()) * 6));
                int factorNx = Math.abs((subGrid.getNx() - nx) / 2);
                int factorNy = Math.abs((subGrid.getNy() - ny) / 2);
                int newX = (subGrid.getExtent().x - factorNx);
                int newY = (subGrid.getExtent().y - factorNy);
                Rectangle newExtent = new Rectangle(newX, newY, nx, ny);
                HRAPSubGrid newSubGrid = new HRAPSubGrid(newExtent);
                gridGeometry = MapUtil.getGridGeometry(newSubGrid);
                break;
            default:
                statusHandler.handle(Priority.PROBLEM, "\"" + projection
                        + "\" is not a recognized projection.");
                return;
            }

            if (editor == null) {
                editor = EditorUtil.getActiveVizContainer();
            }

            for (IDisplayPane pane : editor.getDisplayPanes()) {
                IMapDescriptor md = (IMapDescriptor) pane.getDescriptor();
                md.setGridGeometry(gridGeometry);
                pane.getRenderableDisplay().scaleToClientArea(pane.getBounds());
                pane.getRenderableDisplay().recenter(
                        new double[] { centerLon, centerLat });
                pane.getRenderableDisplay().zoom(1.0 / zoomLimit);
            }
            editor.refresh();
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unexpected error setting projection", e);
        }
    }
}
