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
package com.raytheon.viz.volumebrowser.loader;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.ui.IEditorPart;
import org.geotools.referencing.CRS;
import org.geotools.referencing.operation.projection.MapProjection;
import org.geotools.referencing.operation.projection.MapProjection.AbstractProvider;
import org.opengis.geometry.Envelope;
import org.opengis.parameter.ParameterNotFoundException;
import org.opengis.parameter.ParameterValueGroup;

import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.grid.rsc.GridLoadProperties;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.display.VizMapEditor;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionRenderableDisplay;
import com.raytheon.uf.viz.xy.crosssection.rsc.CrossSectionResourceData;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.util.CrossSectionUtil;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.SpaceTimeMenu;
import com.raytheon.viz.volumebrowser.vbui.VolumeBrowserDialogSettings;
import org.locationtech.jts.densify.Densifier;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LineString;

/**
 * 
 * Creates a {@link CrossSectionRenderableDisplay} containing a
 * {@link CrossSectionResourceData}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 03, 2015  3861     bsteffen  Initial Creation
 * Apr 18, 2018  6719     njensen   Create a curved line if the user selected
 *                                  a single Lat or Lon and try to center and
 *                                  zoom on it nicely
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class CrossSectionProductCreator extends AbstractProductCreator {

    @Override
    protected AbstractRenderableDisplay createNewRenderableDisplay(
            VolumeBrowserDialogSettings dialogSettings,
            SelectedData selectedData) {
        CrossSectionRenderableDisplay display = new CrossSectionRenderableDisplay();
        CrossSectionDescriptor descriptor = display.getDescriptor();
        descriptor.setRenderableDisplay(display);

        SpaceTimeMenu currentSpaceTime = dialogSettings.getSpaceTimeSelection();
        String planesKey = selectedData.getPlanesKey();
        if (currentSpaceTime == SpaceTimeMenu.TIME) {
            /*
             * The Selected Plane in Time setting will be either a Baseline or a
             * Lat/Lon Line
             */
            GeometryFactory factory = new GeometryFactory();
            LineString line = null;

            if (planesKey.startsWith("Lon")) {
                double x = Double.parseDouble(planesKey.substring(3));
                Coordinate start = new Coordinate(x, -89.999);
                Coordinate end = new Coordinate(x, 89.999);
                line = factory.createLineString(
                        makeProjectedLine(start, end, 1.0, false));
            } else if (planesKey.startsWith("Lat")) {
                double y = Double.parseDouble(planesKey.substring(3));
                Coordinate start = new Coordinate(-179.999, y);
                Coordinate end = new Coordinate(179.999, y);
                line = factory.createLineString(
                        makeProjectedLine(start, end, 1.0, true));
            } else if (planesKey.startsWith("Line")) {
                ToolsDataManager dataManager = ToolsDataManager.getInstance();
                line = dataManager.getBaseline(planesKey.substring(4));
                /* default to Baseline A if all else fails */
                if (line == null) {
                    line = dataManager.getBaseline("A");
                }
            } else {
                throw new RuntimeException(
                        "Invalid line has been selected: " + planesKey);
            }

            /* set the line at which the results are displayed */
            descriptor.setLines(Arrays.asList(line));
            descriptor.setLineID(selectedData.getPlanesText());
        } else if (currentSpaceTime == SpaceTimeMenu.SPACE) {
            if ("LATS".equals(planesKey)) {
                descriptor.setLines(CrossSectionUtil.getAllLats());
                descriptor.setLineID("AllLAT");
            } else if ("LONS".equals(planesKey)) {
                descriptor.setLines(CrossSectionUtil.getAllLons());
                descriptor.setLineID("AllLON");
            }
        }
        descriptor.setHeightScale(dialogSettings.getHeightScaleSelection());
        return display;
    }

    @Override
    protected CrossSectionResourceData createNewResourceData(
            IDataCatalog dataCatalog, IDataCatalogEntry catalogEntry,
            DisplayType displayType) {
        SelectedData selectedData = catalogEntry.getSelectedData();
        CrossSectionResourceData resourceData = new CrossSectionResourceData();
        resourceData.setParameter(selectedData.getFieldsKey());
        resourceData.setParameterName(selectedData.getFieldsText());
        resourceData.setSource(selectedData.getSourcesText());
        return resourceData;
    }

    @Override
    protected LoadProperties createNewLoadProperties(DisplayType displayType) {
        return new GridLoadProperties(displayType);
    }

    /**
     * Creates a projected line (that appears curved on projections other than
     * basic lat/lon) by splitting the original line into multiple coordinates,
     * centering over the central meridian if it's a latitude line, and then
     * filtering the coordinates that fit within the map's display extent.
     * 
     * Turns the line into multiple smaller segments using {@link Densifier}.
     * For example, the equator with only two coordinates could have a start
     * point of (-180, 0) and end point of (180, 0). Using a distanceTolerance
     * of 1.0, the line would be turned into 360 coordinates, each 1.0 degrees
     * (arbitrary units) apart. So the same line would now be (-180, 0), (-179,
     * 0), (-178, 0)... (178, 0), (179, 0), (180, 0).
     * 
     * After the line has been been turned into more coordinates, the
     * coordinates are checked against the display's extent to see if the
     * coordinate fits on the extent. If not, the coordinate is discarded. For
     * example, if your extent only covered the Americas, then the equator line
     * would now be cut off on both ends to fit inside the extent.
     * 
     * @param start
     *            the start of the line
     * @param end
     *            the end of the line
     * @param distanceTolerance
     *            how much distance to place between each line segment
     * @param latitude
     *            if this is a latitude line
     * @return a line that is projected against the map descriptor's projection
     */
    private static Coordinate[] makeProjectedLine(Coordinate start,
            Coordinate end, double distanceTolerance, boolean latitude) {
        try {
            IEditorPart editor = EditorUtil.findEditor(VizMapEditor.EDITOR_ID);
            MapDescriptor mapDesc = null;
            if (editor != null) {
                mapDesc = (MapDescriptor) ((AbstractEditor) editor)
                        .getActiveDisplayPane().getDescriptor();
            } else {
                mapDesc = new MapDescriptor();
            }

            if (latitude) {
                // recenter over the descriptor's central meridian
                double centralMeridian = getMeridian(
                        mapDesc.getGridGeometry().getEnvelope());
                start = new Coordinate(centralMeridian + start.x, start.y);
                end = new Coordinate(centralMeridian + end.x, end.y);
            }

            // increase number of coordinates along line
            GeometryFactory factory = new GeometryFactory();
            LineString line = factory
                    .createLineString(new Coordinate[] { start, end });
            line = (LineString) Densifier.densify(line, distanceTolerance);

            // keep the coordinates that fit in the extent
            IRenderableDisplay display = mapDesc.getRenderableDisplay();
            IExtent extent = display.getExtent();
            List<Coordinate> withinBounds = new ArrayList<>();
            for (Coordinate coord : line.getCoordinates()) {
                double[] c = new double[] { coord.x, coord.y };
                double[] pixel = mapDesc.worldToPixel(c);
                if (pixel != null && extent.contains(pixel)) {
                    withinBounds.add(coord);
                }
            }

            if (!withinBounds.isEmpty()) {
                return withinBounds.toArray(new Coordinate[0]);
            } else {
                /*
                 * no points on the line crossed the map, ideally user shouldn't
                 * have selected this, the desired behavior is unknown, give
                 * them the original line
                 */
                return line.getCoordinates();
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Gets the central meridian out of an envelope
     * 
     * @param worldEnvelope
     * @return
     */
    private static double getMeridian(Envelope worldEnvelope) {
        MapProjection worldProjection = CRS
                .getMapProjection(worldEnvelope.getCoordinateReferenceSystem());
        double centralMeridian = 0.0;
        if (worldProjection != null) {
            ParameterValueGroup group = worldProjection.getParameterValues();
            try {
                centralMeridian = group.parameter(
                        AbstractProvider.CENTRAL_MERIDIAN.getName().getCode())
                        .doubleValue();
            } catch (ParameterNotFoundException e) {
                UFStatus.getHandler().handle(Priority.WARN,
                        "Error determining central meridian", e);
                centralMeridian = Double.NaN;
            }
        }
        return centralMeridian;
    }

}
