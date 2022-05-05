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
package com.raytheon.uf.viz.xy.crosssection.rsc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.Envelope2D;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.style.contour.ContourPreferences;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor;
import com.raytheon.viz.core.contours.ContourSupport;
import com.raytheon.viz.core.contours.ContourSupport.ContourGroup;
import org.locationtech.jts.geom.Envelope;

/**
 * Resource for displaying cross sections as contours
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Dec 04, 2007           njensen     Initial creation
 * Feb 17, 2009           njensen     Refactored to new rsc architecture
 * Feb 09, 2011  8244     bkowal      Enabled the magnification capability.
 * Feb 17, 2014  2661     bsteffen    Use only u,v for vectors.
 * Nov 10, 2015  4689     kbisanz     Ensure corners are within grid when
 *                                    zooming.
 * Feb 28, 2018  7231     njensen     Use source as creating entity to get style rule
 * Apr 12, 2018  7264     njensen     Use OutlineCapability/LineStyle
 * 
 * 
 * </pre>
 * 
 * @author njensen
 */
public class CrossSectionContourResource extends AbstractCrossSectionResource {

    private ContourPreferences contourPrefs;

    private Map<DataTime, ContourGroup[]> contours = new HashMap<>();

    private static final double ZOOM_REACTION_FACTOR = .45;

    private static final int NUMBER_CONTOURING_LEVELS = 5;

    private static final double[] ZOOM_THRESHOLDS = new double[NUMBER_CONTOURING_LEVELS];

    private IFont crossSectionFont = null;

    static {
        for (int i = 0; i < NUMBER_CONTOURING_LEVELS; i++) {
            ZOOM_THRESHOLDS[i] = Math.pow(ZOOM_REACTION_FACTOR, i);
        }
    }

    public CrossSectionContourResource(CrossSectionResourceData data,
            LoadProperties props, AbstractCrossSectionAdapter<?> adapter) {
        super(data, props, adapter);
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setLevel(null);
        List<String> paramList = new ArrayList<>();
        paramList.add(resourceData.getParameter());
        match.setParameterName(paramList);
        List<String> creatingEntities = new ArrayList<>();
        creatingEntities.add(resourceData.getSource());
        match.setCreatingEntityNames(creatingEntities);
        StyleRule sr = null;
        try {
            sr = StyleManager.getInstance()
                    .getStyleRule(StyleManager.StyleType.CONTOUR, match);
        } catch (StyleException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error getting contour style rule", e);
        }
        if (sr != null) {
            prefs = contourPrefs = (ContourPreferences) sr.getPreferences();
        }
        getCapability(DisplayTypeCapability.class)
                .setAlternativeDisplayTypes(Arrays.asList(DisplayType.IMAGE));
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        for (ContourGroup[] groups : contours.values()) {
            for (ContourGroup group : groups) {
                if (group != null) {
                    group.posValueShape.dispose();
                    group.negValueShape.dispose();
                }
            }
        }
        contours.clear();

        if (this.crossSectionFont != null) {
            this.crossSectionFont.dispose();
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        super.paintInternal(target, paintProps);
        DataTime currentTime = paintProps.getDataTime();
        if (sliceMap.get(currentTime) == null) {
            return;
        }
        LineStyle posLineStyle = null;
        LineStyle negLineStyle = null;
        OutlineCapability lineCap = getCapability(OutlineCapability.class);
        if (lineCap.getLineStyle() == LineStyle.DEFAULT) {
            posLineStyle = LineStyle.SOLID;
            negLineStyle = LineStyle.DASHED_LARGE;
        } else {
            posLineStyle = lineCap.getLineStyle();
            negLineStyle = lineCap.getLineStyle();
        }
        double density = getCapability(DensityCapability.class).getDensity();
        if (density > 4) {
            density = 4;
        }
        ContourGroup[] cgs = contours.get(currentTime);
        if (cgs == null) {
            cgs = new ContourGroup[NUMBER_CONTOURING_LEVELS];
            contours.put(currentTime, cgs);
        }

        int level = 0;
        double zoom = paintProps.getZoomLevel();
        for (level = NUMBER_CONTOURING_LEVELS - 1; level > 0; level--) {
            if (zoom < ZOOM_THRESHOLDS[level]) {
                break;
            }
        }

        IExtent viewExtent = paintProps.getView().getExtent();

        IExtent extent = descriptor.getGraph(this).getExtent();

        Envelope viewedEnv = ((PixelExtent) viewExtent.intersection(extent))
                .getEnvelope();

        if (cgs[level] == null || cgs[level].lastDensity != density
                || !cgs[level].lastUsedPixelExtent.getEnvelope()
                        .contains(viewedEnv)) {

            if (cgs[level] != null) {
                // Dispose old wireframe shapes
                if (cgs[level].posValueShape != null) {
                    cgs[level].posValueShape.dispose();
                }

                if (cgs[level].negValueShape != null) {
                    cgs[level].negValueShape.dispose();
                }
            }
            List<float[]> dataList = sliceMap.get(currentTime);
            GeneralGridGeometry geometry = this.geometry;
            try {
                // Prepare math transforms
                MathTransform grid2crs = geometry
                        .getGridToCRS(PixelInCell.CELL_CORNER);
                MathTransform crs2grid = grid2crs.inverse();

                // Get two opposite corners
                DirectPosition2D minCorner = new DirectPosition2D(
                        viewedEnv.getMinX(), viewedEnv.getMinY());
                DirectPosition2D maxCorner = new DirectPosition2D(
                        viewedEnv.getMaxX(), viewedEnv.getMaxY());

                // Transform the corners to grid space.
                crs2grid.transform(minCorner, minCorner);
                crs2grid.transform(maxCorner, maxCorner);

                double width = maxCorner.x - minCorner.x;
                double height = maxCorner.y - minCorner.y;

                // This does several things at once.
                // 1. Expand the grid area by 25% to avoid constant recontouring
                // 2. Round everything to ints
                minCorner.x = (int) (minCorner.x - width / 4);
                minCorner.y = (int) (minCorner.y - height / 4) + 1;
                maxCorner.x = (int) (maxCorner.x + width / 4) + 1;
                maxCorner.y = (int) (maxCorner.y + height / 4);

                // Ensure corners are within the bounds of the grid.
                constrainPoint(minCorner);
                constrainPoint(maxCorner);

                // Copy the data to a smaller array for the subgrid area.
                List<float[]> newDataList = new ArrayList<>();
                for (float[] data : dataList) {
                    float[] newData = new float[(int) Math
                            .abs((minCorner.y - maxCorner.y)
                                    * (maxCorner.x - minCorner.x))];
                    int newIndex = 0;
                    for (int j = (int) maxCorner.y; j < minCorner.y; j++) {
                        for (int i = (int) minCorner.x; i < maxCorner.x; i++) {
                            newData[newIndex] = data[j * GRID_SIZE + i];
                            newIndex++;
                        }
                    }
                    newDataList.add(newData);
                }
                GridEnvelope2D gridEnv = new GridEnvelope2D(0, 0,
                        (int) (maxCorner.x - minCorner.x),
                        (int) (minCorner.y - maxCorner.y));

                // Shift to cell corner.
                // minCorner.x -= 0.5;
                // maxCorner.x -= 0.5;
                // minCorner.y -= 0.5;
                // maxCorner.y -= 0.5;

                // Transform back to pixel space
                grid2crs.transform(minCorner, minCorner);
                grid2crs.transform(maxCorner, maxCorner);
                Envelope2D env = new Envelope2D(minCorner, maxCorner);

                // make a new geometry and extent for the subgrid.
                geometry = new GeneralGridGeometry(gridEnv, env);
                dataList = newDataList;
                extent = new PixelExtent(minCorner.x, maxCorner.x, minCorner.y,
                        maxCorner.y);
            } catch (TransformException e) {
                statusHandler.handle(Priority.ERROR,
                        "Error occured subgridding data, full grid will be contoured, this may be slow.",
                        e);
            }
            // worry about the viewed pane? Reduce data etc?
            if (getCapability(DisplayTypeCapability.class)
                    .getDisplayType() == DisplayType.STREAMLINE) {
                dataList = Arrays.asList(dataList.get(0), dataList.get(1));
                cgs[level] = ContourSupport.createContours(dataList, level,
                        extent, density, geometry, target, contourPrefs);
            } else {
                cgs[level] = ContourSupport.createContours(dataList.get(0),
                        level, extent, density, geometry, target, contourPrefs);
            }
            cgs[level].lastUsedPixelExtent = (PixelExtent) extent;
        }

        // Determine the magnification for the contour text
        Double magnification = getCapability(MagnificationCapability.class)
                .getMagnification();
        if (this.crossSectionFont == null) {
            this.crossSectionFont = target.getDefaultFont()
                    .deriveWithSize(target.getDefaultFont().getFontSize());
        }

        target.clearClippingPlane();
        this.crossSectionFont.setMagnification(magnification.floatValue());
        cgs[level].drawContours(target,
                getCapability(ColorableCapability.class).getColor(),
                lineCap.getOutlineWidth(), posLineStyle, negLineStyle,
                this.crossSectionFont, null);
    }

    /**
     * Ensure point is between 0 and grid size, updating values if necessary.
     * 
     * @param point
     *            Point to check and update.
     */
    private void constrainPoint(DirectPosition2D point) {
        // Ensure values are at least 0. Zooming near the edge of the
        // screen may cause values less than 0.
        point.x = Math.max(point.x, 0.0);
        point.y = Math.max(point.y, 0.0);

        // Ensure values are at most GRID_SIZE. Zooming near the edge
        // of the screen may cause values greater than GRID_SIZE.
        point.x = Math.min(point.x, GRID_SIZE);
        point.y = Math.min(point.y, GRID_SIZE);
    }

    @Override
    public void setDescriptor(CrossSectionDescriptor descriptor) {
        super.setDescriptor(descriptor);
        for (ContourGroup[] groups : contours.values()) {
            for (ContourGroup group : groups) {
                if (group != null) {
                    group.posValueShape.dispose();
                    group.negValueShape.dispose();
                }
            }
        }
        contours.clear();
    }

    @Override
    public void disposeTimeData(DataTime dataTime) {
        super.disposeTimeData(dataTime);
        ContourGroup[] contours = this.contours.remove(dataTime);
        if (contours != null) {
            for (ContourGroup group : contours) {
                if (group != null) {
                    group.posValueShape.dispose();
                    group.negValueShape.dispose();
                }
            }
        }
    }

}
