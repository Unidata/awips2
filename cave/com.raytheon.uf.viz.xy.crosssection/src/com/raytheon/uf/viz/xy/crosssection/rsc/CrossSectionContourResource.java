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
import org.opengis.referencing.operation.NoninvertibleTransformException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.uf.viz.core.style.VizStyleException;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor;
import com.raytheon.viz.core.contours.ContourSupport;
import com.raytheon.viz.core.contours.ContourSupport.ContourGroup;
import com.raytheon.viz.core.style.contour.ContourPreferences;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Resource for displaying cross sections as contours
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 4, 2007             njensen     Initial creation
 * 02/17/09                njensen     Refactored to new rsc architecture
 * Feb 9, 2011  8244       bkowal      Enabled the magnification capability.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CrossSectionContourResource extends AbstractCrossSectionResource
        implements IResourceDataChanged {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CrossSectionContourResource.class);

    private ContourPreferences contourPrefs;

    private Map<DataTime, ContourGroup[]> contours = new HashMap<DataTime, ContourGroup[]>();

    private boolean useDefaultLines = true;

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
        ArrayList<String> paramList = new ArrayList<String>();
        paramList.add(resourceData.getParameter());
        match.setParameterName(paramList);
        StyleRule sr = null;
        try {
            sr = StyleManager.getInstance().getStyleRule(
                    StyleManager.StyleType.CONTOUR, match);
        } catch (VizStyleException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error getting contour style rule", e);
        }
        if (sr != null) {
            prefs = contourPrefs = (ContourPreferences) sr.getPreferences();
        }
        getCapability(DisplayTypeCapability.class).setAlternativeDisplayTypes(
                Arrays.asList(DisplayType.IMAGE));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.crosssection.rsc.AbstractCrossSectionResource#
     * disposeInternal()
     */
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
        if (useDefaultLines) {
            posLineStyle = LineStyle.SOLID;
            negLineStyle = LineStyle.DASHED_LARGE;
        } else {
            posLineStyle = getCapability(OutlineCapability.class)
                    .getLineStyle();
            negLineStyle = getCapability(OutlineCapability.class)
                    .getLineStyle();
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

        if (cgs[level] == null
                || cgs[level].lastDensity != density
                || !cgs[level].lastUsedPixelExtent.getEnvelope().contains(
                        viewedEnv)) {

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
            GeneralGridGeometry geometry = (GeneralGridGeometry) this.geometry;
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
                // 2. Make sure the new area is not outside the data area
                // 3. Round everything to ints
                minCorner.x = Math.max((int) (minCorner.x - width / 4), 0);
                minCorner.y = Math.min((int) (minCorner.y - height / 4) + 1,
                        GRID_SIZE);
                maxCorner.x = Math.min((int) (maxCorner.x + width / 4) + 1,
                        GRID_SIZE);
                maxCorner.y = Math.max((int) (maxCorner.y + height / 4), 0);

                // Copy the data to a smaller array for the subgrid area.
                List<float[]> newDataList = new ArrayList<float[]>();
                for (float[] data : dataList) {
                    float[] newData = new float[(int) Math
                            .abs((minCorner.y - maxCorner.y)
                                    * (maxCorner.x - minCorner.x))];
                    int newIndex = 0;
                    for (int j = (int) maxCorner.y; j < minCorner.y; j++) {
                        for (int i = (int) minCorner.x; i < maxCorner.x; i++) {
                            newData[newIndex++] = data[j * GRID_SIZE + i];
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
            } catch (NoninvertibleTransformException e) {
                statusHandler
                        .handle(Priority.ERROR,
                                "Error occured subgridding data, full grid will be contoured, this may be slow.",
                                e);
            } catch (TransformException e) {
                statusHandler
                        .handle(Priority.ERROR,
                                "Error occured subgridding data, full grid will be contoured, this may be slow.",
                                e);
            }
            // worry about the viewed pane? Reduce data etc?
            if (getCapability(DisplayTypeCapability.class).getDisplayType() == DisplayType.STREAMLINE) {
                dataList = Arrays.asList(dataList.get(2), dataList.get(3));
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
            this.crossSectionFont = target.getDefaultFont().deriveWithSize(
                    target.getDefaultFont().getFontSize());
        }

        target.clearClippingPlane();
        this.crossSectionFont.setMagnification(magnification.floatValue());
        cgs[level].drawContours(target,
                getCapability(ColorableCapability.class).getColor(),
                getCapability(OutlineCapability.class).getOutlineWidth(),
                posLineStyle, negLineStyle, this.crossSectionFont, null);
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
    public void resourceChanged(ChangeType type, Object object) {
        if (type.equals(ChangeType.CAPABILITY)) {
            if (object instanceof OutlineCapability) {
                useDefaultLines = false;
            }
        }
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
