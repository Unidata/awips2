/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.xy.crosssection;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ICapabilityProvider;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.core.contours.ContourGroup;
import com.raytheon.viz.core.contours.ContourSupport;

/**
 * Renderable contour data for a single cross section frame.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 14, 2024 2037631    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public class CrossSectionContour extends CrossSectionFrameRenderable {

    private static final double ZOOM_REACTION_FACTOR = .45;

    private static final int NUMBER_CONTOURING_LEVELS = 5;

    private static final double[] ZOOM_THRESHOLDS = new double[NUMBER_CONTOURING_LEVELS];
    static {
        for (int i = 0; i < NUMBER_CONTOURING_LEVELS; i++) {
            ZOOM_THRESHOLDS[i] = Math.pow(ZOOM_REACTION_FACTOR, i);
        }
    }

    /**
     * Array of contour groups to display for this frame. Which group is painted
     * at any given time depends on the current zoom level.
     */
    private final ContourGroup[] contourGroups = new ContourGroup[NUMBER_CONTOURING_LEVELS];

    private final IFont font;

    public CrossSectionContour(ICapabilityProvider capProvider, IFont font,
            CrossSectionFrameExtraRenderable extraRenderable) {
        super(capProvider, extraRenderable);
        this.font = font;
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        OutlineCapability lineCap = capProvider
                .getCapability(OutlineCapability.class);
        LineStyle posLineStyle, negLineStyle;
        if (lineCap.getLineStyle() == LineStyle.DEFAULT) {
            posLineStyle = LineStyle.SOLID;
            negLineStyle = LineStyle.DASHED_LARGE;
        } else {
            posLineStyle = lineCap.getLineStyle();
            negLineStyle = lineCap.getLineStyle();
        }

        font.setMagnification(
                capProvider.getCapability(MagnificationCapability.class)
                        .getMagnification().floatValue());

        ColorableCapability colorCap = capProvider
                .getCapability(ColorableCapability.class);
        int level = getLevel(paintProps);
        contourGroups[level].drawContours(target, colorCap.getColor(),
                lineCap.getOutlineWidth(), posLineStyle, negLineStyle, font,
                null);
    }

    @Override
    public void disposeInternal() {
        ContourSupport.disposeContourGroups(contourGroups);
    }

    /**
     * @param level
     *            contour level to get contours for
     * @return contour group for the given level
     */
    public ContourGroup getContourGroup(int level) {
        return contourGroups[level];
    }

    /**
     * Set contour group for given level. If a contour group already exists for
     * that level, it will be disposed.
     *
     * @param level
     *            contour level to set contours for
     * @param contourGroup
     *            contour group to set
     */
    public void setContourGroup(int level, ContourGroup contourGroup) {
        if (contourGroups[level] != contourGroup) {
            ContourSupport.disposeContourGroups(contourGroups[level]);
        }
        this.contourGroups[level] = contourGroup;
    }

    /**
     * Get the contour level to use, based on the current zoom level.
     *
     * @param paintProps
     *            paint properties
     * @return contour level
     */
    public int getLevel(PaintProperties paintProps) {
        int level = 0;
        double zoom = paintProps.getZoomLevel();
        for (level = NUMBER_CONTOURING_LEVELS - 1; level > 0; level--) {
            if (zoom < ZOOM_THRESHOLDS[level]) {
                break;
            }
        }
        return level;
    }
}
