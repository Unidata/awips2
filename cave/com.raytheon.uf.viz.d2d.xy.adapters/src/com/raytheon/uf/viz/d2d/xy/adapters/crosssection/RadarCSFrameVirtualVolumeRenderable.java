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
package com.raytheon.uf.viz.d2d.xy.adapters.crosssection;

import java.util.List;

import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.grid.radar.util.RadarVirtualVolumeStatus;
import com.raytheon.uf.viz.grid.radar.util.RadarVirtualVolumeUtil;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionFrameExtraRenderable;

/**
 * Extra renderables to display for radar cross section frames that are
 * currently displaying a virtual volume. The extra renderables indicate the
 * virtual volume status, by adding text to the product legend and drawing a
 * line on the display that corresponds to the current scan's latest elevation
 * angle.
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
class RadarCSFrameVirtualVolumeRenderable
        extends CrossSectionFrameExtraRenderable {

    protected final RadarVirtualVolumeStatus virtualVolumeStatus;

    protected final DrawableLine line;

    /**
     * Constructor.
     *
     * @param status
     *            current virtual volume status
     * @param currentTiltLine
     *            coordinates indicating current tilt's height along the
     *            baseline, in pixel coordinates relative to the graph
     */
    public RadarCSFrameVirtualVolumeRenderable(RadarVirtualVolumeStatus status,
            List<double[]> currentTiltLine) {
        this.virtualVolumeStatus = status;

        if (currentTiltLine != null) {
            line = new DrawableLine();
            /*
             * Call addPoint rather than directly setting line.points, so
             * DrawableLine can set z coord to what it wants
             */
            for (double[] point : currentTiltLine) {
                line.addPoint(point[0], point[1]);
            }
        } else {
            line = null;
        }
    }

    @Override
    public String getExtraLegendText() {
        /*
         * Build text indicating the virtual volume status for the given frame
         * time, such as "(12:06Z above 1.5)"
         */
        return RadarVirtualVolumeUtil.buildLegendText(virtualVolumeStatus);
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        if (line == null) {
            return;
        }

        line.basics.color = capProvider.getCapability(ColorableCapability.class)
                .getColor();
        target.drawLine(line);
    }

    @Override
    public void dispose() {
        // DrawableLines don't need disposing, so nothing to do
    }
}