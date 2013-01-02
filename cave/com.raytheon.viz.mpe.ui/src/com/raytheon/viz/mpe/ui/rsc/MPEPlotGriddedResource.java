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
package com.raytheon.viz.mpe.ui.rsc;

import java.awt.Rectangle;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.core.contours.rsc.displays.GriddedContourDisplay;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay2;
import com.raytheon.viz.mpe.ui.rsc.AbstractMPEGriddedResourceData.Frame;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pcp;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 6, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MPEPlotGriddedResource extends
        AbstractGriddedMPEResource<AbstractMPEGriddedResourceData, Frame> {

    private Pcp pcp = DailyQcUtils.pcp;

    private Pcp spf = DailyQcUtils.spf;

    /**
     * @param resourceData
     * @param loadProperties
     */
    public MPEPlotGriddedResource(AbstractMPEGriddedResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        DataTime currTime = paintProps.getDataTime();
        if (currTime == null) {
            return;
        }
        boolean image = true;
        boolean contour = false;

        Frame frame = getFrame(currTime);
        if (image) {
            if (frame.imageDisplay == null) {
                frame.imageDisplay = createFrameImage(frame);
            }
            frame.imageDisplay.paint(target, paintProps);
        }
        if (contour) {
            if (frame.contourDisplay == null) {
                frame.contourDisplay = createFrameContour(frame);
            }
            frame.contourDisplay.setColor(getCapability(
                    ColorableCapability.class).getColor());
            frame.contourDisplay.setLineStyle(getCapability(
                    OutlineCapability.class).getLineStyle());
            frame.contourDisplay.paint(target, paintProps);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractGriddedMPEResource#getHrapSubGridExtent
     * ()
     */
    @Override
    protected Rectangle getHrapSubGridExtent() {
        Hrap_Grid hrap_grid = DailyQcUtils.getHrap_grid();
        return new Rectangle(hrap_grid.hrap_minx, hrap_grid.hrap_miny,
                hrap_grid.maxi, hrap_grid.maxj);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IResourceDataChanged#resourceChanged(com
     * .raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractGriddedMPEResource#createFrame(com
     * .raytheon.uf.common.time.DataTime)
     */
    @Override
    protected Frame createFrame(DataTime currTime) throws VizException {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractGriddedMPEResource#createFrameContour
     * (com.raytheon.viz.mpe.ui.rsc.AbstractMPEGriddedResourceData.Frame)
     */
    @Override
    protected GriddedContourDisplay createFrameContour(Frame frame)
            throws VizException {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractGriddedMPEResource#createFrameImage
     * (com.raytheon.viz.mpe.ui.rsc.AbstractMPEGriddedResourceData.Frame)
     */
    @Override
    protected GriddedImageDisplay2 createFrameImage(Frame frame)
            throws VizException {
        // TODO Auto-generated method stub
        return null;
    }

}
