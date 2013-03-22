/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package com.raytheon.viz.awipstools.ui.layer;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.IFrameChangedListener;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.awipstools.common.StormTrackData;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Port of feature following zoom tool
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2010 #5952      bkowal      This tool will no longer update the extents by itself,
 *                                     instead it will let the target know about the updated
 *                                     extents.
 * 15Mar2013	15693	mgamazaychikov Added magnification capability.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class FeatureFollowingZoomLayer extends
        AbstractVizResource<AbstractResourceData, MapDescriptor> implements
        IFrameChangedListener {

    private int lastFrame = -1;

    public FeatureFollowingZoomLayer(
            GenericToolsResourceData<DistanceSpeedLayer> resourceData,
            LoadProperties loadProperties, MapDescriptor descriptor) {
        super(resourceData, loadProperties);
        // add magnification capability
		getCapabilities().addCapability(new MagnificationCapability());
    }

    @Override
    protected void disposeInternal() {
        descriptor.removeFrameChangedListener(this);

    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        descriptor.addFrameChangedListener(this);
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        // Nothing to paint
    }

    private boolean hasVisibleResource() {
        boolean rval = false;
        for (ResourcePair rp : descriptor.getResourceList()) {
            ResourceProperties props = rp.getProperties();
            if (props.isMapLayer() == false
                    && props.isSystemResource() == false && props.isVisible()) {
                rval = true;
            }
        }
        return rval;
    }

    @Override
    public void frameChanged(IDescriptor descriptor, DataTime oldTime,
            DataTime newTime) {
        StormTrackData data = ToolsDataManager.getInstance()
                .getStormTrackData();
        // Depending on frame, set center of screen to point
        Coordinate[] trackPoints = data.getCoordinates();
        int index = descriptor.getFramesInfo().getFrameIndex();
        if (lastFrame != index && index >= 0 && index < trackPoints.length
                && trackPoints.length > 0 && hasVisibleResource()) {
            Coordinate coord = trackPoints[index];
            double[] end = descriptor.worldToPixel(new double[] { coord.x,
                    coord.y });
            double[] start = descriptor.getRenderableDisplay().getExtent()
                    .getCenter();
            IExtent updatedExtent = descriptor.getRenderableDisplay()
                    .getExtent().clone();
            updatedExtent.shift(end[0] - start[0], end[1] - start[1]);

            descriptor.getRenderableDisplay().setExtent(updatedExtent);
            issueRefresh();
        }
        lastFrame = index;

    }
}
