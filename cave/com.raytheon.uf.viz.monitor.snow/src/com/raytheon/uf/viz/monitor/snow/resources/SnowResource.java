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
 */
package com.raytheon.uf.viz.monitor.snow.resources;

import java.util.Date;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.monitor.snow.listeners.ISnowResourceListener;

/**
 * SnowResource
 * 
 * Implements empty display for SNOW
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date             Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    July 20, 2010    4891        skorolev    Initial Creation.
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1
 */
public class SnowResource extends
        AbstractVizResource<SnowResourceData, MapDescriptor> implements
        IResourceDataChanged, ISnowResourceListener {

    private DataTime displayedDataTime;

    private DataTime previousDataTime;

    /**
     * Create an empty surface plot resource.
     * 
     * @param aTarget
     *            The graphic target to draw to
     * 
     * @throws VizException
     */
    public SnowResource(SnowResourceData data, LoadProperties props) {
        super(data, props);
        data.addChangeListener(this);
    }

    @Override
    public String getName() {
        if (this.displayedDataTime != null) {
            return "SNOW Table Display "
                    + this.displayedDataTime.getLegendString();
        }
        return "";
    }

    @Override
    protected synchronized void paintInternal(IGraphicsTarget aTarget,
            PaintProperties paintProps) throws VizException {
        this.displayedDataTime = paintProps.getDataTime();
        FramesInfo info = paintProps.getFramesInfo();
        if (info.getFrameCount() != 0) {
            int currentFrame = info.getFrameIndex();
            this.displayedDataTime = info.getFrameTimes()[currentFrame];
            if (!this.displayedDataTime.equals(previousDataTime)) {
                updateDialogTime(displayedDataTime.getRefTime());
            }
        }
        this.previousDataTime = displayedDataTime;
    }

    @Override
    protected void initInternal(IGraphicsTarget aTarget) throws VizException {

    }

    @Override
    protected void disposeInternal() {
        resourceData.getSnowMonitor().removeSnowResourceListener(this);
        resourceData.getSnowMonitor().closeDialog();
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type.equals(ChangeType.DATA_UPDATE)) {

        } else if (type.equals(ChangeType.CAPABILITY)) {
            if (object instanceof ColorableCapability) {

            } else if (object instanceof DensityCapability
                    || object instanceof MagnificationCapability) {

            }
        }
        issueRefresh();
    }

    @Override
    public void updateDialogTime(Date time) {
        resourceData.getSnowMonitor().updateDialogTime(time);
    }
}
