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
package com.raytheon.viz.awipstools.common.stormtrack;

import java.util.Date;

import org.eclipse.jface.action.IMenuManager;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.DisplayType;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.Mode;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.raytheon.viz.ui.input.EditableManager;

/**
 * Abstract class for drawing storm track
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Aug 16, 2010  2492     bkowal     Completed a TODO so that the available
 *                                   datatimes would be re-calculated when in
 *                                   time match mode and when the user increased
 *                                   the number of frames.
 * Oct 27, 2010  6964     bkowal     The OutlineCapability is now used to
 *                                   retrieve the requested line style so that
 *                                   it can be stored in the StormTrackState.
 * Feb 12, 2013  1600     jsanchez   Changed the visibility of the method
 *                                   adjustAngle
 * Mar 05, 2013  1600     jsanchez   Returned the visibility of the method
 *                                   adjustAngle to protected.
 * Mar 15, 2013  15693    mgamazayc  Added magnification to display state.
 * Jun 10, 2014  3263     bsteffen   Synchronize dataTimes
 * Jul 30, 2015  17761    dfriedman  Fix time matching.
 * Nov 03, 2016  5941     bsteffen   Fix recycling
 * Nov 28, 2017  5863     bsteffen   Change dataTimes to a NavigableSet
 * Jan 09, 2019  7526     tgurney    Remove resource data change listener on
 *                                   dispose
 *
 * </pre>
 *
 * @author mschenke
 */
public abstract class AbstractStormTrackResource
        extends AbstractVizResource<AbstractResourceData, MapDescriptor>
        implements IResourceDataChanged, IContextMenuContributor {

    private StormTrackDisplay display;

    protected StormTrackState displayState;

    private StormTrackUIManager manager;

    /**
     * When this resource is generating its own times instead of matching the
     * other times in the descriptor then this field is used to hold the initial
     * time which the generated times are based off of.
     */
    private DataTime basisTime = null;

    private int maximumFrameCount = -1;

    protected StormTrackUtil trackUtil;

    private boolean keepTrackOfDuration = false;

    private int lastFrameCount = -1;

    public AbstractStormTrackResource(
            GenericToolsResourceData<? extends AbstractStormTrackResource> resourceData,
            LoadProperties loadProperties, MapDescriptor descriptor) {
        super(resourceData, loadProperties, false);
        setDescriptor(descriptor);
        resourceData.addChangeListener(this);

        displayState = new StormTrackState();
        trackUtil = new StormTrackUtil();
        initializeState(displayState);

        if (displayState.duration == -1) {
            keepTrackOfDuration = true;
        }
    }

    @Override
    public void setDescriptor(MapDescriptor descriptor) {
        super.setDescriptor(descriptor);
        if (display != null) {
            display.setDescriptor(descriptor);
        }
    }

    @Override
    public DataTime[] getMatchedDataTimes(DataTime[] timeSteps) {
        synchronized (this.dataTimes) {

            if (basisTime != null) {
                /*
                 * We only want to calculate more data times if the user has
                 * selected more frames than there have been in the past.
                 */
                if (this.descriptor
                        .getNumberOfFrames() > this.maximumFrameCount) {
                    int variance = this.descriptor.getNumberOfFrames()
                            - this.maximumFrameCount;

                    this.maximumFrameCount = this.descriptor
                            .getNumberOfFrames();

                    if (dataTimes.isEmpty()) {
                        dataTimes.add(basisTime);
                    }

                    DataTime earliestTime = this.dataTimes.first();
                    this.fillDataTimeArray(earliestTime, variance);
                }
            } else {
                dataTimes.clear();
                this.maximumFrameCount = this.descriptor.getNumberOfFrames();
                // First time called
                if (timeSteps != null) {
                    for (DataTime dt : timeSteps) {
                        dataTimes.add(dt);
                    }
                }

                if (dataTimes.isEmpty()) {

                    basisTime = new DataTime(
                            SimulatedTime.getSystemTime().getTime());

                    dataTimes.add(basisTime);
                    this.fillDataTimeArray(basisTime,
                            this.descriptor.getNumberOfFrames() - 1);
                }
            }
            return dataTimes.toArray(new DataTime[dataTimes.size()]);
        }
    }

    private void fillDataTimeArray(DataTime startDataTime,
            int numberOfDataTimes) {
        int fifteenMin = 15 * 60 * 1000;
        long time = startDataTime.getRefTime().getTime();
        DataTime currentDataTime = null;

        for (int i = 0; i < numberOfDataTimes; i++) {
            time -= fifteenMin;
            currentDataTime = new DataTime(new Date(time));
            this.dataTimes.add(currentDataTime);
        }
    }

    public boolean isEditable() {
        return getCapability(EditableCapability.class).isEditable();
    }

    @Override
    protected void disposeInternal() {
        if (display != null) {
            display.dispose();
        }
        if (manager != null) {
            manager.dispose();
        }
        this.resourceData.removeChangeListener(this);
        this.dataTimes.clear();
        this.maximumFrameCount = 0;
        this.displayState.geomChanged = true;
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        EditableManager.makeEditable(this, true);
        manager = new StormTrackUIManager(this);
        manager.setTrackUtil(trackUtil);
        // Create the display
        display = new StormTrackDisplay(descriptor, manager);
        descriptor.getTimeMatcher().redoTimeMatching(this);
        descriptor.getTimeMatcher().redoTimeMatching(descriptor);
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        FramesInfo info = paintProps.getFramesInfo();
        if (keepTrackOfDuration && lastFrameCount != info.getFrameCount()) {
            displayState.duration = -1;
            lastFrameCount = info.getFrameCount();
        }

        displayState.color = getCapability(ColorableCapability.class)
                .getColor();
        displayState.lineWidth = getCapability(OutlineCapability.class)
                .getOutlineWidth();
        displayState.lineStyle = getCapability(OutlineCapability.class)
                .getLineStyle();
        // set the magnification for the display state
        displayState.magnification = getCapability(
                MagnificationCapability.class).getMagnification().floatValue();

        PaintProperties newProps = new StormTrackProperties(paintProps,
                displayState);

        display.paint(target, newProps);

        manager.setHandleInput(displayState.isEditable());
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.CAPABILITY) {
            displayState.geomChanged = true;
            if (object instanceof EditableCapability) {
                displayState.editable = ((EditableCapability) object)
                        .isEditable();
            }
            issueRefresh();
        }
    }

    public StormTrackState getStormTrackState() {
        return displayState;
    }

    @Override
    public String getName() {
        return getResourceName();
    }

    /**
     * Adjusts the angle from -180/180 to be between 0/360
     *
     * @param angle
     * @return
     */
    protected double unadjustAngle(double angle) {
        double newVal = angle;
        if (newVal < 0) {
            newVal = 360 - newVal;
        }
        return newVal;
    }

    /**
     * Adjusts the angle to be within the range -180 to +180 degrees.
     *
     * @param angle
     * @return
     */
    protected double adjustAngle(double angle) {
        double newVal = angle % 360;
        if (newVal > 180) {
            newVal -= 360;
        } else if (newVal < -180) {
            newVal += 360;
        }
        return newVal;
    }

    public void resetState() {
        displayState = new StormTrackState();
        initializeState(displayState);
    }

    public StormTrackUIManager getUIManager() {
        return manager;
    }

    protected String getAddVertexText() {
        return "Add Vertex";
    }

    protected String getDeleteVertexText() {
        return "Remove Vertex";
    }

    protected abstract void initializeState(StormTrackState state);

    protected abstract String getResourceName();

    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        if (displayState.mode == Mode.DRAG_ME
                || displayState.displayType != DisplayType.POLY) {
            return;
        }

        if (manager.closeToPoint()) {
            menuManager.add(manager.getDeleteAction());
        } else if (manager.closeToLine()) {
            menuManager.add(manager.getAddAction());
        }
    }

    @Override
    public void propertiesChanged(ResourceProperties props) {
        manager.setHandleInput(props.isVisible());
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        displayState.geomChanged = true;
        issueRefresh();
    }

}
