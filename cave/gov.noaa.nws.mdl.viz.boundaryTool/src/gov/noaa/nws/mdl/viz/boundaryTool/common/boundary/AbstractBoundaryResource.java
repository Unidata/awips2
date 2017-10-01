package gov.noaa.nws.mdl.viz.boundaryTool.common.boundary;

import gov.noaa.nws.mdl.viz.boundaryTool.common.boundary.BoundaryState.Mode;
import gov.noaa.nws.mdl.viz.boundaryTool.common.boundary.BoundaryState.UserAction;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;

import org.eclipse.jface.action.IMenuManager;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
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
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.raytheon.viz.ui.input.EditableManager;

/**
 * Abstract class for drawing boundary object
 * 
 * @author Mamoudou ba
 * @version 1.0
 * 
 *          Modified from A2 "AbstractStormTrackResource" class by renaming the
 *          class name and methods
 */

public abstract class AbstractBoundaryResource extends
        AbstractVizResource<AbstractResourceData, MapDescriptor> implements
        IResourceDataChanged, IContextMenuContributor {

    private BoundaryDisplay display;

    protected BoundaryState displayState;

    protected BoundaryUIManager manager;

    private boolean timeMatchBasis = false;

    private int maximumFrameCount = -1;

    protected BoundaryUtil trackUtil;

    private boolean keepTrackOfDuration = false;

    private int lastFrameCount = -1;

    public AbstractBoundaryResource(
            GenericToolsResourceData<? extends AbstractBoundaryResource> resourceData,
            LoadProperties loadProperties, MapDescriptor descriptor) {
        super(resourceData, loadProperties);
        setDescriptor(descriptor);
        resourceData.addChangeListener(this);
        dataTimes = new ArrayList<DataTime>();

        displayState = new BoundaryState();
        trackUtil = new BoundaryUtil();
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
    public DataTime[] getDataTimes() {
        if (timeMatchBasis) {
            /*
             * We only want to calculate more data times if the user hasselected
             * more frames than there have been in the past.
             */
            if (this.descriptor.getNumberOfFrames() > this.maximumFrameCount) {
                int variance = this.descriptor.getNumberOfFrames()
                        - this.maximumFrameCount;

                this.maximumFrameCount = this.descriptor.getNumberOfFrames();

                DataTime earliestTime = this.dataTimes.get(0);
                this.fillDataTimeArray(earliestTime, variance);
            }
        } else {
            FramesInfo info = descriptor.getFramesInfo();
            dataTimes.clear();
            this.maximumFrameCount = this.descriptor.getNumberOfFrames();
            // First time called
            if (info.getFrameTimes() != null) {
                for (DataTime dt : info.getFrameTimes()) {
                    dataTimes.add(dt);
                }
            }

            if (dataTimes.size() == 0) {
                timeMatchBasis = true;
                /*
                 * Case where this tool is time match basis or no data loaded
                 */
                DataTime currentTime = null;
                if (dataTimes.size() > 0) {
                    currentTime = dataTimes.get(dataTimes.size() - 1);
                } else {
                    currentTime = new DataTime(SimulatedTime.getSystemTime()
                            .getTime());
                }

                dataTimes.add(currentTime);
                this.fillDataTimeArray(currentTime,
                        this.descriptor.getNumberOfFrames() - 1);
            }
        }
        Collections.sort(dataTimes);
        return dataTimes.toArray(new DataTime[dataTimes.size()]);
    }

    private void fillDataTimeArray(DataTime startDataTime, int numberOfDataTimes) {
        long fifteenMin = 15 * TimeUtil.MILLIS_PER_MINUTE;
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
        if (manager != null) {
            manager.dispose();
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        EditableManager.makeEditable(this, true);
        manager = new BoundaryUIManager(this);
        manager.setTrackUtil(trackUtil);
        // Create the display
        display = new BoundaryDisplay(descriptor, manager);
        descriptor.getTimeMatcher().redoTimeMatching(this);
        descriptor.getTimeMatcher().redoTimeMatching(descriptor);
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        FramesInfo info = paintProps.getFramesInfo();
        manager.setHandleInput(displayState.isEditable());
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

        PaintProperties newProps = new BoundaryProperties(paintProps,
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

    public BoundaryState getBoundaryState() {
        return displayState;
    }

    @Override
    public String getName() {
        DataTime[] frameTimes = descriptor.getFramesInfo().getFrameTimes();
        if (frameTimes != null) {
            descriptor.getTimeMatchingMap().put(this, frameTimes);
        }
        return getResourceName();
    }

    public void resetState() {
        displayState = new BoundaryState();
        initializeState(displayState);
    }

    public BoundaryUIManager getUIManager() {
        return manager;
    }

    protected String getAddVertexText() {
        return "Add Vertex";
    }

    protected String getDeleteVertexText() {
        return "Remove Vertex";
    }

    protected abstract void initializeState(BoundaryState state);

    protected abstract String getResourceName();

    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        if (displayState.mode == Mode.DRAG_ME
                || displayState.userAction != UserAction.INSERT_BOUNDARY) {
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
