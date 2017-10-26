package gov.noaa.nws.mdl.viz.boundaryTool.ui.layer;

import gov.noaa.nws.mdl.viz.boundaryTool.boundaries.state.xml.ReadBoundariesXmlFile;
import gov.noaa.nws.mdl.viz.boundaryTool.common.boundary.AbstractBoundaryResource;
import gov.noaa.nws.mdl.viz.boundaryTool.common.boundary.BoundaryState;
import gov.noaa.nws.mdl.viz.boundaryTool.common.boundary.BoundaryState.LabelMode;
import gov.noaa.nws.mdl.viz.boundaryTool.common.boundary.BoundaryUIManager;
import gov.noaa.nws.mdl.viz.boundaryTool.ui.dialog.BoundaryEditorDialog;

import java.io.FileNotFoundException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.TimeZone;

import javax.xml.datatype.DatatypeConfigurationException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.opengis.coverage.grid.GridEnvelope;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ProgressiveDisclosureProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.input.EditableManager;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * @author Mamoudou ba
 * @version 1.0
 * 
 *          April 2011: Based on A2 "TimeOfArrivalLayer" class
 */

public class BoundaryEditorLayer extends AbstractBoundaryResource {

    public static class BoundaryEditorState {
        public Coordinate loc;

        public Coordinate mouseLoc;

        // public String text = "Drag me to Point of Arrival";

        public double distance;

        public boolean changed;
    }

    private final DateFormat timeFormat = new SimpleDateFormat("HH:mm");

    public static final String NAME = "Boundary Editor";

    private static final int PD_MAX_WIDTH = 1999000;

    private IWireframeShape jazzyExtras;

    private BoundaryEditorDialog dialog;

    private BoundaryEditorState boundaryEditState;

    private ProgressiveDisclosureProperties pdProps;

    private Shell shell;

    private Cursor movePoint;

    private boolean hovering = false;

    private InputAdapter adapter = new InputAdapter() {
        @Override
        public boolean handleMouseMove(int x, int y) {
            Coordinate mouse = new Coordinate(x, y);
            hovering = false;
            Coordinate loc = boundaryEditState.loc;

            if (loc != null) {
                if (BoundaryUIManager.getCoordinateIndex(
                        BoundaryEditorLayer.this, new Coordinate[] { loc },
                        mouse) > -1) {
                    shell.setCursor(movePoint);
                    hovering = true;
                }
            }

            return super.handleMouseMove(x, y);
        }

        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {

            if (mouseButton == 1 && hovering) {
                boundaryEditState.mouseLoc = new Coordinate(
                        boundaryEditState.loc);
                issueRefresh();
                return true;
            }

            return false;
        }

        // November 25 2013 - updated from TimeOfArrivalLayer
        @Override
        public boolean handleMouseDownMove(int arg_x, int arg_y, int mouseButton) {
            double x = arg_x;
            double y = arg_y;
            if (mouseButton == 1 && hovering) {

                // check if point of arrival is off the drawn map, draw at
                // closest border if it is
                IDisplayPane pane = getResourceContainer()
                        .getActiveDisplayPane();
                double[] world = pane.screenToGrid(x, y, 0);
                GridEnvelope ge = pane.getDescriptor().getGridGeometry()
                        .getGridRange();
                IExtent extent = new PixelExtent(ge);
                if (world == null) {
                    return true;
                } else if (extent.contains(world) == false) {
                    // snap x coord to closest edge if out of bounds
                    if (world[0] > extent.getMaxX()) {
                        world[0] = extent.getMaxX();
                    } else if (world[0] < extent.getMinX()) {
                        world[0] = extent.getMinX();
                    }

                    // snap y coord to closest edge if out of bounds
                    if (world[1] > extent.getMaxY()) {
                        world[1] = extent.getMaxY();
                    } else if (world[1] < extent.getMinY()) {
                        world[1] = extent.getMinY();
                    }

                    // translate back to screen coords
                    double[] screen = pane.gridToScreen(world);
                    x = screen[0];
                    y = screen[1];
                }

                // translate from screen to lat/lon
                Coordinate c = getResourceContainer().translateClick(x, y);
                if (c != null) {
                    boundaryEditState.mouseLoc = c;
                    issueRefresh();
                }
                return true;
            }

            return false;
        }

        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            if (mouseButton == 1 && hovering) {
                boundaryEditState.loc = boundaryEditState.mouseLoc;
                boundaryEditState.mouseLoc = null;
                boundaryEditState.changed = true;

                displayState.geomChanged = true;

                issueRefresh();
                return true;
            }

            return false;
        }
    };

    public BoundaryEditorLayer(
            GenericToolsResourceData<BoundaryEditorLayer> resourceData,
            LoadProperties loadProperties, MapDescriptor descriptor) {
        super(resourceData, loadProperties, descriptor);
        // add magnification capability
        getCapabilities().addCapability(new MagnificationCapability());
        this.pdProps = new ProgressiveDisclosureProperties();
        this.pdProps.setMaxDisplayWidth(BoundaryEditorLayer.PD_MAX_WIDTH);

        timeFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        // reopenDialog();
        boundaryEditState = new BoundaryEditorState();

        shell = VizWorkbenchManager.getInstance().getCurrentWindow().getShell();

        Display display = Display.getCurrent();

        movePoint = display.getSystemCursor(SWT.CURSOR_HAND);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.registerMouseHandler(adapter);
        }
        reopenDialog();
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        dialog.close();
        displayState.dialogObject = null;
        if (jazzyExtras != null) {
            jazzyExtras.dispose();
        }

        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(adapter);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * gov.noaa.nws.mdl.viz.boundaryTool.common.boundary.AbstractBoundaryResource
     * #getResourceName()
     */
    @Override
    protected String getResourceName() {
        return NAME;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * gov.noaa.nws.mdl.viz.boundaryTool.common.boundary.AbstractBoundaryResource
     * #initializeState
     * (gov.noaa.nws.mdl.viz.boundaryTool.common.boundary.BoundaryState)
     */
    @Override
    protected void initializeState(BoundaryState state) {
        IDisplayPaneContainer container = getResourceContainer();
        if (container.getLoopProperties().isLooping()) {
            container.getLoopProperties().setLooping(false);
            state.loopingWasOn = true;
        }
        FramesInfo info = descriptor.getFramesInfo();
        // Setup the initial state for the storm track
        // Default angle for POINT
        displayState.userAction = BoundaryState.UserAction.READ_ACTIVE_BOUNDARIES;

        displayState.labelMode = LabelMode.TIME;
        state.angle = 90;
        state.boundaryId = 0;
        state.dragMePoint = null;
        state.dragMeLine = null;
        // default for POLY, calculated usually
        state.lineOfStormsLength = 1000;
        state.mode = BoundaryState.Mode.DRAG_ME;
        state.numDragMePoints = 1;
        state.pivotIndex = trackUtil.getCurrentFrame(info);
        state.otherPivotIndex = displayState.pivotIndex > 0 ? 0 : trackUtil
                .getFrameCount(info) - 1;
        state.thingToDragTo = "boundary location";

        ReadBoundariesXmlFile readXMLFile = new ReadBoundariesXmlFile();
        try {
            readXMLFile.readBoundariesXmlFile(state);
            readXMLFile = null;
        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.PROBLEM, "No such file exists", e);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "VisException", e);
        } catch (DatatypeConfigurationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Data type configuration problem", e);
        }

    }

    /**
     * Re-opens the dialog if closed
     */
    public void reopenDialog() {
        // Open the dialog
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (dialog == null || dialog.getShell() == null
                        || dialog.getShell().isDisposed()) {
                    dialog = new BoundaryEditorDialog(VizWorkbenchManager
                            .getInstance().getCurrentWindow().getShell(),
                            BoundaryEditorLayer.this);
                    dialog.setBlockOnOpen(false);
                    dialog.open();
                    displayState.dialogObject = dialog;
                }
            }
        });
    }

    // November 25, 2013

    public void makeEditableAndReopenDialog() {
        EditableManager.makeEditable(this, true);
        reopenDialog();
    }

}