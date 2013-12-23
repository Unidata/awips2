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

import java.util.ArrayList;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.viz.awipstools.common.stormtrack.AbstractStormTrackResource;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.LabelMode;
import com.raytheon.viz.awipstools.ui.dialog.DistanceSpeedDialog;
import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * Port of Distance Speed tool
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/02/2011   7975       bkowal      A NULL pointer exception will
 *                                     no longer be thrown when the
 *                                     Distance/Speed tool is opened
 *                                     in multiple panes.
 * 02/15/2011   7975       bkowal      The Distance Speed Tools will
 *                                     operate independently of each other;
 *                                     the dialogs will have unique titles.
 * 15Mar2013	15693	mgamazaychikov Added magnification capability.
 * 2013-12-20   ss#114     D. Friedman Change 3 of 3
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DistanceSpeedLayer extends AbstractStormTrackResource {

    public static ArrayList<String> dialogsInUse = new ArrayList<String>();

    public static final String NAME = "Distance Speed";

    /** Distance Speed dialog */
    public String dialogTitle = "Distance Speed";

    private DistanceSpeedDialog dialog;

    public DistanceSpeedLayer(
            GenericToolsResourceData<DistanceSpeedLayer> resourceData,
            LoadProperties loadProperties, MapDescriptor descriptor) {
        super(resourceData, loadProperties, descriptor);
    	// add magnification capability
        getCapabilities().addCapability(new MagnificationCapability());
        this.determineDialogTitle();
        reopenDialog();
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        dialog.close();
        DistanceSpeedLayer.dialogsInUse.remove(this.dialogTitle);
    }

    private void determineDialogTitle() {
        /* The Default Dialog Title. */
        this.dialogTitle = "Distance Speed";

        /* We Want To Use The First Available Dialog Title. */
        int counter = 1;
        while (DistanceSpeedLayer.dialogsInUse.contains(this.dialogTitle)) {
            ++counter;
            if (counter > 1) {
                this.dialogTitle = "Distance Speed <" + counter + ">";
            }
        }
        DistanceSpeedLayer.dialogsInUse.add(this.dialogTitle);
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (displayState.lineOfStormsLength == -1) {
            double widthInMeters = this.getDescriptor().getMapWidth();
            double zoomLevel = paintProps.getZoomLevel();
            double widthInPixels = paintProps.getCanvasBounds().width;
            double metersPerPixel = widthInMeters / widthInPixels;
            double desiredPixelWidth = widthInPixels / 6;
            double distanceThreshold = (paintProps.getView().getExtent()
                    .getWidth() / paintProps.getCanvasBounds().width) * 10;
            displayState.lineOfStormsLength = (zoomLevel * desiredPixelWidth * metersPerPixel)
                    / (distanceThreshold * 2);
        }

        super.paintInternal(target, paintProps);
    }

    @Override
    public String getResourceName() {
        return NAME;
    }

    @Override
    protected void initializeState(StormTrackState state) {
        FramesInfo currInfo = descriptor.getFramesInfo();
        // Setup the initial state for the storm track
        // Default angle for POINT
        displayState.displayType = StormTrackState.DisplayType.POINT;
        displayState.labelMode = LabelMode.SPEED;
        displayState.dragMePoint = null;
        displayState.dragMeLine = null;
        // default for POLY, calculated in paintInternal
        displayState.lineOfStormsLength = -1;
        displayState.mode = StormTrackState.Mode.DRAG_ME;
        displayState.numDragMePoints = 1;
        displayState.pivotIndex = trackUtil.getCurrentFrame(currInfo);
        displayState.otherPivotIndex = displayState.pivotIndex > 0 ? 0
                : trackUtil.getFrameCount(currInfo) - 1;
        displayState.thingToDragTo = "feature";
    }

    /**
     * Re-opens the dialog if closed
     */
    public void reopenDialog() {
        // Open the dialog
        if (dialog == null || dialog.getShell() == null
                || dialog.getShell().isDisposed()) {
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    dialog = new DistanceSpeedDialog(VizWorkbenchManager
                            .getInstance().getCurrentWindow().getShell(),
                            DistanceSpeedLayer.this);
                    dialog.setBlockOnOpen(false);
                    dialog.open();
                }
            });

        }
    }
}
