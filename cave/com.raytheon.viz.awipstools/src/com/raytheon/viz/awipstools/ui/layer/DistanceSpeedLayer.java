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

import java.util.HashSet;
import java.util.Set;

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
 * 15Mar2013    15693      mgamazay    Added magnification capability.
 * Feb 14, 2018 6911       tgurney     Prevent multiple copies of same dialog
 *                                     from being open at once
 *
 * </pre>
 *
 * @author mschenke
 */

public class DistanceSpeedLayer extends AbstractStormTrackResource {

    private static final Set<String> dialogTitlesInUse = new HashSet<>();

    public static final String NAME = "Distance Speed";

    public String dialogTitle = "Distance Speed";

    private DistanceSpeedDialog dialog;

    public DistanceSpeedLayer(
            GenericToolsResourceData<DistanceSpeedLayer> resourceData,
            LoadProperties loadProperties, MapDescriptor descriptor) {
        super(resourceData, loadProperties, descriptor);
        // add magnification capability
        getCapabilities().addCapability(new MagnificationCapability());
        /*
         * Use the dialog title with the lowest number that is not already in
         * use
         */
        synchronized (dialogTitlesInUse) {
            int suffix = 2;
            while (dialogTitlesInUse.contains(dialogTitle)) {
                dialogTitle = "Distance Speed <" + suffix + ">";
                suffix++;
            }
            dialogTitlesInUse.add(dialogTitle);
        }
        reopenDialog();
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        dialog.close();
        synchronized (dialogTitlesInUse) {
            dialogTitlesInUse.remove(dialogTitle);
        }
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
            displayState.lineOfStormsLength = (zoomLevel * desiredPixelWidth
                    * metersPerPixel) / (distanceThreshold * 2);
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
        VizApp.runAsync(() -> {
            if (dialog == null || !dialog.isOpen()) {
                dialog = new DistanceSpeedDialog(VizWorkbenchManager
                        .getInstance().getCurrentWindow().getShell(),
                        DistanceSpeedLayer.this);
                dialog.setBlockOnOpen(false);
                dialog.open();
            } else {
                dialog.bringToTop();
            }

        });
    }
}
