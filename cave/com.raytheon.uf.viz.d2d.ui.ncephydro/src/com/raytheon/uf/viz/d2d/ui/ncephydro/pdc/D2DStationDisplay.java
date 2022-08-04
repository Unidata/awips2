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
package com.raytheon.uf.viz.d2d.ui.ncephydro.pdc;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.d2d.ui.perspectives.D2D5Pane;
import com.raytheon.uf.viz.pdc.PointDataControlManager;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.events.MapUpdateEvent;
import com.raytheon.viz.hydrocommon.events.StationDisplayUpdateEvent;
import com.raytheon.viz.hydrocommon.listeners.MapUpdateListener;
import com.raytheon.viz.hydrocommon.listeners.StationDisplayListener;
import com.raytheon.viz.hydrocommon.pdc.PDCConstants;
import com.raytheon.viz.hydrocommon.resource.AbstractMultiPointResource;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

/**
 * Displays the river station icons on the D2D perspective map. Stations are
 * pulled from IHFS Location table.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 03, 2018   7379     mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 */

public class D2DStationDisplay
        implements MapUpdateListener, StationDisplayListener {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(D2DStationDisplay.class);

    private static D2DStationDisplay instance;

    /**
     * The D2DMultiPointResource instance.
     */
    private AbstractMultiPointResource gageResource;

    public static final synchronized D2DStationDisplay getInstance() {
        if (instance == null) {
            instance = new D2DStationDisplay();
        }
        return instance;
    }

    /**
     * Private constructor.
     */
    private D2DStationDisplay() {
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        pdcManager.addStationDisplayListener(this);
        HydroDisplayManager.getInstance().addMapUpdateListener(this);
        gageResource = getD2DMultiPointResource();
    }

    @Override
    public void notifyUpdate(StationDisplayUpdateEvent sdue) {
        if (gageResource != null) {
            gageResource.issueRefresh();
        }
    }

    @Override
    public void notifyUpdate(MapUpdateEvent mue) {
        String perspectiveId = getPerspectiveId();

        if (!perspectiveId.equals(D2D5Pane.ID_PERSPECTIVE)) {
            return;
        }

        HydroDisplayManager displayManager = HydroDisplayManager.getInstance();
        if ((gageResource != null) && !gageResource.isDisposed()) {
            resetGageDisplay();
            gageResource.issueRefresh();
        } else if (displayManager.isDrawStation()) {
            gageResource = getD2DMultiPointResource();
        }
    }

    public void redraw() {
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        if ((gageResource != null) && !gageResource.isDisposed()) {
            pdcManager.scheduleRequest(true,
                    PointDataControlManager.REQUEST_TYPE.REQUEST_AD_HOC);

            pdcManager.applyShiftValues();
        }
    }

    public synchronized AbstractMultiPointResource getD2DMultiPointResource() {

        String perspectiveId = getPerspectiveId();
        if (!perspectiveId.equals(D2D5Pane.ID_PERSPECTIVE)) {
            return null;
        }

        if ((gageResource == null) || gageResource.isDisposed()) {
            D2DMultiPointResourceData resourceData = new D2DMultiPointResourceData(
                    "Gages");
            try {
                gageResource = resourceData.construct(new LoadProperties(),
                        EditorUtil.getActiveVizContainer()
                                .getActiveDisplayPane().getDescriptor());
                gageResource.getCapability(ColorableCapability.class)
                        .setColor(PDCConstants.DEFAULT_COLOR);
            } catch (VizException e) {
                statusHandler.error("Error constructing MultiPointResource.",
                        e);
            }
        }
        ResourceProperties props = new ResourceProperties();
        props.setVisible(true);

        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            IDescriptor desc = container.getActiveDisplayPane().getDescriptor();
            if (!desc.getResourceList().containsRsc(gageResource)) {
                desc.getResourceList().add(gageResource, props);
            }
        }

        return gageResource;
    }

    /**
     * remove and restore the gage resource.
     */
    public void resetGageDisplay() {
        gageResource.resetDataMap();

        // force update
        gageResource = getD2DMultiPointResource();
    }

    private String getPerspectiveId() {
        return VizPerspectiveListener.getCurrentPerspectiveManager()
                .getPerspectiveId();

    }

    /**
     * Null out the instance.
     */
    public synchronized void dispose() {
        instance = null;
    }
}
