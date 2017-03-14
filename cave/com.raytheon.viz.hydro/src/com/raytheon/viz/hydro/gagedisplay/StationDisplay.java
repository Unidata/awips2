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
package com.raytheon.viz.hydro.gagedisplay;

import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ProgressiveDisclosureProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.hydro.perspective.HydroPerspectiveManager;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants.QueryMode;
import com.raytheon.viz.hydro.pointdatacontrol.PointDataControlManager;
import com.raytheon.viz.hydro.resource.DamLocationResource;
import com.raytheon.viz.hydro.resource.DamLocationResourceData;
import com.raytheon.viz.hydro.resource.MultiPointResource;
import com.raytheon.viz.hydro.resource.MultiPointResourceData;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.events.MapUpdateEvent;
import com.raytheon.viz.hydrocommon.events.StationDisplayUpdateEvent;
import com.raytheon.viz.hydrocommon.listeners.MapUpdateListener;
import com.raytheon.viz.hydrocommon.listeners.StationDisplayListener;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Displays the stations on the Hydro Perspective map. Stations are pulled from
 * IHFS Location table
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jun 17, 2008 1194        M. Duff     Initial creation.
 * Nov 05, 2008   ---      D. Hladky    refactor and make work.
 * 
 * </pre>
 * 
 * @author M. Duff
 * @version 1.0
 */

public class StationDisplay implements MapUpdateListener,
        StationDisplayListener {
    /** Singleton instance of this class */
    private static StationDisplay stationDisplay = null;

    /** The MultiPointResource */
    private MultiPointResource mpr = null;

    /** The DamLocationResource */
    private DamLocationResource dlr = null;

    /**
     * Get an instance of this singleton.
     * 
     * @return Instance of this class
     */
    public static synchronized StationDisplay getInstance() {
        if (stationDisplay == null) {
            stationDisplay = new StationDisplay();
        }
        return stationDisplay;
    }

    /**
     * Constructor
     */
    private StationDisplay() {
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        pdcManager.addMapUpdateListener(this);
        pdcManager.addStationDisplayListener(this);

        /*
         * Update the hydroview map at the frequency defined in the
         * Apps_defaults file.
         */
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        String refreshMinutesStr = appsDefaults
                .getToken(PDCConstants.HV_REFRESH_MINUTES);

        int refreshMinutes = Integer.parseInt(refreshMinutesStr)
                * HydroConstants.MILLIS_PER_MINUTE;
        int delay = 0; // Don't delay, just update
        Timer timer = new Timer();

        timer.schedule(new TimerTask() {
            @Override
            public void run() {
                String perspectiveId = null;
                final IPerspectiveDescriptor[] desc = new IPerspectiveDescriptor[1];

                // Get the perspective, must use this since we're not in the UI
                // thread
                VizApp.runSync(new Runnable() {

                    @Override
                    public void run() {
                        IWorkbenchWindow window = PlatformUI.getWorkbench()
                                .getActiveWorkbenchWindow();
                        IWorkbenchPage page = window.getActivePage();
                        desc[0] = page.getPerspective();
                    }
                });

                perspectiveId = desc[0].getId();
                if (perspectiveId
                        .equals(HydroPerspectiveManager.HYDRO_PERSPECTIVE)) {
                    redraw();
                }
            }
        }, delay, refreshMinutes);
    }

    /**
     * Gets the DamLocationResource.
     * 
     * @return the DamLocationResource
     */
    public DamLocationResource getDamLocationResource() {
        if ((dlr == null) || dlr.isDisposed()) {
            AppsDefaults appsDefaults = AppsDefaults.getInstance();
            String iconColor = appsDefaults.getToken("dam_icon_color");
            RGB damColor = RGBColors.getRGBColor(iconColor);
            DamLocationResourceData hydroPointResourceData = new DamLocationResourceData(
                    "Dam Sites");
            try {
                dlr = hydroPointResourceData.construct(new LoadProperties(),
                        EditorUtil.getActiveVizContainer()
                                .getActiveDisplayPane().getDescriptor());
                dlr.getCapability(ColorableCapability.class).setColor(damColor);
            } catch (VizException e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
            }
        }
        ResourceProperties props = new ResourceProperties();
        props.setMapLayer(true);
        props.setVisible(true);
        props.setPdProps(new ProgressiveDisclosureProperties());

        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            IDescriptor desc = container.getActiveDisplayPane().getDescriptor();
            if (!desc.getResourceList().containsRsc(dlr)) {
                desc.getResourceList().add(dlr, props);
            }
        }

        return dlr;
    }

    /**
     * Get the MultiPointResource resource
     * 
     * @return The MultiPointResource
     */
    public synchronized MultiPointResource getMultiPointResource() {

        String perspectiveId = null;
        final IPerspectiveDescriptor[] pDesc = new IPerspectiveDescriptor[1];

        // Get the perspective, must use this since we're not in the UI
        // thread
        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                IWorkbenchWindow window = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow();
                IWorkbenchPage page = window.getActivePage();
                pDesc[0] = page.getPerspective();
            }
        });

        perspectiveId = pDesc[0].getId();
        if (!perspectiveId.equals(HydroPerspectiveManager.HYDRO_PERSPECTIVE)) {
            return null;
        }

        if ((mpr == null) || mpr.isDisposed()) {
            MultiPointResourceData resourceData = new MultiPointResourceData(
                    "Gages");
            try {
                mpr = resourceData.construct(new LoadProperties(), EditorUtil
                        .getActiveVizContainer().getActiveDisplayPane()
                        .getDescriptor());
                mpr.getCapability(ColorableCapability.class).setColor(
                        PDCConstants.DEFAULT_COLOR);
            } catch (VizException e) {
                e.printStackTrace();
            }
        }
        ResourceProperties props = new ResourceProperties();
        props.setVisible(true);
        props.setPdProps(new ProgressiveDisclosureProperties());

        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            IDescriptor desc = container.getActiveDisplayPane().getDescriptor();
            if (!desc.getResourceList().containsRsc(mpr)) {
                desc.getResourceList().add(mpr, props);
                HydroDisplayManager.getInstance().setDisplayedResource(mpr);
            }
        }

        return mpr;
    }

    /**
     * remove and restore the gage resource.
     */
    public void resetGageDisplay() {
        mpr.resetDataMap();

        // force update
        mpr = getMultiPointResource();
    }

    /**
     * remove and restore the dam resource.
     */
    public void resetDamDisplay() {
        dlr.resetDamMap();

        // force update
        dlr = getDamLocationResource();
    }

    /**
     * Method called when a Map Update Event is fired.
     * 
     * @param MapUpdateEvent
     */
    @Override
    public void notifyUpdate(MapUpdateEvent mue) {
        String perspectiveId = null;
        final IPerspectiveDescriptor[] desc = new IPerspectiveDescriptor[1];

        // Get the perspective, must use this since we're not in the UI
        // thread
        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                IWorkbenchWindow window = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow();
                IWorkbenchPage page = window.getActivePage();
                desc[0] = page.getPerspective();
            }
        });

        perspectiveId = desc[0].getId();
        if (!perspectiveId.equals(HydroPerspectiveManager.HYDRO_PERSPECTIVE)) {
            return;
        }

        HydroDisplayManager displayManager = HydroDisplayManager.getInstance();
        if ((mpr != null) && !mpr.isDisposed()) {
            resetGageDisplay();
            mpr.issueRefresh();
        } else if (displayManager.isDrawStation()) {
            mpr = getMultiPointResource();
        }

        if ((dlr != null) && !dlr.isDisposed()) {
            resetDamDisplay();
            dlr.issueRefresh();
        } else if ((displayManager.getDamList() != null)
                && (displayManager.getDamList().size() > 0)) {
            dlr = getDamLocationResource();
            dlr.getResourceData().setDamList(displayManager.getDamList());
        }
    }

    public void redraw() {
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        if ((mpr != null) && !mpr.isDisposed()) {
            if (pcOptions.getQueryMode() == QueryMode.AD_HOC_MODE
                    .getQueryMode()) {
                pdcManager.scheduleRequest(true,
                        PointDataControlManager.REQUEST_TYPE.REQUEST_AD_HOC);
            } else {
                pdcManager.scheduleRequest(true,
                        PointDataControlManager.REQUEST_TYPE.REQUEST_TIME_STEP);
            }

            pdcManager.applyShiftValues();
        }
        if ((dlr != null) && !dlr.isDisposed()) {
            dlr.issueRefresh();
        }
    }

    /**
     * Method called when a Station Display Update Event is fired.
     * 
     * @param StationDisplayUpdateEvent
     *            The updated station data
     */
    @Override
    public void notifyUpdate(StationDisplayUpdateEvent sdue) {
        if (mpr != null) {
            mpr.issueRefresh();
        }
    }
}