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
package com.raytheon.viz.gfe.core.internal;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang.Validate;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.actions.ShowISCMarkersAction;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.GFETimeMatcher;
import com.raytheon.viz.gfe.core.ISpatialDisplayManager;
import com.raytheon.viz.gfe.core.msgs.IActivatedParmChangedListener;
import com.raytheon.viz.gfe.core.msgs.IDisplayModeChangedListener;
import com.raytheon.viz.gfe.core.msgs.IGlobalSelectionTRChangedListener;
import com.raytheon.viz.gfe.core.msgs.IGridVisibilityChangedListener;
import com.raytheon.viz.gfe.core.msgs.ISpatialEditorTimeChangedListener;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.SetImageOnActiveChangedMsg;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.EditorType;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisMode;
import com.raytheon.viz.gfe.edittool.AbstractGFEEditTool;
import com.raytheon.viz.gfe.rsc.GFEResource;
import com.raytheon.viz.gfe.rsc.GFESystemResource;

/**
 * Abstract Spatial Display Manager for GFE. Most of the implemented methods
 * were pulled out of GFESpatialDisplayManager and slightly modified for
 * compatibility with the OffscreenSpatialDisplayManager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 20, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class AbstractSpatialDisplayManager implements
        ISpatialDisplayManager {

    protected final DataManager dataManager;

    private TimeRange globalTimeRange;

    protected Date seTime;

    protected Parm activeParm;

    protected Parm lastActiveParm;

    private boolean showDescription = true;

    protected boolean showISCMarkers;

    protected boolean showISCUpdateTimeMarker;

    protected boolean showISCSiteIDMarker;

    protected boolean showISCOfficialSymbolMarker;

    protected boolean showISCUpdateTime;

    protected boolean showISCSiteID;

    protected boolean showISCOfficialSymbol;

    protected final Set<IActivatedParmChangedListener> activatedParmChangedListeners;

    protected final Set<IGridVisibilityChangedListener> gridVisibilityChangedListeners;

    protected final Set<IGlobalSelectionTRChangedListener> globalSelectionTRChangedListeners;

    protected final Set<IDisplayModeChangedListener> displayModeChangedListeners;

    protected final Set<ISpatialEditorTimeChangedListener> spatialEditorTimeChangedListeners;

    protected AbstractSpatialDisplayManager(DataManager dataMgr) {
        this.dataManager = dataMgr;
        this.globalTimeRange = new TimeRange(0, 0);
        this.activatedParmChangedListeners = new HashSet<IActivatedParmChangedListener>();
        this.gridVisibilityChangedListeners = new HashSet<IGridVisibilityChangedListener>();
        this.globalSelectionTRChangedListeners = new HashSet<IGlobalSelectionTRChangedListener>();
        this.displayModeChangedListeners = new HashSet<IDisplayModeChangedListener>();
        this.spatialEditorTimeChangedListeners = new HashSet<ISpatialEditorTimeChangedListener>();

        setShowISCMarkers(Activator.getDefault().getPreferenceStore()
                .getBoolean("ShowISCMarkers"));

        setShowISCUpdateTimeMarker(Activator.getDefault().getPreferenceStore()
                .getBoolean("ShowISCUpdateTimeMarker"));

        setShowISCSiteIDMarker(Activator.getDefault().getPreferenceStore()
                .getBoolean("ShowISCSiteIDMarker"));

        setShowISCOfficialSymbolMarker(Activator.getDefault()
                .getPreferenceStore().getBoolean("ShowISCOfficialSymbolMarker"));

        setShowISCUpdateTime(Activator.getDefault().getPreferenceStore()
                .getBoolean("ShowISCUpdateTime"));

        setShowISCSiteID(Activator.getDefault().getPreferenceStore()
                .getBoolean("ShowISCSiteID"));

        setShowISCOfficialSymbol(Activator.getDefault().getPreferenceStore()
                .getBoolean("ShowISCOfficialSymbol"));
    }

    /**
     * Gets any descriptors to be kept in sync with the display manager (can be
     * empty)
     * 
     * @return
     */
    protected abstract IDescriptor[] getDescriptors();

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ISpatialDisplayManager#getActivatedParm()
     */
    public Parm getActivatedParm() {
        return activeParm;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISpatialDisplayManager#getLastActivatedParm()
     */
    @Override
    public Parm getLastActivatedParm() {
        return lastActiveParm;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ISpatialDisplayManager#
     * addActivatedParmChangedListener
     * (com.raytheon.viz.gfe.core.msgs.IActivatedParmChangedListener)
     */
    public void addActivatedParmChangedListener(
            IActivatedParmChangedListener listener) {
        Validate.notNull(listener, "Attempting to add null listener");
        this.activatedParmChangedListeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ISpatialDisplayManager#
     * removeActivatedParmChangedListener
     * (com.raytheon.viz.gfe.core.msgs.IActivatedParmChangedListener)
     */
    public void removeActivatedParmChangedListener(
            IActivatedParmChangedListener parmChangeListener) {
        this.activatedParmChangedListeners.remove(parmChangeListener);
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.core.ISpatialDisplayManager#
     * addDisplayModeChangedListener
     * (com.raytheon.viz.gfe.core.msgs.IDisplayModeChangedListener)
     */
    @Override
    public void addDisplayModeChangedListener(
            IDisplayModeChangedListener listener) {
        Validate.notNull(listener, "Attempting to add null listener");
        this.displayModeChangedListeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.core.ISpatialDisplayManager#
     * removeDisplayModeChangedListener
     * (com.raytheon.viz.gfe.core.msgs.IDisplayModeChangedListener)
     */
    @Override
    public void removeDisplayModeChangedListener(
            IDisplayModeChangedListener listener) {
        this.displayModeChangedListeners.remove(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ISpatialDisplayManager#
     * addGridVisibilityChangedListener
     * (com.raytheon.viz.gfe.core.msgs.IGridVisibilityChangedListener)
     */
    public void addGridVisibilityChangedListener(
            IGridVisibilityChangedListener listener) {
        Validate.notNull(listener, "Attempting to add null listener");
        this.gridVisibilityChangedListeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ISpatialDisplayManager#
     * removeGridVisibilityChangedListener
     * (com.raytheon.viz.gfe.core.msgs.IGridVisibilityChangedListener)
     */
    public void removeGridVisibilityChangedListener(
            IGridVisibilityChangedListener parmChangeListener) {
        this.gridVisibilityChangedListeners.remove(parmChangeListener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ISpatialDisplayManager#
     * addGlobalSelectionTRChangedListener
     * (com.raytheon.viz.gfe.core.msgs.IGlobalSelectionTRChangedListener)
     */
    @Override
    public void addGlobalSelectionTRChangedListener(
            IGlobalSelectionTRChangedListener listener) {
        Validate.notNull(listener, "Attempting to add null listener");
        this.globalSelectionTRChangedListeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ISpatialDisplayManager#
     * removeGlobalSelectionTRChangedListener
     * (com.raytheon.viz.gfe.core.msgs.IGlobalSelectionTRChangedListener)
     */
    @Override
    public void removeGlobalSelectionTRChangedListener(
            IGlobalSelectionTRChangedListener listener) {
        this.globalSelectionTRChangedListeners.remove(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.core.ISpatialDisplayManager#
     * addSpatialEditorTimeChangedListener
     * (com.raytheon.viz.gfe.core.msgs.ISpatialEditorTimeChangedListener)
     */
    @Override
    public void addSpatialEditorTimeChangedListener(
            ISpatialEditorTimeChangedListener listener) {
        Validate.notNull(listener, "Attempting to add null listener");
        this.spatialEditorTimeChangedListeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.core.ISpatialDisplayManager#
     * removeSpatialEditorTimeChangedListener
     * (com.raytheon.viz.gfe.core.msgs.ISpatialEditorTimeChangedListener)
     */
    @Override
    public void removeSpatialEditorTimeChangedListener(
            ISpatialEditorTimeChangedListener listener) {
        this.spatialEditorTimeChangedListeners.remove(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISpatialDisplayManager#addEditTool(com.raytheon
     * .viz.gfe.edittool.AbstractGFEEditTool)
     */
    @Override
    public void addEditTool(AbstractGFEEditTool editTool) throws VizException {
        IDescriptor[] descriptors = getDescriptors();
        for (IDescriptor desc : descriptors) {
            ResourceList rl = desc.getResourceList();
            List<GFESystemResource> rscs = rl
                    .getResourcesByTypeAsType(GFESystemResource.class);
            for (GFESystemResource rsc : rscs) {
                rsc.addEditTool(editTool);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISpatialDisplayManager#removeEditTool(com.raytheon
     * .viz.gfe.edittool.AbstractGFEEditTool)
     */
    @Override
    public void removeEditTool(AbstractGFEEditTool editTool)
            throws VizException {
        for (IDescriptor desc : getDescriptors()) {
            List<GFESystemResource> rscs = desc.getResourceList()
                    .getResourcesByTypeAsType(GFESystemResource.class);
            for (GFESystemResource rsc : rscs) {
                rsc.removeEditTool(editTool);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISpatialDisplayManager#setDisplayMode(com.raytheon
     * .viz.gfe.core.parm.Parm,
     * com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisMode)
     */
    @Override
    public void setDisplayMode(Parm parm, VisMode mode) {
        for (IDescriptor descriptor : getDescriptors()) {
            List<GFEResource> rscs = descriptor.getResourceList()
                    .getResourcesByTypeAsType(GFEResource.class);
            for (GFEResource rsc : rscs) {
                Parm p = rsc.getParm();
                if (p.equals(parm)) {
                    p.getDisplayAttributes().setVisMode(mode);
                    rsc.reset();
                } else if (mode.equals(VisMode.IMAGE)) {
                    p.getDisplayAttributes().setVisMode(VisMode.GRAPHIC);
                    rsc.reset();
                }
            }
        }

        for (IDisplayModeChangedListener listener : displayModeChangedListeners) {
            listener.displayModeChanged(EditorType.SPATIAL, parm, mode);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ISpatialDisplayManager#getSystemResource()
     */
    @Override
    public GFESystemResource getSystemResource() throws VizException {
        for (IDescriptor descriptor : getDescriptors()) {
            List<GFESystemResource> rscs = descriptor.getResourceList()
                    .getResourcesByTypeAsType(GFESystemResource.class);
            for (GFESystemResource rsc : rscs) {
                return rsc;
            }
        }

        return null;
    }

    /**
     * Returns the resource pair for a specific Parm and null if the resource
     * pair cannot be found.
     * 
     * @param parm
     *            The parm that is associated to the resource pair
     * @return The resource pair that matches the parm or null if the resource
     *         pair is not found
     */
    @Override
    public ResourcePair getResourcePair(Parm parm) {
        for (IDescriptor descriptor : getDescriptors()) {
            ResourceList resourceList = descriptor.getResourceList();

            for (ResourcePair resourcePair : resourceList) {
                AbstractVizResource<?, ?> rsc = resourcePair.getResource();
                if (rsc instanceof GFEResource) {
                    GFEResource grsc = (GFEResource) rsc;
                    if (grsc.getParm().equals(parm)) {
                        return resourcePair;
                    }
                }
            }
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISpatialDisplayManager#getCurrentlyEnabledParms
     * ()
     */
    @Override
    public Parm[] getCurrentlyEnabledParms() {
        for (IDescriptor descriptor : getDescriptors()) {
            List<Parm> parmList = new ArrayList<Parm>();
            List<GFEResource> rscs = descriptor.getResourceList()
                    .getResourcesByTypeAsType(GFEResource.class);
            for (GFEResource rsc : rscs) {
                if (rsc.getProperties().isVisible()) {
                    parmList.add(rsc.getParm());
                }
            }
            if (parmList.size() > 0) {
                return parmList.toArray(new Parm[parmList.size()]);
            }
        }

        return new Parm[0];
    }

    @Override
    public void makeVisible(Parm parm, boolean visible, boolean makeOnlyVisible) {
        for (IDescriptor descriptor : getDescriptors()) {
            makeVisible(descriptor, parm, visible, makeOnlyVisible);
        }

        for (IGridVisibilityChangedListener listener : this.gridVisibilityChangedListeners) {
            listener.gridVisibilityChanged(parm, visible, makeOnlyVisible);
        }
    }

    private void makeVisible(IDescriptor descriptor, Parm parm,
            boolean visible, boolean makeOnlyVisible) {
        List<GFEResource> rscs = descriptor.getResourceList()
                .getResourcesByTypeAsType(GFEResource.class);
        for (GFEResource rsc : rscs) {
            ResourceProperties props = rsc.getProperties();
            Parm p = rsc.getParm();
            if (p.equals(parm)) {
                props.setVisible(visible);
            } else if (makeOnlyVisible) {
                props.setVisible(false);
            }
        }

        try {
            // Parm visibility affects GFE time matching
            descriptor.redoTimeMatching();
        } catch (VizException e) {
            UFStatus.getHandler().handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISpatialDisplayManager#activateParm(com.raytheon
     * .viz.gfe.core.parm.Parm)
     */
    public void activateParm(Parm parmToActivate)
            throws GFEOperationFailedException {
        // Keep any resources on descriptors in sync
        for (IDescriptor descriptor : getDescriptors()) {
            ResourceList resourceList = descriptor.getResourceList();
            List<GFEResource> rscs = resourceList
                    .getResourcesByTypeAsType(GFEResource.class);
            for (GFEResource rsc : rscs) {
                ResourceProperties props = rsc.getProperties();
                // skip the quick view resource
                if (props.isSystemResource()) {
                    continue;
                }
                Parm parm = ((GFEResource) rsc).getParm();
                if (parm.equals(parmToActivate)) {
                    props.setVisible(true);
                }
                ((GFEResource) rsc).reset();
            }
        }

        this.activeParm = parmToActivate;
        if (activeParm != null) {
            if (Message.inquireLastMessage(SetImageOnActiveChangedMsg.class)
                    .isEnabled()) {
                setDisplayMode(parmToActivate, VisMode.IMAGE);
            }
            this.lastActiveParm = parmToActivate;
        }

        for (IActivatedParmChangedListener listener : this.activatedParmChangedListeners) {
            listener.activatedParmChanged(parmToActivate);
        }
    }

    @Override
    public boolean getShowDescription() {
        return this.showDescription;
    }

    @Override
    public void setShowDescription(boolean showDescription) {
        this.showDescription = showDescription;
    }

    @Override
    public boolean isShowISCMarkers() {
        return this.showISCMarkers;
    }

    @Override
    public void setShowISCMarkers(boolean showISCMarkers) {
        this.showISCMarkers = showISCMarkers;
        updateElement(ShowISCMarkersAction.COMMAND_ID, this.showISCMarkers);
    }

    @Override
    public boolean isShowISCUpdateTimeMarker() {
        return showISCUpdateTimeMarker;
    }

    @Override
    public void setShowISCUpdateTimeMarker(boolean showISCUpdateTimeMarker) {
        this.showISCUpdateTimeMarker = showISCUpdateTimeMarker;
    }

    @Override
    public boolean isShowISCSiteIdMarker() {
        return showISCSiteIDMarker;
    }

    @Override
    public void setShowISCSiteIDMarker(boolean showISCSiteIDMarker) {
        this.showISCSiteIDMarker = showISCSiteIDMarker;
    }

    @Override
    public boolean isShowISCOfficialSymbolMarker() {
        return showISCOfficialSymbolMarker;
    }

    @Override
    public void setShowISCOfficialSymbolMarker(
            boolean showISCOfficialSymbolMarker) {
        this.showISCOfficialSymbolMarker = showISCOfficialSymbolMarker;
    }

    @Override
    public boolean isShowIscSampleUpdateTime() {
        return showISCUpdateTime;
    }

    @Override
    public void setShowISCUpdateTime(boolean showISCUpdateTime) {
        this.showISCUpdateTime = showISCUpdateTime;
    }

    @Override
    public boolean isShowISCSiteID() {
        return showISCSiteID;
    }

    @Override
    public void setShowISCSiteID(boolean showISCSiteID) {
        this.showISCSiteID = showISCSiteID;
    }

    @Override
    public boolean isShowISCOfficialSymbol() {
        return showISCOfficialSymbol;
    }

    @Override
    public void setShowISCOfficialSymbol(boolean showISCOfficialSymbol) {
        this.showISCOfficialSymbol = showISCOfficialSymbol;
    }

    @Override
    public Date getSpatialEditorTime() {
        // Check descriptors first for current time
        IDescriptor[] descs = getDescriptors();
        for (IDescriptor desc : descs) {
            FramesInfo fi = desc.getFramesInfo();
            DataTime dt = fi.getCurrentFrame();
            if (dt != null) {
                return dt.getRefTime();
            }
        }
        // No descriptors or no set time, use seTime
        return seTime;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISpatialDisplayManager#setSpatialEditorTime
     * (java.util.Date)
     */
    @Override
    public void setSpatialEditorTime(Date date) {
        this.seTime = date;
        // Keep descriptors in sync
        for (IDescriptor desc : getDescriptors()) {
            setSpatialEditorTime(desc, date);
        }
    }

    protected void setSpatialEditorTime(IDescriptor descriptor, Date date) {
        AbstractTimeMatcher matcher = descriptor.getTimeMatcher();
        if (matcher instanceof GFETimeMatcher) {
            ((GFETimeMatcher) matcher).setSelectedDate(date);
            try {
                // Selected spatial editor time affects GFE time matching
                matcher.redoTimeMatching(descriptor);
            } catch (VizException e) {
                UFStatus.getHandler().handle(Priority.PROBLEM,
                        "Error redoing time matching", e);
            }
        }
        if (date != null) {
            descriptor.getFrameCoordinator().changeFrame(date);
        }
    }

    private void updateElement(String commandId, boolean state) {
        if (PlatformUI.isWorkbenchRunning()) {
            ICommandService service = (ICommandService) PlatformUI
                    .getWorkbench().getService(ICommandService.class);

            service.refreshElements(commandId, null);
        }
    }

    @Override
    public void toggleVisibility(Parm parm) {
        Boolean visible = null;
        // Check for parm on any descriptor, use visiblity of first found
        for (IDescriptor descriptor : getDescriptors()) {
            ResourceList resourceList = descriptor.getResourceList();
            List<GFEResource> rscs = resourceList
                    .getResourcesByTypeAsType(GFEResource.class);
            for (GFEResource rsc : rscs) {
                Parm p = rsc.getParm();
                if (p.equals(parm)) {
                    visible = rsc.getProperties().isVisible();
                    break;
                }
            }
            if (visible != null) {
                break;
            }
        }

        if (visible != null) {
            // Only if resource visibilty was found do we toggle and notify
            visible = !visible;
            for (IDescriptor descriptor : getDescriptors()) {
                makeVisible(descriptor, parm, visible, false);
            }

            for (IGridVisibilityChangedListener listener : this.gridVisibilityChangedListeners) {
                listener.gridVisibilityChanged(parm, visible, false);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISpatialDisplayManager#setGlobalTimeRange(com
     * .raytheon.edex.plugin.time.TimeRange)
     */
    @Override
    public void setGlobalTimeRange(TimeRange timeRange) {
        this.globalTimeRange = timeRange;

        for (IGlobalSelectionTRChangedListener listener : this.globalSelectionTRChangedListeners) {
            listener.globalSelectionTRChanged(timeRange);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISpatialDisplayManager#getGlobalTimeRange()
     */
    @Override
    public TimeRange getGlobalTimeRange() {
        return this.globalTimeRange;
    }

}
