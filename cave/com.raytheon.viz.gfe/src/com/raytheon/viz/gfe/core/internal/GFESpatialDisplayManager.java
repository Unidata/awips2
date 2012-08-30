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

import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.IWorkbenchWindow;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.opengis.geometry.Envelope;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.PythonPreferenceStore;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.ISampleSetManager;
import com.raytheon.viz.gfe.core.msgs.IDisplayedParmListChangedListener;
import com.raytheon.viz.gfe.core.msgs.IGlobalSelectionTRChangedListener;
import com.raytheon.viz.gfe.core.msgs.IGridVisibilityChangedListener;
import com.raytheon.viz.gfe.core.msgs.ISampleSetChangedListener;
import com.raytheon.viz.gfe.core.msgs.ISpatialEditorTimeChangedListener;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.perspective.GFEPerspective;
import com.raytheon.viz.gfe.rsc.GFEReferenceSetResource;
import com.raytheon.viz.gfe.rsc.GFEResource;
import com.raytheon.viz.gfe.rsc.GFESystemResource;
import com.raytheon.viz.gfe.rsc.colorbar.GFEColorbarResource;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Provides utilities to populate and depopulate the map with GFE resources from
 * ParmManager
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/10/2008              chammack    Initial Creation.
 * 07/03/2008       #1160  randerso    Added makeVisible method
 * 10/06/2008   1433       chammack    Removed log listener
 * 12/02/2008   1450       randerso    Moved getEditors method into UiUtil for general use
 * 04/09/2009   1288       rjpeter     Add sample set listener,ensure remove called for listeners
 * 08/20/2009   2310       njensen   Separated most logic out into AbstractSpatialDisplayManager
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class GFESpatialDisplayManager extends AbstractSpatialDisplayManager
        implements IDisplayedParmListChangedListener {

    private final IWorkbenchWindow window;

    private TimeRange globalTimeRange;

    private static final String GFE_PERSPECTIVE = GFEPerspective.ID_PERSPECTIVE;

    private final ISampleSetChangedListener sampleSetListener = new ISampleSetChangedListener() {
    	@Override
        public void sampleSetChanged(ISampleSetManager sampleSetMgr) {
            GFESpatialDisplayManager.this.refresh();
        }
    };

    private boolean isPopulated;

    public final static String GFE_STATUS_GROUP = "GFE_STATUS_GROUP";

    private static final String WXD_SETTING = "WxDiscrete_Description";

    public GFESpatialDisplayManager(IWorkbenchWindow window, DataManager mgr) {
        super(mgr);
        this.window = window;
        this.globalTimeRange = new TimeRange(0, 0);
        PythonPreferenceStore prefs = Activator.getDefault()
                .getPreferenceStore();
        boolean wxd_val = true;
        if (prefs.contains(WXD_SETTING)) {
            if (prefs.isBoolean(WXD_SETTING)) {
                wxd_val = prefs.getBoolean(WXD_SETTING);
            } else if (prefs.isInt(WXD_SETTING)) {
                wxd_val = prefs.getInt(WXD_SETTING) != 0;
            } else {
                try {
                    wxd_val = "true".equalsIgnoreCase(prefs
                            .getString(WXD_SETTING));
                } catch (ClassCastException e) {
                    ; // ignore; use default
                }
            }
        }
        setShowDescription(wxd_val);
    }

    @Override
    protected IDescriptor[] getDescriptors() {
        AbstractEditor[] editors = UiUtil.getEditors(window, GFE_PERSPECTIVE);
        IDescriptor[] descriptors = new IDescriptor[editors.length];
        for (int i = 0; i < editors.length; i++) {
            descriptors[i] = editors[i].getActiveDisplayPane().getDescriptor();
        }
        return descriptors;
    }

    /**
     * Populate an editor with its corresponding ParmManager
     * 
     * @param editor
     *            the editor to populate
     * @throws VizException
     */
    public void populate(AbstractEditor editor) throws VizException {
        synchronized (this) {
            if (isPopulated) {
                return;
            }

            IParmManager parmManager = this.dataManager.getParmManager();
            parmManager.addDisplayedParmListChangedListener(this);

            Parm[] parms = parmManager.getDisplayedParms();

            IDisplayPane pane = editor.getActiveDisplayPane();
            MapDescriptor descriptor = (MapDescriptor) pane.getDescriptor();

            GridLocation gloc = parmManager.compositeGridLocation();
            GridGeometry2D gridGeometry = MapUtil.getGridGeometry(gloc);
            Envelope envelope = gridGeometry.getEnvelope();
            double colorBarHeight = GFEColorbarResource.HEIGHT
                    * envelope.getSpan(1) / pane.getBounds().height;
            
            PythonPreferenceStore prefs = Activator.getDefault()
                     .getPreferenceStore();
            
            double expandLeft = 10;
            if (prefs.contains("OfficeDomain_expandLeft")) {
                expandLeft = prefs.getDouble("OfficeDomain_expandLeft");
            }
            double expandRight = 0.1;
            if (prefs.contains("OfficeDomain_expandRight")) {
                expandRight = prefs.getDouble("OfficeDomain_expandRight");
            }
            double expandTop = 0.1;
            if (prefs.contains("OfficeDomain_expandTop")) {
                expandTop = prefs.getDouble("OfficeDomain_expandTop");
            }
            double expandBottom = 0.1;
            if (prefs.contains("OfficeDomain_expandBottom")) {
                expandBottom = prefs.getDouble("OfficeDomain_expandBottom");
            }
            
            double dxLeft = (envelope.getSpan(0) * expandLeft / 100.0);
            double dxRight = (envelope.getSpan(0) * expandRight / 100.0);
            double dyTop = (envelope.getSpan(1) * expandTop / 100.0);
            double dyBottom = (envelope.getSpan(1) * expandBottom / 100.0);

            GeneralEnvelope newEnvelope = new GeneralEnvelope(
                    envelope.getCoordinateReferenceSystem());
            newEnvelope.setRange(0, envelope.getMinimum(0) - dxLeft,
                    envelope.getMaximum(0) + dxRight);
            newEnvelope.setRange(1, envelope.getMinimum(1) - dyBottom,
            		envelope.getMaximum(1) + colorBarHeight + dyTop);
            GridGeometry2D newGridGeometry = new GridGeometry2D(
                    gridGeometry.getGridRange(), newEnvelope);
            descriptor.setGridGeometry(newGridGeometry);
            pane.getRenderableDisplay().scaleToClientArea(pane.getBounds());

            // Stop any looping
            editor.getLoopProperties().setLooping(false);

            // Construct a new gfe time match resource which is used to help
            // establish gfe time matching
            GFESystemResource basis = new GFESystemResource(this.dataManager);
            basis.setDescriptor(descriptor);
            descriptor.getResourceList().add(basis);
            descriptor.getResourceList().getProperties(basis)
                    .setSystemResource(true);
            descriptor.setNumberOfFrames(Integer.MAX_VALUE);

            // create the gfe color bar
            GFEColorbarResource colorBar = new GFEColorbarResource(
                    this.dataManager);
            colorBar.setDescriptor(descriptor);
            descriptor.getResourceList().add(colorBar);
            descriptor.getResourceList().getProperties(colorBar)
                    .setSystemResource(true);

            // create the gfe reference set
            GFEReferenceSetResource refSetRsc = new GFEReferenceSetResource(
                    this.dataManager.getRefManager());
            refSetRsc.setDescriptor(descriptor);
            descriptor.getResourceList().add(refSetRsc);
            descriptor.getResourceList().getProperties(refSetRsc)
                    .setSystemResource(true);

            Arrays.sort(parms);

            for (int i = parms.length - 1; i >= 0; i--) {
                createResourceFromParm(descriptor, parms[i], false);
            }

            this.dataManager.getSampleSetManager().addSampleSetChangedListener(
                    sampleSetListener);

            this.isPopulated = true;
        }

    }

    /**
     * Create a resource from a parm, and add it to the map
     * 
     * @param descriptor
     *            the descriptor to use
     * @param p
     *            the parm to use
     * @param visibility
     *            whether the parm should be currently displaying
     */
    private void createResourceFromParm(IDescriptor descriptor, Parm p,
            boolean visibility) {

        // Make sure the resource doesn't already exist
        for (ResourcePair rp : descriptor.getResourceList()) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (rsc instanceof GFEResource) {
                Parm rscParm = ((GFEResource) rsc).getParm();

                if (rscParm.getParmID().equals(p.getParmID())) {
                    return;
                }

            }
        }

        RGB color = p.getDisplayAttributes().getBaseColor();
        if (color == null) {
            color = ColorUtil.getNewColor(descriptor);
            p.getDisplayAttributes().setBaseColor(color);
        }
        GFEResource rsc = new GFEResource(p, this.dataManager);
        rsc.getCapability(ColorableCapability.class).setColor(color);

        ResourceProperties rp = new ResourceProperties();
        rp.setVisible(visibility);

        ResourcePair pair = new ResourcePair();
        pair.setProperties(rp);
        pair.setResource(rsc);

        descriptor.getResourceList().add(pair);

    }

    /**
     * Delete a resource from the map that displays a specific parm
     * 
     * @param descriptor
     *            the descriptor
     * @param p
     *            the parm to use
     */
    protected void deleteResourceFromParm(IDescriptor descriptor, Parm p) {
        for (ResourcePair rp : descriptor.getResourceList()) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (rsc instanceof GFEResource) {
                Parm rscParm = ((GFEResource) rsc).getParm();
                if (rscParm == p) {
                    descriptor.getResourceList().remove(rp);
                }
            }
        }
    }

    /**
     * Depopulate the editor's GFE resources
     * 
     * @throws VizException
     */
    public void depopulate() throws VizException {
        synchronized (this) {
            this.dataManager.getSampleSetManager()
                    .removeSampleSetChangedListener(sampleSetListener);
            this.dataManager.getParmManager()
                    .removeDisplayedParmListChangedListener(this);
            this.isPopulated = false;
        }

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
        super.setSpatialEditorTime(date);

        for (ISpatialEditorTimeChangedListener listener : spatialEditorTimeChangedListeners) {
            listener.spatialEditorTimeChanged(date);
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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ISpatialDisplayManager#refresh()
     */
    private void refresh() {
        for (AbstractEditor editor : UiUtil.getEditors(window, GFE_PERSPECTIVE)) {
            editor.refresh();
        }
    }

    @Override
    public void displayedParmListChanged(Parm[] parms, Parm[] deletions,
            Parm[] additions) {

        AbstractEditor[] editors = UiUtil.getEditors(window, GFE_PERSPECTIVE);
        for (AbstractEditor e : editors) {
            IRenderableDisplay disp = e.getActiveDisplayPane()
                    .getRenderableDisplay();
            IDescriptor descriptor = disp.getDescriptor();

            // put parms to delete in set for easy look up
            Set<Parm> parmsToDelete = new HashSet<Parm>();
            for (Parm p : deletions) {
                parmsToDelete.add(p);
            }

            for (ResourcePair rp : descriptor.getResourceList()) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (rsc instanceof GFEResource) {
                    GFEResource gfeRsc = (GFEResource) rsc;

                    if (parmsToDelete.contains(gfeRsc.getParm())) {
                        descriptor.getResourceList().remove(rp);
                    }
                }
            }

            for (Parm addParm : additions) {
                createResourceFromParm(descriptor, addParm, false);
            }
        }
    }

    @Override
    public void toggleVisibility(Parm parm) {
        AbstractEditor[] editors = UiUtil.getEditors(window, GFE_PERSPECTIVE);
        if (editors.length == 0) {
            return;
        }

        for (AbstractEditor editor : editors) {
            IDescriptor descriptor = editor.getActiveDisplayPane()
                    .getDescriptor();
            boolean needRefresh = false;

            ResourceList resourceList = descriptor.getResourceList();
            synchronized (resourceList) {
                Iterator<ResourcePair> iterator = resourceList.iterator();

                while (iterator.hasNext()) {
                    ResourcePair resourcePair = iterator.next();
                    AbstractVizResource<?, ?> rsc = resourcePair.getResource();
                    if (rsc instanceof GFEResource) {
                        Parm p = ((GFEResource) rsc).getParm();
                        if (p.equals(parm)) {
                            ResourceProperties props = resourcePair
                                    .getProperties();
                            boolean visible = !props.isVisible();
                            props.setVisible(visible);
                            needRefresh = true;

                            for (IGridVisibilityChangedListener listener : gridVisibilityChangedListeners) {
                                listener.gridVisibilityChanged(p, visible,
                                        false);
                            }
                        }
                    }
                }
            }

            if (needRefresh) {
                editor.refresh();
            }
        }
    }

}
