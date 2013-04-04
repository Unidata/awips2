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
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.opengis.geometry.Envelope;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.IFrameChangedListener;
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
import com.raytheon.viz.gfe.core.msgs.ISampleSetChangedListener;
import com.raytheon.viz.gfe.core.msgs.ISpatialEditorTimeChangedListener;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.rsc.GFEReferenceSetResource;
import com.raytheon.viz.gfe.rsc.GFEResource;
import com.raytheon.viz.gfe.rsc.GFESystemResource;
import com.raytheon.viz.gfe.rsc.colorbar.GFEColorbarResource;
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
        implements IDisplayedParmListChangedListener, IFrameChangedListener {

    private final ISampleSetChangedListener sampleSetListener = new ISampleSetChangedListener() {
        @Override
        public void sampleSetChanged(ISampleSetManager sampleSetMgr) {
            GFESpatialDisplayManager.this.refresh();
        }
    };

    private boolean isRegistered;

    public final static String GFE_STATUS_GROUP = "GFE_STATUS_GROUP";

    private static final String WXD_SETTING = "WxDiscrete_Description";

    /**
     * Editors managed by this display manager, use
     * {@link #populate(AbstractEditor)} to add editor
     */
    private List<AbstractEditor> managedEditors = new ArrayList<AbstractEditor>();

    public GFESpatialDisplayManager(DataManager mgr) {
        super(mgr);
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
        List<IDescriptor> descriptors = new ArrayList<IDescriptor>();
        for (AbstractEditor editor : managedEditors) {
            for (IDisplayPane pane : editor.getDisplayPanes()) {
                descriptors.add(pane.getDescriptor());
            }
        }
        return descriptors.toArray(new IDescriptor[descriptors.size()]);
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
            if (isRegistered == false) {
                // First time called, register listeners
                this.dataManager.getParmManager()
                        .addDisplayedParmListChangedListener(this);
                this.dataManager.getSampleSetManager()
                        .addSampleSetChangedListener(sampleSetListener);
                isRegistered = true;
            }

            if (managedEditors.contains(editor)) {
                // Editor already managed
                return;
            }

            IParmManager parmManager = this.dataManager.getParmManager();
            Parm[] parms = parmManager.getDisplayedParms();
            Arrays.sort(parms);

            for (IDisplayPane pane : editor.getDisplayPanes()) {
                MapDescriptor descriptor = (MapDescriptor) pane.getDescriptor();
                descriptor.addFrameChangedListener(this);

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
                GFESystemResource basis = new GFESystemResource(
                        this.dataManager);
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

                for (int i = parms.length - 1; i >= 0; i--) {
                    createResourceFromParm(descriptor, parms[i], false);
                }

                setSpatialEditorTime(descriptor, getSpatialEditorTime());
            }

            managedEditors.add(editor);
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
    public void dispose() throws VizException {
        synchronized (this) {
            if (isRegistered) {
                this.dataManager.getSampleSetManager()
                        .removeSampleSetChangedListener(sampleSetListener);
                this.dataManager.getParmManager()
                        .removeDisplayedParmListChangedListener(this);
                isRegistered = false;
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ISpatialDisplayManager#refresh()
     */
    private void refresh() {
        for (IDescriptor desc : getDescriptors()) {
            if (desc.getRenderableDisplay() != null) {
                desc.getRenderableDisplay().refresh();
            }
        }
    }

    @Override
    public void displayedParmListChanged(Parm[] parms, Parm[] deletions,
            Parm[] additions) {
        Set<Parm> toDelete = new HashSet<Parm>(Arrays.asList(deletions));
        for (IDescriptor desc : getDescriptors()) {
            ResourceList list = desc.getResourceList();
            List<GFEResource> rscs = list
                    .getResourcesByTypeAsType(GFEResource.class);
            for (GFEResource rsc : rscs) {
                if (toDelete.contains(rsc.getParm())) {
                    list.removeRsc(rsc);
                }
            }

            for (Parm addParm : additions) {
                createResourceFromParm(desc, addParm, false);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IDescriptor.IFrameChangedListener#
     * frameChanged(com.raytheon.uf.viz.core.drawables.IDescriptor,
     * com.raytheon.uf.common.time.DataTime,
     * com.raytheon.uf.common.time.DataTime)
     */
    @Override
    public void frameChanged(IDescriptor descriptor, DataTime oldTime,
            DataTime newTime) {
        if (newTime != null && oldTime != null && seTime != null
                && oldTime.getRefTime().equals(seTime)) {
            // time was set to seTime, frame changed, set seTime to null
            setSpatialEditorTime(null);
        }
        for (ISpatialEditorTimeChangedListener subListener : spatialEditorTimeChangedListeners) {
            subListener.spatialEditorTimeChanged(newTime != null ? newTime
                    .getRefTime() : null);
        }
    }

}
