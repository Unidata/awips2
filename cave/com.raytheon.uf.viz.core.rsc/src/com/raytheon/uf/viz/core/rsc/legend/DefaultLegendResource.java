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
package com.raytheon.uf.viz.core.rsc.legend;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendedCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.viz.ui.input.EditableManager;
import com.raytheon.viz.ui.input.preferences.MousePreferenceManager;

/**
 * Default legend resource, handles visibility toggling and editableness
 * toggling. TODO: Make D2DLegendResource extend it
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 17, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DefaultLegendResource extends
        AbstractLegendResource<GenericResourceData> {

    private static final String HIDE_RESOURCE_PREF = "com.raytheon.viz.d2d.ui.hide.resource";

    private static final String EDIT_RESOURCE_PREF = "com.raytheon.viz.ui.input.resource.edit";

    private MousePreferenceManager prefManager = MousePreferenceManager
            .getInstance();

    /**
     * @param resourceData
     * @param loadProperties
     */
    public DefaultLegendResource(GenericResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.legend.ILegendDecorator#getLegendData(com.raytheon
     * .uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public LegendEntry[] getLegendData(IDescriptor descriptor) {

        List<LegendData> labels = new ArrayList<LegendData>();
        ResourceList resourceList = descriptor.getResourceList();
        if (resourceList != null) {
            for (int i = 0; i < resourceList.size(); i++) {
                ResourcePair resourcePair = resourceList.get(i);
                // See if resource is a system resource (does not
                // participate in legend)
                boolean system = resourcePair.getProperties()
                        .isSystemResource();
                // See if resource is visible
                boolean vis = resourcePair.getProperties().isVisible();
                AbstractVizResource<?, ?> rsc = resourcePair.getResource();
                if (system) {
                    continue;
                } else {
                    LegendData legend = new LegendData();
                    if (rsc == null) {
                        continue;
                    } else if (rsc.getStatus() != ResourceStatus.INITIALIZED) {
                        continue;
                    } else {
                        legend.label = rsc.getName();
                        legend.resource = resourcePair;

                        if (rsc.hasCapability(EditableCapability.class)
                                && rsc.getCapability(EditableCapability.class)
                                        .isEditable()) {
                            legend.label += " (Editable)";
                        }
                    }

                    if (!vis) {
                        legend.color = new RGB(50, 50, 50);
                    } else {
                        legend.color = rsc.getCapability(
                                ColorableCapability.class).getColor();
                    }
                    labels.add(legend);
                }

            }
        }

        LegendEntry[] entries = new LegendEntry[labels.size()];
        for (int i = 0; i < entries.length; ++i) {
            entries[i] = new LegendEntry();
            entries[i].legendParts = new LegendData[] { labels.get(i) };
        }
        return entries;
    }

    @Override
    protected boolean checkResourceClick(ResourcePair mouseDownRsc,
            int mouseButton) {
        return true;
    }

    @Override
    protected void resourceClicked(ResourcePair resource, int mouseButton) {
        if (prefManager.handleClick(EDIT_RESOURCE_PREF, mouseButton)
                && resource.getResource().hasCapability(
                        EditableCapability.class)) {
            // Editable case
            toggleEditability(resource);
        } else {
            // Visibility case
            toggleVisibility(resource);
        }
        issueRefresh();
    }

    /**
     * Toggle visibility of resource taking blended/blendable resources into
     * account.
     * 
     * If resource to toggle is blended, 1st check to see if parent resource is
     * visible. If not visible then make parent and all children visible.
     * 
     * @param rp
     */
    protected boolean toggleVisibility(ResourcePair rp) {
        AbstractVizResource<?, ?> rsc = rp.getResource();
        if (rsc != null) {
            if (rsc.hasCapability(BlendedCapability.class)) {
                ResourcePair parentRsc = rsc.getCapability(
                        BlendedCapability.class).getBlendableResource();
                ResourceList children = parentRsc.getResource()
                        .getCapability(BlendableCapability.class)
                        .getResourceList();
                if (parentRsc.getProperties().isVisible() == false) {
                    parentRsc.getProperties().setVisible(true);
                    for (ResourcePair child : children) {
                        child.getProperties().setVisible(true);
                    }
                } else {
                    // topmost resource is visible, toggle us and other
                    // rsc
                    if (rp.getProperties().isVisible() == false) {
                        rp.getProperties().setVisible(true);
                        parentRsc
                                .getResource()
                                .getCapability(BlendableCapability.class)
                                .setAlphaStep(BlendableCapability.BLEND_MAX / 2);
                    } else {
                        parentRsc.getResource()
                                .getCapability(BlendableCapability.class)
                                .toggle(rp);
                    }
                }
                return rp.getProperties().isVisible();
            }
        }
        rp.getProperties().setVisible(!rp.getProperties().isVisible());
        return rp.getProperties().isVisible();
    }

    /**
     * Toggles the editable flag on the resource
     * 
     * @param rp
     * @return
     */
    protected boolean toggleEditability(ResourcePair rp) {
        EditableManager.makeEditable(rp.getResource(), !rp.getResource()
                .getCapability(EditableCapability.class).isEditable());
        return rp.getResource().getCapability(EditableCapability.class)
                .isEditable();
    }
}
