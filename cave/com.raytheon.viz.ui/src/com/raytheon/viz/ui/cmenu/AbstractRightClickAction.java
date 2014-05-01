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
package com.raytheon.viz.ui.cmenu;

import org.eclipse.jface.action.Action;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendedCapability;

/**
 * Base class for right click actions
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Nov 13, 2006             chammack    Initial Creation.
 * Dec 28, 2007             chammack    Changed class to use IDisplayPaneContainer
 *                                      rather than AbstractEditor
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public abstract class AbstractRightClickAction extends Action {

    private static int HASH_CODE;

    private int hashCode;

    protected ResourcePair selectedRsc;

    protected int order;

    protected IDisplayPaneContainer container;

    public AbstractRightClickAction() {
        super();
        hashCode = HASH_CODE;
        HASH_CODE++;

    }

    public boolean isHidden() {
        return false;
    }

    public AbstractRightClickAction(String name) {
        super(name);
        hashCode = HASH_CODE;
        HASH_CODE++;
    }

    public AbstractRightClickAction(int style) {
        super("RightClickAction", style);
        hashCode = HASH_CODE;
        HASH_CODE++;
    }

    public AbstractRightClickAction(String name, int style) {
        super(name, style);
        hashCode = HASH_CODE;
        HASH_CODE++;
    }

    /**
     * @return the selectedRsc
     */
    public AbstractVizResource<?, ?> getSelectedRsc() {
        return selectedRsc != null ? selectedRsc.getResource() : null;
    }

    /**
     * 
     * @return the selected resources ResourceProperties
     */
    public ResourceProperties getSelectedProperties() {
        return selectedRsc != null ? selectedRsc.getProperties() : null;
    }

    /**
     * Returns the parent resource if has BlendedCapability
     * 
     * @return
     */
    public AbstractVizResource<?, ?> getTopMostSelectedResource() {
        AbstractVizResource<?, ?> rsc = getSelectedRsc();
        if (rsc != null) {
            if (rsc.hasCapability(BlendedCapability.class)) {
                rsc = rsc.getCapability(BlendedCapability.class)
                        .getBlendableResource().getResource();
            }
        }
        return rsc;
    }

    /**
     * Returns the parent resource if has BlendedCapability
     * 
     * @return
     */
    public ResourcePair getTopMostSelectedResourcePair() {
        ResourcePair pair = selectedRsc;
        AbstractVizResource<?, ?> rsc = pair.getResource();
        if (rsc.hasCapability(BlendedCapability.class)) {
            pair = rsc.getCapability(BlendedCapability.class)
                    .getBlendableResource();
        }
        return pair;
    }

    /**
     * Get the display pane container that the action should work against
     * 
     * @return
     */
    public IDisplayPaneContainer getContainer() {
        return container;
    }

    /**
     * set the display pane container that the action should work against
     * 
     * @param container
     *            the container
     */
    public void setContainer(IDisplayPaneContainer container) {
        this.container = container;
    }

    /**
     * @param selectedRsc
     *            the selectedRsc to set
     */
    public void setSelectedRsc(ResourcePair selectedRsc) {
        this.selectedRsc = selectedRsc;
    }

    /**
     * @return the order
     */
    public int getOrder() {
        return order;
    }

    /**
     * @param order
     *            the order to set
     */
    public void setOrder(int order) {
        this.order = order;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {

        // Return false always to allow sets to contain duplicates
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return hashCode;
    }

    protected IDescriptor getDescriptor() {
        return getContainer().getActiveDisplayPane().getDescriptor();
    }

}
