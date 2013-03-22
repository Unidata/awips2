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
package com.raytheon.viz.awipstools.ui.action;

import java.util.Iterator;
import java.util.LinkedList;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.uf.viz.core.rsc.tools.action.AbstractGenericToolAction;
import com.raytheon.viz.awipstools.ui.layer.HomeToolLayer;
import com.raytheon.viz.ui.input.EditableManager;

/**
 * Show 'Home' location. Also shows range and home from home.
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Aug242007    #429        ebabin      Initial Creation.
 *  25Oct2007    #499        ebaibn      Added sampling switch to mouseMove action.
 *  19Dec2007    #647        ebabin      Fix mouse following issue..
 *  10-21-09     #1049       bsteffen    Refactor to common MovableTool model
 * </pre>
 * 
 * @author ebabin
 * @version 1
 */
public class HomeToolAction extends AbstractGenericToolAction<HomeToolLayer> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(HomeToolAction.class);

    private HomeToolLayer resource;

    /**
     * check the descriptor for a reference to the passed in HomeToolLayer
     * 
     * @param resource
     * @return
     */
    private HomeToolLayer checkAndGetResource() {
        LinkedList<HomeToolLayer> resources = new LinkedList<HomeToolLayer>();

        // looking for this exact resource, not one that is equivalent
        for (IDisplayPane pane : getSelectedPanes()) {
            IDescriptor desc = pane.getDescriptor();
            Iterator<ResourcePair> iter = desc.getResourceList().iterator();

            while (iter.hasNext()) {
                ResourcePair pair = iter.next();
                AbstractVizResource<?, ?> rsc = pair.getResource();
                if (rsc instanceof HomeToolLayer) {
                    resources.add((HomeToolLayer) rsc);
                }
            }
        }

        if (resources.size() > 0) {
            if (resources.size() > 1) {
                statusHandler.handle(Priority.EVENTA,
                        "More than one HomeToolLayer found! (found "
                                + resources.size() + ")");
            }
            return resources.getFirst();
        } else {
            return null;
        }
    }

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        boolean turnOnEditable = false;

        // if the descriptor has the saved resource then enable editable
        if ((resource = checkAndGetResource()) != null) {
            turnOnEditable = true;
        }

        Object rval = super.execute(arg0);

        // make resource editable if needed
        if (turnOnEditable && resource != null) {
            EditableManager.makeEditable(resource, true);
        }

        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.ui.action.MapToolAction#getResourceData()
     */
    @Override
    protected GenericToolsResourceData<HomeToolLayer> getResourceData() {
        return new GenericToolsResourceData<HomeToolLayer>(
                HomeToolLayer.DEFAULT_NAME, HomeToolLayer.class);

    }

    @Override
    protected HomeToolLayer getResource(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        if (resource == null || data == null) {
            // if the resource or resourcedata is null then it needs to be
            // rebuilt
            resource = super.getResource(loadProperties, descriptor);
        }
        return resource;
    }
}
