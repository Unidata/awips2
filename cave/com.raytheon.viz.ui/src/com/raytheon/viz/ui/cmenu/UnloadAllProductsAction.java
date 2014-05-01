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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;

/**
 * 
 * Unload all products
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jan 3, 2008              chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class UnloadAllProductsAction extends AbstractRightClickAction {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        IDisplayPane[] panes = getContainer().getDisplayPanes();

        for (IDisplayPane displayPane : panes) {
            IDescriptor desc = displayPane.getDescriptor();
            ResourceList rl = desc.getResourceList();

            Iterator<ResourcePair> iterator = rl.iterator();
            List<AbstractVizResource<?, ?>> rscsToRemove = new ArrayList<AbstractVizResource<?, ?>>();

            while (iterator.hasNext()) {
                ResourcePair rp = iterator.next();
                if (!rp.getProperties().isMapLayer()
                        && !rp.getProperties().isSystemResource()) {
                    rscsToRemove.add(rp.getResource());
                }
            }

            for (AbstractVizResource<?, ?> rsc : rscsToRemove) {
                rl.removeRsc(rsc);
            }
        }
        this.container.refresh();

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return "Unload All Products";
    }

}
