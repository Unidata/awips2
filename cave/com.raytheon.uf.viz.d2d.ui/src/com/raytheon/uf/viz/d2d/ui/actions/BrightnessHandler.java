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
package com.raytheon.uf.viz.d2d.ui.actions;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * Handler that increases or decreases brightness of imagery resources. We do
 * not use a parameter on the event for the step value because that makes the
 * command look weird with an extra () in the Eclipse key bindings preferences
 * page.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 19, 2018 6908       njensen     Initial creation
 *
 * </pre>
 *
 * @author njensen
 */

public abstract class BrightnessHandler extends AbstractTool {

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        super.execute(event);

        for (IDisplayPane pane : editor.getDisplayPanes()) {
            IDescriptor descriptor = pane.getDescriptor();
            ResourceList rscs = descriptor.getResourceList();
            for (ResourcePair rp : rscs) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (!rsc.hasCapability(ImagingCapability.class)) {
                    continue;
                }
                ImagingCapability cap = rsc
                        .getCapability(ImagingCapability.class);
                float bright = cap.getBrightness();
                bright += getStep();
                if (bright <= 0.00f) {
                    bright = 0.00f;
                } else if (bright >= 1.00f) {
                    bright = 1.00f;
                }
                cap.setBrightness(bright, true);
            }
        }

        return null;
    }

    /**
     * Gets the amount the brightness should be changed. This should be a value
     * between 0 and 1 in hundredths. For example, 0.25 would translate to 25%
     * increment.
     * 
     * @return
     */
    protected abstract float getStep();

}
