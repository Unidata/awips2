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

import java.util.List;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.d2d.core.ImageCombiner;
import com.raytheon.viz.ui.cmenu.LoadAsImageAction;

/**
 * 
 * Extension to the {@link LoadAsImageAction} that enables the
 * {@link ImageCombiner} to combine the newly created image.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Mar 14, 2018  6700     bsteffen  Initial creation
 * 
 * </pre>
 *
 * @author bsteffen
 */
public class LoadAsImageAndCombine extends LoadAsImageAction {

    @Override
    public String getText() {
        return super.getText() + " and combine";
    }

    @Override
    protected void performLoad(IDescriptor descriptor,
            ResourcePair selectedRsc) {
        ImageCombiner.setCombineImages(true);
        super.performLoad(descriptor, selectedRsc);
        ImageCombiner.setCombineImages(false);
    }

    @Override
    public boolean isHidden() {
        if (ImageCombiner.isCombineImages()) {
            return true;
        }
        if (!super.isHidden()) {
            AbstractVizResource<?, ?> rsc = getSelectedRsc();
            if (rsc != null) {
                List<AbstractVizResource<?, ?>> imageResources = rsc
                        .getDescriptor().getResourceList()
                        .getResourcesByCapability(ImagingCapability.class);
                return imageResources.size() != 1;
            }
        }
        return true;
    }

}
