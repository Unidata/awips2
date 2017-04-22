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
package com.raytheon.uf.viz.daylight.transition.resource;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.viz.core.rsc.BlendedResource;

/**
 * 
 * Provides the ability to blend two satellite resources with one of them
 * becoming transparent when it is night.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 28, 2015  4633     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class DaylightTransitionBlendedResource extends BlendedResource {

    private DaylightTransitionBlendedResourceData resourceData;

    public DaylightTransitionBlendedResource(
            DaylightTransitionBlendedResourceData data, LoadProperties props) {
        super(data, props);
        this.resourceData = data;
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        BlendableCapability blendable = getCapability(BlendableCapability.class);
        blendable.toggle(resourceData.getTransitionIndex());
        super.paintInternal(target, paintProps);
        blendable.toggle(1 - resourceData.getTransitionIndex());
        super.paintInternal(target, paintProps);
        getCapability(BlendableCapability.class).setAlphaStep(
                BlendableCapability.BLEND_MAX / 2);
        for (ResourcePair pair : getResourceList()) {
            pair.getResource().getCapability(ImagingCapability.class)
                    .setAlpha(1.0f);
        }

    }

    @Override
    public DaylightTransitionBlendedResourceData getResourceData() {
        return resourceData;
    }

}
