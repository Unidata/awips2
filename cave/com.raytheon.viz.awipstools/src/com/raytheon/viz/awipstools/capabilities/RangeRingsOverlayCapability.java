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
package com.raytheon.viz.awipstools.capabilities;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.awipstools.capabilityInterfaces.IRangeableResource;
import com.raytheon.viz.awipstools.ui.layer.RangeRingsOverlayLayer;
import com.raytheon.viz.core.rsc.BlendedResource;

/**
 * A resource with this capability can have a range rings layer overlayed on it.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  12-03-09     #3698      bgonzale    Range rings overlay with labels 
 *                                      for radius in km and miles and
 *                                      labels for elevation in ftMSL and
 *                                      kmAgl.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class RangeRingsOverlayCapability extends AbstractCapability implements
        IRenderable {

    private RangeRingsOverlayLayer rangeOverlayLayer;

    private boolean isLayerPainted;

    /**
     * @return true if the overlay layer is painted; false otherwise.
     */
    public boolean isOverlayLayerPainted() {
        return isLayerPainted;
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        if (isLayerPainted && rangeOverlayLayer != null) {
            rangeOverlayLayer.paint(target, paintProps);
        }
    }
    
    public void setRangeableResource(IRangeableResource resource) throws VizException {
        if (this.rangeOverlayLayer != null) {
            this.rangeOverlayLayer.setRangedResource(resource);
        }
    }

    public void updatePaintStatus(AbstractVizResource<?, ?> resource,
            ColorableCapability colorableCapability,
            OutlineCapability outlineCapability) throws VizException {
        if (this.rangeOverlayLayer == null) {
            IMapDescriptor mDesc = (IMapDescriptor) resource.getDescriptor();

            this.rangeOverlayLayer = new RangeRingsOverlayLayer(mDesc,
                    colorableCapability, outlineCapability);
            if (resource instanceof IRangeableResource) {
                this.rangeOverlayLayer
                        .setRangedResource((IRangeableResource) resource);
            } else if (resource instanceof BlendedResource) {
                ResourceList rl = ((BlendedResource) resource)
                        .getResourceList();
                for (int i = 0; i < rl.size(); ++i) {
                    ResourcePair rp = rl.get(i);
                    if (rp.getResource() instanceof IRangeableResource) {
                        this.rangeOverlayLayer
                                .setRangedResource((IRangeableResource) rp
                                        .getResource());
                        break;
                    }
                }
            }
        }

        if (isLayerPainted) {
            isLayerPainted = false;
        } else {
            isLayerPainted = true;
        }
    }

    @Override
    public AbstractCapability clone() {
        RangeRingsOverlayCapability rroc = new RangeRingsOverlayCapability();
        rroc.rangeOverlayLayer = rangeOverlayLayer;
        rroc.isLayerPainted = isLayerPainted;
        return rroc;
    }
}
