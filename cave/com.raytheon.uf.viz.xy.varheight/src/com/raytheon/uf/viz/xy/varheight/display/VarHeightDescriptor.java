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
package com.raytheon.uf.viz.xy.varheight.display;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.uf.viz.xy.graph.XyGraphDescriptor;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.uf.viz.xy.varheight.graph.VarHeightGraph;
import com.raytheon.uf.viz.xy.varheight.rsc.VarHeightResource;
import com.raytheon.viz.core.slice.request.HeightScale;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 28, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class VarHeightDescriptor extends XyGraphDescriptor {

    @XmlElement
    private HeightScale heightScale;

    public VarHeightDescriptor() {
        super();
    }

    /**
     * @param pixelExtent
     * @param varHeightGraphFactory
     */
    public VarHeightDescriptor(PixelExtent pixelExtent) {
        super(pixelExtent);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.graph.XyGraphDescriptor#constructGraph()
     */
    @Override
    public IGraph constructGraph() {
        return new VarHeightGraph(this);
    }

    /**
     * @return the heightScale
     */
    public HeightScale getHeightScale() {
        return heightScale;
    }

    /**
     * @param heightScale
     *            the heightScale to set
     */
    public void setHeightScale(HeightScale heightScale) {
        if (heightScale != this.heightScale) {
            this.heightScale = heightScale;
            for (ResourcePair rp : this.resourceList) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (rsc instanceof IGraphableResource<?, ?>) {
                    this.getGraph((IGraphableResource<?, ?>) rsc).reconstruct();
                    if (rsc instanceof VarHeightResource) {
                        ((VarHeightResource) rsc).setDescriptor(this);
                    }
                }
            }
            if (renderableDisplay != null) {
                ((VarHeightRenderableDisplay) renderableDisplay)
                        .setTabTitle(String.format("Var vs height : "
                                + heightScale.getName()));
            }
        }
    }

    @Override
    public boolean isCompatible(IDescriptor other) {
        if (other instanceof VarHeightDescriptor) {
            VarHeightDescriptor vhOther = (VarHeightDescriptor) other;
            return vhOther.heightScale.equals(this.heightScale);
        }
        return false;
    }

}
