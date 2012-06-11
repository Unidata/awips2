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
package com.raytheon.uf.viz.xy.timeheight.display;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.uf.viz.xy.graph.XyGraphDescriptor;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.uf.viz.xy.timeheight.graph.TimeHeightGraph;
import com.raytheon.uf.viz.xy.timeheight.rsc.AbstractTimeHeightResource;
import com.raytheon.viz.core.slice.request.HeightScale;
import com.raytheon.viz.core.slice.request.VerticalPointRequest.TimeDirection;

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
public class TimeHeightDescriptor extends XyGraphDescriptor {

    public static final int REAL_FRAME_COUNT_TO_USE_WHEN_FRAME_COUNT_IS_ONE = 999;

    @XmlAttribute
    public TimeDirection timeDirection;

    @XmlElement
    private HeightScale heightScale;

    public TimeHeightDescriptor() {
        super();
    }

    /**
     * @param pixelExtent
     * @param varHeightGraphFactory
     */
    public TimeHeightDescriptor(PixelExtent pixelExtent) {
        super(pixelExtent);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.graph.XyGraphDescriptor#constructGraph()
     */
    @Override
    public IGraph constructGraph() {
        return new TimeHeightGraph(this);
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
                    if (rsc instanceof AbstractTimeHeightResource) {
                        ((AbstractTimeHeightResource) rsc).setDescriptor(this);
                    }
                }
            }
            if (renderableDisplay != null) {
                ((TimeHeightRenderableDisplay) renderableDisplay)
                        .setTabTitle(String.format("Time Height : "
                                + heightScale.getName()));
            }
        }
    }

    /**
     * @return the timeDirection
     */
    public TimeDirection getTimeDirection() {
        return timeDirection;
    }

    /**
     * @param timeDirection
     *            the timeDirection to set
     */
    public void setTimeDirection(TimeDirection timeDirection) {
        this.timeDirection = timeDirection;
    }

    @Override
    public boolean isCompatible(IDescriptor other) {
        if (other instanceof TimeHeightDescriptor) {
            TimeHeightDescriptor thOther = (TimeHeightDescriptor) other;
            return thOther.heightScale.equals(this.heightScale);
        }
        return false;
    }

    @Override
    public int getNumberOfFrames() {
        int numFrames = super.getNumberOfFrames();
        if (numFrames == 1) {
            // reset to a different number because A1 did
            numFrames = Math.min(
                    REAL_FRAME_COUNT_TO_USE_WHEN_FRAME_COUNT_IS_ONE,
                    limitedNumberOfFrames);
        }
        return numFrames;
    }

}
