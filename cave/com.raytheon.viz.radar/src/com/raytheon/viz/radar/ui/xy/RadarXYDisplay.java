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
package com.raytheon.viz.radar.ui.xy;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.swt.layout.FormData;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.d2d.ui.AbstractNonMapDisplay;
import com.raytheon.viz.core.graphing.GraphProperties;

/**
 * Display used for Radar products that need to be displayed on an XY editor
 * without a graph
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------
 * Mar 19, 2009           askripsk  Initial creation
 * Feb 07, 2018  6845     bsteffen  Do not paint invisible resources.
 * 
 * </pre>
 * 
 * @author askripsk
 */
@XmlRootElement
public class RadarXYDisplay extends AbstractNonMapDisplay {

    @XmlAttribute
    private boolean insetMap = false;

    public RadarXYDisplay() {
        super(new PixelExtent(0, 1000, 0, 1000), new RadarXYDescriptor());
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        target.setBackgroundColor(backgroundColor);
        LoopProperties lp = paintProps.getLoopProperties();
        if (lp == null) {
            lp = new LoopProperties();
        }

        GraphProperties graphProps = new GraphProperties(paintProps);

        // Plot the resource data on the graph
        for (ResourcePair rp : getDescriptor().getResourceList()) {
            AbstractVizResource<?, ?> resource = rp.getResource();
            if (resource != null && rp.getProperties().isVisible()) {
                graphProps = (GraphProperties) calcPaintDataTime(graphProps,
                        resource);
                resource.paint(target, graphProps);
            }
        }
    }

    @Override
    public FormData getInsetMapLocation() {
        return insetMap ? super.getInsetMapLocation() : null;
    }

}
