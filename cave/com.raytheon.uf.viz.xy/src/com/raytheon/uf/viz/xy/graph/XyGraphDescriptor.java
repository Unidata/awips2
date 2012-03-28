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
package com.raytheon.uf.viz.xy.graph;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import org.geotools.referencing.crs.DefaultEngineeringCRS;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.uf.viz.xy.map.rsc.GraphResource;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The AbstractDescriptor for Graphs, contains a graph resource which most graph
 * specific code gets delegated to
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 29, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class XyGraphDescriptor extends AbstractDescriptor implements
        RemoveListener, ISerializableObject {

    protected int verticalFrameCount = 0;

    protected double zoomLevel = 1.0f;

    public XyGraphDescriptor() {
        this(new PixelExtent(0, 1000, 0, 1000));
    }

    public XyGraphDescriptor(PixelExtent anExtent) {
        super(createGridGeometry(anExtent, DefaultEngineeringCRS.CARTESIAN_2D));
    }

    public IGraph getGraph(IGraphableResource<?, ?> rsc) {
        GraphResource gr = getGraphResource();
        if (rsc != null && gr != null) {
            return gr.getGraph(rsc);
        }
        return null;
    }

    public GraphResource getGraphResource() {
        List<GraphResource> rscs = resourceList
                .getResourcesByTypeAsType(GraphResource.class);
        if (rscs.size() > 0) {
            return rscs.get(0);
        }
        return null;
    }

    public IGraph constructGraph() {
        return null;
    }

    @Override
    public void notifyRemove(ResourcePair rp) throws VizException {
        GraphResource graphResource = getGraphResource();
        if (graphResource != null
                && rp.getResource() instanceof IGraphableResource<?, ?>) {
            graphResource.removeFromGraph((IGraphableResource<?, ?>) rp
                    .getResource());
        }
    }

    public Coordinate getGraphCoordiante(IGraphableResource<?, ?> rsc,
            Coordinate c) {
        IGraph graph = getGraphResource().getClosestGraph(c);
        if (graph != null && getGraph(rsc) == graph) {
            if (graph.getExtent().contains(new double[] { c.x, c.y })) {
                double[] values = graph.getVirtualLocation(c.x, c.y);
                return new Coordinate(values[0], values[1]);
            }
        }
        return null;
    }

}
