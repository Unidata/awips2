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
package com.raytheon.uf.viz.xy.map.rsc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.xy.graph.GraphProperties;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.uf.viz.xy.graph.XyGraphDescriptor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The graph resource is a resource that contains 1-N graphs, lays them out and
 * provides functionality for drawing to them / sampling them
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

public class GraphResource extends
        AbstractVizResource<GraphResourceData, XyGraphDescriptor> {

    /** The distance between graphs */
    private static final int GRAPH_DISTANCE = 100;

    /** The displayed graphs */
    protected List<IGraph> graphs;

    protected boolean newGraphs = false;

    protected IExtent totalExtent;

    /**
     * Map used to map IGraphableResources to a specific graph as multiple
     * resources can be on the same graph
     */
    protected Map<Object, IGraph> graphMap;

    protected GraphResource(GraphResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        graphs = new ArrayList<IGraph>();
        graphMap = new HashMap<Object, IGraph>();
    }

    @Override
    protected void disposeInternal() {
        for (IGraph graph : graphs) {
            graph.dispose();
        }
        graphs.clear();
        graphMap.clear();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        // graphs.add(getDescriptor().constructGraph());
        // newGraphs = true;
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        GraphProperties props = (GraphProperties) paintProps;
        // TODO Layout graphs in horizontal fashion and paint them
        if (graphs.size() == 0) {
            return;
        }

        if (newGraphs) {
            totalExtent = props.getWorldExtent();
            IExtent totalGraphExtent = new PixelExtent(
                    totalExtent.getMinX() + 25, totalExtent.getMaxX() - 1,
                    totalExtent.getMinY() + 25, totalExtent.getMaxY() - 25);
            double minX, maxX, minY, maxY;
            switch (resourceData.getOverlayMode()) {
            case VERTICAL:
                int numGraphs = visibleGraphCount();
                double graphHeight = (totalGraphExtent.getHeight() / numGraphs)
                        - GRAPH_DISTANCE;

                minX = totalGraphExtent.getMinX();
                maxX = totalGraphExtent.getMaxX();
                minY = totalGraphExtent.getMinY();
                maxY = minY + graphHeight;
                for (int i = graphs.size() - 1; i >= 0; --i) {
                    IGraph graph = graphs.get(i);
                    if (graph.isDisplayed()) {
                        graph.updateExtent(new PixelExtent(minX, maxX, minY,
                                maxY));
                        graph.paint(target, paintProps);
                        minY = maxY + GRAPH_DISTANCE;
                        maxY = minY + graphHeight;
                    }
                }
                break;
            case OVERLAY:
            default:
                minX = totalGraphExtent.getMinX();
                maxX = totalGraphExtent.getMaxX();
                minY = totalGraphExtent.getMinY();
                maxY = totalGraphExtent.getMaxY() - GRAPH_DISTANCE;
                for (int i = graphs.size() - 1; i >= 0; --i) {
                    IGraph graph = graphs.get(i);
                    if (graph.isDisplayed()) {
                        graph.updateExtent(new PixelExtent(minX, maxX, minY,
                                maxY));
                        graph.paint(target, paintProps);
                    }
                }
                break;
            }

            newGraphs = false;
        } else {
            for (IGraph graph : graphs) {
                if (graph.isDisplayed() == true) {
                    graph.paint(target, paintProps);
                }
            }
        }
    }

    /**
     * Returns the graph associated with this graphable resource, graph will be
     * constructed if doesn't exist, should only be called once by the resource
     * and cached
     * 
     * @param rsc
     * @return
     */
    public synchronized IGraph getGraph(IGraphableResource<?, ?> rsc) {
        IGraph graph = graphMap.get(rsc.getGraphKey());
        if (graph == null) {
            graph = descriptor.constructGraph();
            graphMap.put(rsc.getGraphKey(), graph);
            graphs.add(graph);
            newGraphs = true;
            issueRefresh();
        }
        graph.addGraphResource(rsc);
        return graph;
    }

    public void removeFromGraph(IGraphableResource<?, ?> rsc) {
        IGraph graph = graphMap.get(rsc.getGraphKey());
        if (graph != null) {
            graph.removeGraphResource(rsc);
            if (graphs.size() > 1 && graph.getResourceCount() == 0) {
                graphs.remove(graph);
                graphMap.remove(rsc.getGraphKey());
                graph.dispose();
                newGraphs = true;
            }
        }
    }

    /**
     * Returns the closest graph to the grid coordinates
     * 
     * @param gridCoords
     * @return
     */
    public IGraph getClosestGraph(Coordinate gridCoords) {
        if (graphs.size() == 0) {
            return null;
        } else if (graphs.size() == 1) {
            return graphs.get(0);
        }
        if (totalExtent == null) {
            return null;
        }

        double yclick = gridCoords.y;
        for (int i = graphs.size() - 1; i >= 0; --i) {
            IGraph graph = graphs.get(i);
            if (graph.isDisplayed()) {
                IExtent extent = graph.getExtent();
                // because the graphs are laid out from top to bottom
                if (extent != null && extent.getMaxY() > yclick) {
                    return graphs.get(i);
                }
            }
        }

        return null;
    }

    /**
     * Returns the closest graph to the grid coordinates
     * 
     * @param gridCoords
     * @return
     */
    public IGraph getFurthestGraph(Coordinate gridCoords) {
        if (graphs.size() == 0) {
            return null;
        } else if (graphs.size() == 1) {
            return graphs.get(0);
        }
        if (totalExtent == null) {
            return null;
        }
        IGraph furthest = null;
        double maxDist = Double.MIN_VALUE;
        double yclick = gridCoords.y;
        for (int i = 0; i < graphs.size(); ++i) {
            IGraph graph = graphs.get(i);
            if (graph.isDisplayed()) {
                IExtent extent = graph.getExtent();
                double dist = Math.min(Math.abs(extent.getMinY() - yclick),
                        Math.abs(extent.getMaxY() - yclick));
                if (dist > maxDist) {
                    maxDist = dist;
                    furthest = graph;
                }
            }
        }

        return furthest;
    }

    public void hideGraph(IGraph graph) {
        graph.setDisplayed(false);
        newGraphs = true;
        issueRefresh();
    }

    public void showGraph(IGraph graph) {
        if (graph != null) {
            graph.setDisplayed(true);
            newGraphs = true;
        }
        issueRefresh();
    }

    public int visibleGraphCount() {
        int i = 0;
        for (IGraph graph : graphs) {
            if (graph.isDisplayed()) {
                ++i;
            }
        }
        return i;
    }

    public int totalGraphCount() {
        return graphs.size();
    }

}
