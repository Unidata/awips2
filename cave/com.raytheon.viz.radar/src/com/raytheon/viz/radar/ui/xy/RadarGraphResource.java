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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.dataplugin.radar.RadarDataKey;
import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.CellTrendDataPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.CellTrendVolumeScanPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.Layer;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.xy.map.rsc.IInsetMapResource;
import com.raytheon.viz.awipstools.capabilities.RangeRingsOverlayCapability;
import com.raytheon.viz.core.graphing.GraphProperties;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYDataList;
import com.raytheon.viz.radar.rsc.AbstractRadarResource;
import com.raytheon.viz.radar.rsc.RadarResourceData;
import com.raytheon.viz.radar.ui.xy.CellTrendGraph.PointType;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Resource that handles radar data that is meant to be displayed on a graph
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2009            askripsk     Initial creation
 * 10-21-09     #1711        bsteffen    Updated Baseline and Points to use new ToolsDataManager
 * 
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */

public class RadarGraphResource extends
        AbstractRadarResource<RadarGraphDescriptor> implements
        IInsetMapResource {

    public enum GraphPosition {
        UL, LL, LR
    }

    // Contains all the graphs for each resource, indexed by the resources time
    private HashMap<DataTime, Map<GraphPosition, CellTrendGraph>> allDataGraphSets;

    private HashMap<GraphPosition, PixelExtent> graphLocations;

    private PointStyle style = PointStyle.STAR;

    public RadarGraphResource(RadarResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);

        getCapabilities().removeCapability(ImagingCapability.class);
        getCapabilities().removeCapability(RangeRingsOverlayCapability.class);

        this.dataTimes = new ArrayList<DataTime>();
        this.allDataGraphSets = new HashMap<DataTime, Map<GraphPosition, CellTrendGraph>>();

    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        GraphProperties graphProps = (GraphProperties) paintProps;

        // Get the current graph set
        displayedDate = paintProps.getDataTime();
        Map<GraphPosition, CellTrendGraph> currentDataGraphSet;
        if (!allDataGraphSets.containsKey(displayedDate)) {
            RadarRecord record = getRadarRecord(displayedDate);
            if (record == null) {
                return;
            }

            // Create the graphs so data series can be added to them
            createGraphs();

            // Get the data for the selected point and add it to the graph
            populateData(record);

        }
        currentDataGraphSet = allDataGraphSets.get(displayedDate);
        setGraphLocations(graphProps);

        for (CellTrendGraph graph : currentDataGraphSet.values()) {
            graph.paint(target, paintProps);
        }

    }

    private void createGraphLocations(GraphProperties graphProps) {
        // Divide into quadrands

        IExtent extent = new PixelExtent(descriptor.getGridGeometry()
                .getGridRange());

        double miniWidth = .9 * extent.getWidth() / 2;

        PixelExtent ulGraphExtent = new PixelExtent(extent.getMinX(),
                extent.getMinX() + miniWidth, extent.getMinY(),
                extent.getMaxY() / 2);
        PixelExtent llGraphExtent = new PixelExtent(extent.getMinX(),
                extent.getMinX() + miniWidth, extent.getMaxY() / 2,
                extent.getMaxY());
        PixelExtent lrGraphExtent = new PixelExtent(extent.getMaxX()
                - miniWidth, extent.getMaxX(), extent.getMaxY() / 2,
                extent.getMaxY());

        graphLocations = new HashMap<GraphPosition, PixelExtent>();
        graphLocations.put(GraphPosition.UL, ulGraphExtent);
        graphLocations.put(GraphPosition.LL, llGraphExtent);
        graphLocations.put(GraphPosition.LR, lrGraphExtent);

    }

    /**
     * Create the graphs that will be used to display data
     * 
     * @param graphProps
     * @return
     */
    private void setGraphLocations(GraphProperties graphProps) {
        // if (graphLocations == null) {
        createGraphLocations(graphProps);
        // }

        CellTrendGraph graph;

        // UL
        graph = getGraph(GraphPosition.UL);
        graph.setWorldExtent(graphLocations.get(GraphPosition.UL));

        // LL
        graph = getGraph(GraphPosition.LL);
        graph.setWorldExtent(graphLocations.get(GraphPosition.LL));

        // LR
        graph = getGraph(GraphPosition.LR);
        graph.setWorldExtent(graphLocations.get(GraphPosition.LR));
    }

    private void createGraphs() {

        CellTrendGraph graph;
        OutlineCapability lineCap = getCapability(OutlineCapability.class);
        ColorableCapability colorCap = getCapability(ColorableCapability.class);

        // UL
        graph = getGraph(GraphPosition.UL);
        graph.setCapabilities(lineCap, colorCap);
        graph.setYLabel("Refl Hgt (kft)");

        // LL
        graph = getGraph(GraphPosition.LL);
        graph.setCapabilities(lineCap, colorCap);

        // LR
        graph = getGraph(GraphPosition.LR);
        graph.setCapabilities(lineCap, colorCap);
        graph.setYLabel("Prob (pct)");
    }

    private void setGraphXLabels(List<Integer> xLabels) {
        if (xLabels == null || xLabels.isEmpty()) {
            // Add some dummy values, since there isn't any data
            xLabels = Arrays.asList(0, 1);
        }

        // UL
        getGraph(GraphPosition.UL).setXValues(xLabels);

        // LL
        getGraph(GraphPosition.LL).setXValues(xLabels);

        // LR
        getGraph(GraphPosition.LR).setXValues(xLabels);
    }

    private CellTrendGraph getGraph(GraphPosition position) {
        Map<GraphPosition, CellTrendGraph> currentGraphSet = allDataGraphSets
                .get(displayedDate);
        if (currentGraphSet == null) {
            currentGraphSet = new EnumMap<GraphPosition, CellTrendGraph>(
                    GraphPosition.class);
            allDataGraphSets.put(displayedDate, currentGraphSet);
        }
        CellTrendGraph graph = currentGraphSet.get(position);
        if (graph == null) {
            graph = new CellTrendGraph();
            allDataGraphSets.get(displayedDate).put(position, graph);
        }

        return graph;
    }

    /**
     * This converts the radar data packets into data series and attaches the
     * series to the appropriate graph
     */
    private void populateData(RadarRecord radarRecord) throws VizException {
        if (radarRecord.getProductCode() == 62) {
            // Storm Structure ==> Cell Trend
            CellTrendGraph graph;
            LineStyle lineType;
            PointType pointType;
            XYDataList currData;
            String dataLabel;
            List<Integer> xLabels = null;
            if (radarRecord.getSymbologyBlock() != null) {
                // Set the x values, the times
                for (Layer currlayer : radarRecord.getSymbologyBlock()
                        .getLayers()) {
                    for (SymbologyPacket currPacket : currlayer.getPackets()) {
                        if (currPacket instanceof CellTrendVolumeScanPacket) {
                            // Set the times of the scans
                            CellTrendVolumeScanPacket packet = (CellTrendVolumeScanPacket) currPacket;

                            xLabels = new ArrayList<Integer>();

                            for (int time : reorderData(
                                    packet.getVolumeTimes(),
                                    packet.getLatestVolume())) {
                                xLabels.add(time);
                            }
                            setGraphXLabels(xLabels);
                        }
                    }
                }

                String selectedPoint = this.resourceData.getPointID();

                // Default to Point A if somehow the point wasn't set
                if ("".equals(selectedPoint)) {
                    selectedPoint = "A";
                }

                // Find the packet that is closest to the selected point
                CellTrendDataPacket packet = getNearestCell(selectedPoint,
                        radarRecord.getSymbologyData());

                // get the data for each trend code
                for (Integer trendCode : packet.getLatestScans().keySet()) {

                    // Top Left:
                    //
                    // Refl Hgt (kft)
                    // 1-Top (^, dotted line)
                    // 2-Base (down arrow, dotted line)
                    // 3-Max (x, solid line)
                    // 8-Cent (o, large-dashed)
                    //
                    // Bottom Left:
                    // 7-Max Refl (dBZ) (x, solid line)
                    // 6-VIL (Kg/m/m) (o, large-dashed)
                    //
                    // Bottom Right:
                    // Prob (Pct)
                    // 4-Hail (o, large-dashed)
                    // 5-Svr Hail (x, solid line)

                    switch (trendCode) {
                    case 1:
                        // Cell Top
                        dataLabel = "Top";
                        // Location: Top Left
                        graph = getGraph(GraphPosition.UL);

                        // Line (^, dotted line)
                        lineType = LineStyle.DOTTED;
                        pointType = PointType.UP_ARROW;
                        break;
                    case 2:
                        // Cell Base
                        dataLabel = "Base";
                        // Location: Top Left
                        graph = getGraph(GraphPosition.UL);

                        // Line (down arrow, dotted line)
                        lineType = LineStyle.DOTTED;
                        pointType = PointType.DOWN_ARROW;
                        break;
                    case 3:
                        // Max. Ref. Hgt.
                        dataLabel = "Max";
                        // Location: Top Left
                        graph = getGraph(GraphPosition.UL);

                        // Line (x, solid line)
                        lineType = LineStyle.SOLID;
                        pointType = PointType.X;
                        break;
                    case 8:
                        // Centroid Hgt.
                        dataLabel = "Cent";
                        // Location: Top Left
                        graph = getGraph(GraphPosition.UL);

                        // Line (o, large-dashed)
                        lineType = LineStyle.DASHED;
                        pointType = PointType.CIRCLE;
                        break;
                    case 7:
                        // Max. Ref.
                        dataLabel = "Max Refl (dBZ)";
                        // Location: Bottom Left
                        graph = getGraph(GraphPosition.LL);

                        // Line (x, solid line)
                        lineType = LineStyle.SOLID;
                        pointType = PointType.X;
                        break;
                    case 6:
                        // Cell Based VIL
                        dataLabel = "Vil (Kg/m/m)";
                        // Location: Bottom Left
                        graph = getGraph(GraphPosition.LL);

                        // Line (o, large-dashed)
                        lineType = LineStyle.DASHED;
                        pointType = PointType.CIRCLE;
                        break;
                    case 4:
                        // Prob. Hail
                        dataLabel = "Hail";
                        // Location: Bottom Right
                        graph = getGraph(GraphPosition.LR);

                        // Line (o, large-dashed)
                        lineType = LineStyle.DASHED;
                        pointType = PointType.CIRCLE;
                        break;
                    case 5:
                        // Prob. Svr. Hail
                        dataLabel = "Svr Hail";
                        // Location: Bottom Right
                        graph = getGraph(GraphPosition.LR);

                        // Line (x, solid line)
                        lineType = LineStyle.SOLID;
                        pointType = PointType.X;
                        break;
                    default:
                        dataLabel = "";
                        graph = null;
                        lineType = null;
                        pointType = null;
                        break;
                    }

                    if (graph != null) {
                        currData = new XYDataList();

                        // Stuff the cell trend data into the data
                        // series
                        int i = 0;

                        int[] volumesData = reorderData(packet
                                .getCellTrendData(trendCode).getData(), packet
                                .getLatestScans().get(trendCode));

                        int dataOffset = xLabels.size() - volumesData.length;
                        for (Integer data : volumesData) {
                            if (data >= 0) {
                                currData.getData().add(
                                        new XYData(xLabels.get(i + dataOffset),
                                                data));
                                i++;
                            }
                        }

                        graph.addDataSeries(currData, lineType, pointType,
                                dataLabel);
                    }
                }
            }
        }
    }

    private CellTrendDataPacket getNearestCell(String point,
            Map<RadarDataKey, RadarDataPoint> symbologyData) {

        Coordinate pointCoord = PointsDataManager.getInstance().getCoordinate(
                point);

        CellTrendDataPacket nearestCell = null;
        CellTrendDataPacket currCell = null;

        GeodeticCalculator gc = new GeodeticCalculator();

        gc.setStartingGeographicPoint(pointCoord.x, pointCoord.y);

        double closestDistance = Double.MAX_VALUE;
        double currDistance;

        LOCATIONS: for (RadarDataKey currKey : symbologyData.keySet()) {
            for (HashMap<Integer, SymbologyPacket> packets : symbologyData
                    .get(currKey).getDisplayPacketData().values()) {
                for (SymbologyPacket currPacket : packets.values()) {
                    if (currPacket instanceof CellTrendDataPacket) {
                        currCell = (CellTrendDataPacket) currPacket;

                        gc.setDestinationGeographicPoint(currKey.getLon(),
                                currKey.getLat());

                        currDistance = gc.getOrthodromicDistance();

                        if (currDistance < closestDistance) {
                            closestDistance = currDistance;
                            nearestCell = currCell;

                        }

                        continue LOCATIONS;
                    }
                }
            }
        }

        return nearestCell;
    }

    private int[] reorderData(ArrayList<Integer> unsortedData, int latestVolume) {
        int[] sortedTimes = new int[unsortedData.size()];

        // Move the times around so the latest time is to the
        // right
        int sortedIndex = 0;
        int origIndex = latestVolume;
        for (int i = latestVolume; i < (unsortedData.size() + latestVolume); i++) {
            if (origIndex >= unsortedData.size()) {
                origIndex = 0;
            }

            sortedTimes[sortedIndex++] = unsortedData.get(origIndex++);
        }

        return sortedTimes;
    }

    /** This should happen on initialization and cache it maybe */
    private Coordinate getMapCoordinate() {
        RadarRecord record = getRadarRecord(displayedDate);
        if (record == null) {
            return null;
        }
        return new Coordinate(record.getLongitude(), record.getLatitude(), 0.0);
    }

    @Override
    public Geometry getInsetMapLocation() {
        return IInsetMapResource.factory.createPoint(getMapCoordinate());
    }

    @Override
    public void paintInsetMap(IGraphicsTarget target,
            PaintProperties paintProps, MapDescriptor insetMapDescriptor)
            throws VizException {
        Coordinate latLon = getMapCoordinate();
        double[] pixels = descriptor.worldToPixel(new double[] { latLon.x,
                latLon.y });
        target.drawPoint(pixels[0], pixels[1], 0.0,
                getCapability(ColorableCapability.class).getColor(), style);
    }
}
