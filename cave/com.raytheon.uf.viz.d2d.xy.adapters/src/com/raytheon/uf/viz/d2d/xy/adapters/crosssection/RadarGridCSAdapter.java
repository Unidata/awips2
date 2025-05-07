/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.d2d.xy.adapters.crosssection;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.tuple.Pair;
import org.geotools.coverage.grid.GridGeometry2D;
import org.locationtech.jts.geom.Coordinate;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.grid.radar.RadarVirtualTimeAndSpace;
import com.raytheon.uf.viz.grid.radar.util.RadarVirtualVolumeStatus;
import com.raytheon.uf.viz.grid.radar.util.RadarVirtualVolumeUtil;
import com.raytheon.uf.viz.xy.crosssection.graph.CrossSectionGraph;
import com.raytheon.uf.viz.xy.crosssection.rsc.AbstractCrossSectionResource;
import com.raytheon.viz.core.map.GeoUtil;
import com.raytheon.viz.grid.record.RequestableDataRecord;
import com.raytheon.viz.radar.util.RadarAsGridUtil;

/**
 * Adapter for radar-as-grid data.
 *
 * This specifies that Hydrometeor Classification (HC) data should use nearest
 * neighbor interpolation.
 *
 * This also adds support for the "virtual volume" concept, where higher tilts
 * from the previous volume scan are blended into the current volume scan to
 * produce a full volume scan. The actual blending of records is handled by the
 * underlying derived parameters, but this does the following:
 *
 * <pre>
 * 1) Registers listeners so that only the latest frame uses virtual volumes
 *    and so that frame is reloaded when virtual volumes are toggled on/off.
 * 2) Indicates the current virtual volume status in the product legend and by
 *    drawing a line that indicates the current scan's latest elevation angle
 * </pre>
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 16, 2024 2037092    mapeters    Initial creation
 * Jun 20, 2024 2037565    mapeters    Add getExtraLegendText to indicate
 *                                     virtual volume status in legend
 * Jul 15, 2024 2037624    mapeters    Extract logic to RadarVirtualVolumeUtil
 * Aug 06, 2024 2037698    bines       Changed to RadarGridCSAdapter and added
 *                                     useNearestNeighbor function
 * Aug 20, 2024 2037631    mapeters    Indicate virtual volume status by drawing a line on the
 *                                     graph, move extra legend text into new frame renderable
 *                                     class also so it always matches displayed data
 * Oct 14, 2024 2037939    mapeters    Override getMetadataMaps()
 *
 * </pre>
 *
 * @author mapeters
 */
public class RadarGridCSAdapter extends GridCSAdapter {

    private static final long serialVersionUID = 1L;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarGridCSAdapter.class);

    @Override
    public void setResource(AbstractCrossSectionResource resource) {
        super.setResource(resource);

        String paramAbbrev = getParameterAbbrev();
        if (RadarAsGridUtil.isVirtualVolume(paramAbbrev)) {
            RadarVirtualVolumeUtil.registerVirtualVolumeListeners(resource,
                    this::getRecords);
        }
    }

    private Collection<GridRecord> getRecords() {
        synchronized (records) {
            return List.copyOf(records);
        }
    }

    @Override
    public boolean useNearestNeighbor() {
        String paramAbbrev = getParameterAbbrev();

        /*
         * Hydrometeor Class (HC) uses nearest neighbor since it is categorical
         * data rather than numerical.
         */
        return RadarRecordUtil.HC
                .equals(RadarAsGridUtil.getStandardParamAbbrev(paramAbbrev));
    }

    /*
     * This is overridden to get y records (tilt heights) for the correct time
     * when virtual volumes are used. This checks which tilts are using radar
     * data records from the previous scan, and then updates those tilts to use
     * y records from the previous scan as well. This should only ever
     * practically matter if the Volume Coverage Pattern (VCP) changed between
     * the previous scan and the current scan, which changes the primary
     * elevation angle -> true elevation angle mappings.
     */
    @Override
    protected Pair<Map<Level, GridRecord>, Map<Level, GridRecord>> getMetadataMaps(
            DataTime time) throws VizException {
        Pair<Map<Level, GridRecord>, Map<Level, GridRecord>> metadataMaps = super.getMetadataMaps(
                time);
        if (metadataMaps == null) {
            return metadataMaps;
        }

        // Determine which tilts are using data from the previous scan
        Map<Level, GridRecord> xMap = metadataMaps.getLeft();
        Set<Level> prevScanLevels = new HashSet<>();
        Set<DataTime> prevScanTimes = new HashSet<>();
        for (GridRecord xRecord : xMap.values()) {
            if (xRecord instanceof RequestableDataRecord) {
                TimeAndSpace tas = ((RequestableDataRecord) xRecord)
                        .getTimeAndSpace();
                if (tas instanceof RadarVirtualTimeAndSpace) {
                    prevScanTimes.add(
                            ((RadarVirtualTimeAndSpace) tas).getPrevScanTime());
                    prevScanLevels.add(xRecord.getLevel());
                }
            }
        }

        if (prevScanTimes.isEmpty()) {
            return metadataMaps;
        } else if (prevScanTimes.size() > 1) {
            throw new RuntimeException("Multiple previous scan times for '"
                    + time + "' virtual volume: " + prevScanTimes);
        }

        /*
         * For tilts that are using radar data from the previous scan, update
         * their y records to be for the previous scan as well.
         */
        DataTime prevScanTime = prevScanTimes.iterator().next();
        Map<Level, GridRecord> yMap = metadataMaps.getRight();
        Set<GridRecord> prevScanYRecords = getYRecords(prevScanTime);
        for (GridRecord rec : prevScanYRecords) {
            if (prevScanLevels.contains(rec.getLevel())) {
                yMap.put(rec.getLevel(), rec);
            }
        }
        return metadataMaps;
    }

    @Override
    protected RadarCSFrameVirtualVolumeRenderable buildExtraFrameRenderable(
            DataTime frameTime, CrossSectionGraph graph,
            Map<Level, GridRecord> xMap, Map<Level, GridRecord> yMap,
            GridGeometry2D geometry) {
        if (!RadarAsGridUtil.isVirtualVolume(getParameterAbbrev())) {
            return null;
        }

        List<AbstractRequestableData> data = xMap.values().stream()
                .filter(r -> r instanceof RequestableDataRecord)
                .map(r -> ((RequestableDataRecord) r).getRequester())
                .collect(Collectors.toList());
        RadarVirtualVolumeStatus vvStatus = RadarVirtualVolumeStatus.build(data,
                frameTime);
        if (vvStatus == null) {
            return null;
        }

        /*
         * Get screen coordinates of latest tilt, so that it can be drawn on the
         * screen to indicate where the current scan's data and previous scan's
         * data is being blended in the virtual volume.
         */
        Level tiltLevel = LevelFactory.getInstance().getLevel(RadarUtil.TILT,
                vvStatus.getCurrScanTilt());
        GridRecord currTiltYRecord = yMap.get(tiltLevel);
        List<double[]> currTiltLineCoords = null;
        try {
            currTiltLineCoords = getTiltLine(frameTime, graph, currTiltYRecord,
                    geometry);
        } catch (Exception e) {
            statusHandler.error("Error building virtual volume line", e);
        }
        return new RadarCSFrameVirtualVolumeRenderable(vvStatus,
                currTiltLineCoords);
    }

    /**
     * Get a list of coordinates that indicates the given radar tilt's height
     * along the baseline, in pixel coordinates relative to the graph.
     *
     * @param frameTime
     * @param graph
     * @param tiltYRecord
     *            record containing height values of radar tilt
     * @param geometry
     * @return tilt line coordinates, in pixel coordinates
     * @throws VizException
     */
    protected List<double[]> getTiltLine(DataTime frameTime,
            CrossSectionGraph graph, GridRecord tiltYRecord,
            GridGeometry2D geometry) throws VizException {
        Coordinate[] baselineVertices = descriptor.getLine(frameTime)
                .getCoordinates();
        // Include somewhat arbitrary multiplier to make the line smoother
        int nx = (int) geometry.getGridRange2D().getWidth() * 20;
        List<Pair<Coordinate, Double>> coordAndDistanceList = GeoUtil
                .splitLineWithDistances(nx, baselineVertices);

        List<double[]> tiltLineCoords = new ArrayList<>();
        for (Pair<Coordinate, Double> coordAndDistance : coordAndDistanceList) {
            Coordinate coord = coordAndDistance.getLeft();
            double distance = coordAndDistance.getRight();
            // Interpolate latest radar tilt's height at this coord
            float height = interpolateAndConvertYVal(tiltYRecord, coord,
                    frameTime, geometry);

            // Convert to screen coord
            double[] gridLoc;
            if (height <= INVALID_VALUE_CUTOFF) {
                // Prevent stray line segments being drawn for invalid heights
                gridLoc = new double[] { Double.NaN, Double.NaN };
            } else {
                gridLoc = graph.getGridLocation(distance, height);
            }
            tiltLineCoords.add(gridLoc);
        }

        return tiltLineCoords;
    }
}
