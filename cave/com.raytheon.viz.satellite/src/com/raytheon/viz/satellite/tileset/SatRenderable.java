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
package com.raytheon.viz.satellite.tileset;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.geospatial.data.GeographicDataSource;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Renderable for displaying multiple {@link SatelliteRecord}s at the same time.
 * Each SatelliteRecord is rendered using a {@link SatTileSetRenderable}. Only
 * one record with the same {@link SatMapCoverage} can be contained in the
 * renderable to prevent overlap. This renderable is not designed to handle
 * overlapping records, it should be used for single records or multiple records
 * from a pre-tiled source(such as GOES-R or Himawari CMI).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 *  Date          Ticket#   Engineer  Description
 *  ------------- --------  --------- --------------------------
 *  Oct 08, 2015  4937      bsteffen  Extracted from SatResource
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class SatRenderable implements IRenderable {

    private final AbstractVizResource<?, ?> resource;

    private Map<SatMapCoverage, SatTileSetRenderable> tileMap = new HashMap<SatMapCoverage, SatTileSetRenderable>();

    private DataTime renderableTime;

    public SatRenderable(AbstractVizResource<?, ?> resource,
            DataTime renderableTime) {
        this.resource = resource;
        this.renderableTime = renderableTime;
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        Collection<DrawableImage> images = getImagesToRender(target,
                paintProps);
        if (images.isEmpty() == false) {
            target.drawRasters(paintProps,
                    images.toArray(new DrawableImage[0]));
        }
    }

    public Collection<DrawableImage> getImagesToRender(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        List<DrawableImage> images = new ArrayList<DrawableImage>();
        synchronized (tileMap) {
            for (SatTileSetRenderable renderable : tileMap.values()) {
                images.addAll(renderable.getImagesToRender(target, paintProps));
            }
        }
        return images;
    }

    public void addRecord(SatelliteRecord record) {
        synchronized (tileMap) {
            SatTileSetRenderable tileSet = tileMap.get(record.getCoverage());
            if (tileSet != null) {
                SatelliteRecord existingRecord = tileSet.getSatelliteRecord();
                if (existingRecord.equals(record) == false) {
                    // Different record, same spatial area for same frame
                    // Determine if new one is better than existing
                    long existingTimeMillis = existingRecord.getDataTime()
                            .getMatchRef();
                    long newRecordTimeMillis = record.getDataTime()
                            .getMatchRef();
                    long normalTimeMillis = renderableTime.getMatchRef();
                    if (Math.abs(normalTimeMillis - newRecordTimeMillis) < Math
                            .abs(normalTimeMillis - existingTimeMillis)) {
                        // New is better since it's data time is closer to
                        // the normal time than the existing record's time!
                        tileSet.dispose();
                        tileSet = null;
                    }
                }
            }
            if (tileSet == null) {
                tileSet = createTileSet(record);
                tileMap.put(record.getCoverage(), tileSet);
            }
        }
    }

    protected SatTileSetRenderable createTileSet(SatelliteRecord record) {
        SatTileSetRenderable tileSet = new SatTileSetRenderable(resource,
                record);
        tileSet.project(resource.getDescriptor().getGridGeometry());
        return tileSet;
    }

    public void project() {
        synchronized (tileMap) {
            for (SatTileSetRenderable renderable : tileMap.values()) {
                renderable.project(resource.getDescriptor().getGridGeometry());
            }
        }
    }

    public void dispose() {
        synchronized (tileMap) {
            for (SatTileSetRenderable renderable : tileMap.values()) {
                renderable.dispose();
            }
            tileMap.clear();
        }
    }

    public InterrogationResult interrogate(Coordinate latLon,
            Unit<?> requestUnit) throws VizException {
        InterrogationResult result = null;
        synchronized (tileMap) {
            for (SatTileSetRenderable renderable : tileMap.values()) {
                double rValue = renderable.interrogate(latLon, requestUnit);
                if (Double.isNaN(rValue) == false) {
                    result = new InterrogationResult(renderable, rValue);
                }
            }
        }
        return result;
    }

    /**
     * The result of interrogating a {@link SatRenderable}. Usually the value is
     * the only thing of interest to the caller but this object makes it
     * possible to get more specific information from the specific
     * {@link SatTileSetRenderable} that was used to get the value.
     */
    public static class InterrogationResult {

        private final SatTileSetRenderable renderable;

        private final double value;

        public InterrogationResult(SatTileSetRenderable renderable,
                double value) {
            this.renderable = renderable;
            this.value = value;
        }

        public SatelliteRecord getRecord() {
            return renderable.getSatelliteRecord();
        }

        public double getValue() {
            return value;
        }

        public GeographicDataSource getDataSource() {
            return renderable.getCurrentLevelDataSource();
        }

    }

}