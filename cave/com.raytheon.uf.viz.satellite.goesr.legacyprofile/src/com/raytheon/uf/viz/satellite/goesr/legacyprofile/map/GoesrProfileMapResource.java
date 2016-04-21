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
package com.raytheon.uf.viz.satellite.goesr.legacyprofile.map;

import java.io.FileNotFoundException;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.PaintStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.display.AbstractGriddedDisplay;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.viz.ui.input.EditableManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * GOESR Legacy Moisture/Temperature profiles availability resource. Draws
 * points on map where data is available, user can click points to load
 * sounding.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 30, 2015  4335     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GoesrProfileMapResource extends
        AbstractVizResource<GoesrProfileMapResourceData, IMapDescriptor> {

    private GoesrProfileMapInputHandler inputManager;

    private Map<DataTime, SatelliteRecord> records = new HashMap<>();

    private Map<DataTime, GoesrProfileMapDisplay> displays = new HashMap<>();

    private Job loadJob = new Job("Loading Profile Availability") {

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            DataTime[] timesToLoad = getDataTimes();
            for (DataTime time : timesToLoad) {
                SatelliteRecord record = records.get(time);
                if (record == null) {
                    continue;
                }
                IDataStore dataStore = DataStoreFactory.getDataStore(HDF5Util
                        .findHDF5Location(record));
                IDataRecord dataRecord = null;
                try {
                    dataRecord = dataStore.retrieve(record.getDataURI(),
                            SatelliteRecord.SAT_DATASET_NAME, Request.ALL);
                } catch (FileNotFoundException | StorageException e) {
                    statusHandler.error(
                            "Unable to retrieve satellite data for " + record,
                            e);
                    continue;
                }
                GoesrProfileMapDisplay display = new GoesrProfileMapDisplay(
                        dataRecord, descriptor, record.getGridGeometry(), 5);
                display.setASync(false);
                displays.put(time, display);
                issueRefresh();
            }
            return Status.OK_STATUS;
        }

    };

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected GoesrProfileMapResource(GoesrProfileMapResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        this.inputManager = new GoesrProfileMapInputHandler(this);
        this.dataTimes = new ArrayList<DataTime>();
        getCapability(EditableCapability.class).setEditable(true);
    }

    @Override
    protected void resourceDataChanged(ChangeType type, Object updateObject) {
        if (type == ChangeType.DATA_UPDATE) {
            if (updateObject instanceof PluginDataObject[]) {
                addRecords((PluginDataObject[]) updateObject);
            }
        }
        super.resourceDataChanged(type, updateObject);
    }

    public synchronized void addRecords(PluginDataObject... records) {
        for (PluginDataObject record : records) {
            if (record instanceof SatelliteRecord) {
                this.records
                        .put(record.getDataTime(), (SatelliteRecord) record);
            }
        }

        List<DataTime> dataTimes = new ArrayList<DataTime>(
                this.records.keySet());
        Collections.sort(dataTimes);
        this.dataTimes = dataTimes;
        loadJob.schedule();
    }

    @Override
    public synchronized void remove(DataTime dataTime) {
        records.remove(dataTime);
        GoesrProfileMapDisplay display = displays.remove(dataTime);
        if (display != null) {
            display.dispose();
        }
        super.remove(dataTime);
    }

    protected void resetDisplays() {
        Map<DataTime, GoesrProfileMapDisplay> displays = this.displays;
        this.displays = new HashMap<>();
        for (GoesrProfileMapDisplay display : displays.values()) {
            display.dispose();
        }
    }

    @Override
    protected void disposeInternal() {
        inputManager.dispose();
        getResourceContainer().unregisterMouseHandler(inputManager);
        resetDisplays();
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        resetDisplays();
        loadJob.schedule();
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        DataTime time = paintProps.getDataTime();
        if (time == null) {
            return;
        }
        GoesrProfileMapDisplay display = displays.get(time);
        if (display != null) {
            display.setColor(getCapability(ColorableCapability.class)
                    .getColor());
            display.setDensity(getCapability(DensityCapability.class)
                    .getDensity());
            display.setMagnification(getCapability(
                    MagnificationCapability.class).getMagnification());
            display.paint(target, paintProps);
        } else if (loadJob.getState() == Job.RUNNING) {
            updatePaintStatus(PaintStatus.INCOMPLETE);
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        EditableManager.makeEditable(this,
                getCapability(EditableCapability.class).isEditable());
        getResourceContainer().registerMouseHandler(inputManager);
    }

    public boolean isEditable() {
        return getCapability(EditableCapability.class).isEditable()
                && getProperties().isVisible();
    }

    @Override
    public String getName() {
        return "GOES-R Legacy Temperature/Moisture Availability";
    }

    public boolean isDataPoint(Coordinate sampleCoord) {
        DataTime time = descriptor.getTimeForResource(this);
        if (time == null) {
            return false;
        }
        GoesrProfileMapDisplay display = displays.get(time);
        if (display == null) {
            return false;
        }
        GeneralGridGeometry dataGeometry = display.getGridGeometryOfData();
        DirectPosition2D point = new DirectPosition2D(sampleCoord.x,
                sampleCoord.y);
        try {
            MathTransform ll2crs = CRS.findMathTransform(
                    DefaultGeographicCRS.WGS84,
                    dataGeometry.getCoordinateReferenceSystem(), true);
            MathTransform crs2grid = dataGeometry.getGridToCRS().inverse();
            ll2crs.transform(point, point);
            crs2grid.transform(point, point);
        } catch (TransformException | FactoryException e) {
            statusHandler.error("", e);
            return false;
        }
        int nx = dataGeometry.getGridRange().getSpan(0);
        int ny = dataGeometry.getGridRange().getSpan(1);
        if (point.y < 0 || point.y >= ny || Double.isNaN(point.y)) {
            return false;
        } else if (point.x < 0 || point.x >= nx || Double.isNaN(point.x)) {
            return false;
        } else {
            return display.isDataPoint(point.x, point.y);
        }
    }

    private static class GoesrProfileMapDisplay extends
            AbstractGriddedDisplay<DrawableCircle> {

        private final int nx;

        private final BitSet dataMap;

        public GoesrProfileMapDisplay(IDataRecord dataRecord,
                IMapDescriptor descriptor,
                GeneralGridGeometry gridGeometryOfGrid, double size) {
            super(descriptor, gridGeometryOfGrid, size, 6);
            Object array = dataRecord.getDataObject();
            int fill = dataRecord.getFillValue().intValue();
            /* Don't waste space, we only need to know which points have data. */
            int length = Array.getLength(array);
            dataMap = new BitSet(length);
            for (int i = 0; i < length; i += 1) {
                dataMap.set(i, Array.getInt(array, i) != fill);
            }
            this.nx = (int) dataRecord.getSizes()[0];
        }

        public boolean isDataPoint(double x, double y) {
            int index = (int) (nx * Math.round(y) + Math.round(x));
            return dataMap.get(index);
        }

        public GeneralGridGeometry getGridGeometryOfData() {
            return gridGeometryOfGrid;
        }

        @Override
        protected DrawableCircle getResource(Coordinate coord) {
            if (!isDataPoint(coord.x, coord.y)) {
                return null;
            }
            DrawableCircle circle = new DrawableCircle();
            circle.screenRadius = size * magnification;
            circle.numberOfPoints = (int) (circle.screenRadius * 4);
            circle.basics.color = color;
            circle.filled = true;
            return circle;
        }

        @Override
        protected DrawableCircle createResource(Coordinate coord)
                throws VizException {
            return getResource(coord);
        }

        @Override
        protected void paint(PaintProperties paintProps,
                Collection<GridCellRenderable> renderables) throws VizException {
            List<DrawableCircle> circles = new ArrayList<DrawableCircle>(
                    renderables.size());
            for (GridCellRenderable gridCell : renderables) {
                gridCell.resource.setCoordinates(gridCell.plotLocation.x,
                        gridCell.plotLocation.y);
                circles.add(gridCell.resource);
            }
            target.drawCircle(circles.toArray(new DrawableCircle[0]));
        }

        @Override
        protected void disposeResources() {
            /* DrawableCircles do not need to be disposed. */
        }
    }
}
