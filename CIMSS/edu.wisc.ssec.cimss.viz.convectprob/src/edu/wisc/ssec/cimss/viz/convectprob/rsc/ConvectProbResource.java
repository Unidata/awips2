package edu.wisc.ssec.cimss.viz.convectprob.rsc;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMapException;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.common.colormap.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.dataplugin.HDF5Util;

import edu.wisc.ssec.cimss.common.dataplugin.convectprob.ConvectProbRecord;

/**
 * NOAA/CIMSS Prob Severe Model Visualization Resource
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Mar 27, 2014 DCS 15298   lcronce     Initial Creation.
 *
 * </pre
 *
 * @author Lee Cronce
 * @version 1.0
 *
 */
public class ConvectProbResource extends
AbstractVizResource<ConvectProbResourceData, MapDescriptor> {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ConvectProbResource.class);

    /**
     * Class defining the attributes of the display frame
     *
     * This class holds cached shapes to avoid recalculating everything all
     * the time
     */
    private class DisplayFrame {
        Collection<ConvectProbRecord> records = new ArrayList<ConvectProbRecord>();

        IWireframeShape shape;

        protected void dispose() {
            if (this.shape != null) {
                this.shape.dispose();
                this.shape = null;
            }
        }

        protected void createShapes(IGraphicsTarget target) {
            dispose();
            shape = target.createWireframeShape(false, descriptor);
        }

    }

    // Place to store records that have not yet been processed
    private Map<DataTime, Collection<ConvectProbRecord>> unprocessedRecords = new HashMap<DataTime, Collection<ConvectProbRecord>>();

    private Map<DataTime, DisplayFrame> frames = new HashMap<DataTime, DisplayFrame>();

    private DataTime displayedDataTime;

    /**
     * Constructor to define an instance of this resource
     *
     * @param ConvectProb plugin resource data
     * @param Display load properties
     */
    protected ConvectProbResource(ConvectProbResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        resourceData.addChangeListener(new IResourceDataChanged() {
            @Override
            public void resourceChanged(ChangeType type, Object object) {
                if (type == ChangeType.DATA_UPDATE) {
                    PluginDataObject[] pdo = (PluginDataObject[]) object;
                    for (PluginDataObject p : pdo) {
                        if (p instanceof ConvectProbRecord) {
                            addRecord((ConvectProbRecord) p);
                        }
                    }
                }
                issueRefresh();
            }
        });
        this.dataTimes = new ArrayList<DataTime>();

    }

    /**
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        // Make sure we ditch all the shapes before we go
        disposeFrames();
    }

    /**
     * Disposes of all frames of visualization
     */
    private void disposeFrames() {
        synchronized (frames) {
            for (DisplayFrame frame : frames.values()) {
                frame.dispose();
            }
        }
    }

    /**
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getDataTimes()
     */
    @Override
    public DataTime[] getDataTimes() {
        if (this.dataTimes == null) {
            return new DataTime[0];
        }
        synchronized (dataTimes) {
            return this.dataTimes.toArray(new DataTime[0]);
        }
    }

    /**
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal(com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        // Create colormap parameters
        ColorMapParameters colorMapParams = getCapability(
                ColorMapCapability.class).getColorMapParameters();
        if (colorMapParams == null) {
            colorMapParams = new ColorMapParameters();
        }
        String name = "Grid/gridded data";
        if (colorMapParams.getColorMap() == null) {
            if (colorMapParams.getColorMapName() != null)
                name = colorMapParams.getColorMapName();
            try {
                colorMapParams.setColorMap(ColorMapLoader.loadColorMap(name));
            } catch (ColorMapException e) {
                statusHandler.handle(Priority.ERROR, "Error loading specified colormap: " + name, e);
            }
        }

        colorMapParams.setColorMapMax(100.0f);
        colorMapParams.setColorMapMin(0.0f);
        float[] cbi = {0.0f, 10.0f, 20.0f, 30.0f, 40.0f, 50.0f, 60.0f, 70.0f, 80.0f, 90.0f, 100.0f};
        colorMapParams.setColorBarIntervals(cbi);

    }

    /**
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#inspect(com.raytheon.uf.common.geospatial.ReferencedCoordinate)
     */
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        DisplayFrame frame = null;
        synchronized (frames) {
            frame = frames.get(this.displayedDataTime);
        }
        if (frame == null) {
            return "";
        }
        StringBuilder sample = new StringBuilder();
        Coordinate latLon = new Coordinate();
        try {
            latLon = coord.asLatLon();
        } catch (Exception e1) {
            statusHandler.handle(Priority.ERROR, "Error converting ReferencedCoordinate to Lat/Lon", e1);
        }
        GeometryFactory geom = new GeometryFactory();
        Point point = geom.createPoint(latLon);
        for (ConvectProbRecord record : frame.records) {
            // Check if we have an area we are rendering
            if (resourceData.isDisplayShape()) {
                try {
                    Geometry[] pg = record.getPolyGeoms();
                    for (int i=0; i < pg.length; i++) {
                        if (pg[i].contains(point)) {
                            sample.append("SVR PROB: "+Integer.toString(record.getProbabilities()[i])+"%");
                            sample.append("\n- Env MUCAPE: "+record.getMucapes()[i]);
                            sample.append("\n- Env EBShear: "+record.getEbshears()[i]);
                            sample.append("\n- MRMS MESH: "+record.getMeshes()[i]);
                            sample.append("\n- Norm Vert Growth Rate (Max): "+record.getRcemisses()[i]);
                            sample.append("\n- Glaciation Rate (Max): "+record.getRcicecfs()[i]);
                            if (resourceData.isShowObjectId()) {
                                sample.append("\n- Object ID: "+record.getObjectids()[i]);
                            }
                            return sample.toString();
                        }
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.ERROR, "Error interogating convectprob data", e);
                }
            }
        }
        return "";
    }

    /**
     * Process all records for the displayedDataTime
     *
     * @param target
     * @param paintProps
     * @throws VizException
     */
    private void updateRecords(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        DisplayFrame frame = null;
        synchronized (frames) {
            frame = frames.get(this.displayedDataTime);
            if (frame == null) {
                frame = new DisplayFrame();
                frames.put(this.displayedDataTime, frame);
            }
        }

        // Add all the new Records
        Collection<ConvectProbRecord> newRecords = null;
        synchronized (unprocessedRecords) {
            newRecords = unprocessedRecords.get(this.displayedDataTime);
        }
        for (ConvectProbRecord record : newRecords) {
            // If we need to draw anything for this record then keep it
            if (resourceData.isDisplayShape()) {
                frame.records.add(record);
            }
        }
        newRecords.clear();
        // Get some new shapes
        frame.createShapes(target);
        // Update each record
        for (ConvectProbRecord record : frame.records) {
            File f = HDF5Util.findHDF5Location(record);
            IDataStore ds = DataStoreFactory.getDataStore(f);
            record.retrieveFromDataStore(ds);
        }
    }

    /**
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon.uf.viz.core.IGraphicsTarget, com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        // Create colormap parameters
        ColorMapParameters colorMapParams = getCapability(
                ColorMapCapability.class).getColorMapParameters();

        this.displayedDataTime = paintProps.getDataTime();

        // First check to see if we need to process new data
        Collection<ConvectProbRecord> unprocessed = null;
        synchronized (unprocessedRecords) {
            unprocessed = unprocessedRecords.get(this.displayedDataTime);
        }
        if (unprocessed != null && unprocessed.size() > 0) {
            updateRecords(target, paintProps);
        }

        // Hopefully we now have some data to display, if not bail
        DisplayFrame frame = null;
        synchronized (frames) {
            frame = frames.get(this.displayedDataTime);
        }
        if (frame == null) {
            this.displayedDataTime = null;
            return;
        }

        if (frame.records != null && frame.shape != null) {
            boolean drawShape = false;
            Color color;
            float a, thick;
            RGB shapeColor = new RGB(255, 255, 255);
            for (ConvectProbRecord rec : frame.records) {
                if (rec.getPolyGeoms() != null) {
                    Geometry[] polyGeoms = rec.getPolyGeoms();
                    int[] probabilities = rec.getProbabilities();
                    for (int j=0; j < 101; j++) {
                        frame.shape.reset();
                        drawShape = false;
                        for (int i=0; i < polyGeoms.length; i++) {
                            if (probabilities[i] == j) {
                                frame.shape.addLineSegment(polyGeoms[i].getCoordinates());
                                drawShape = true;
                            }
                        }
                        if (drawShape) {
                            color = colorMapParams.getColorByValue((float) j);
                            shapeColor.red = (int) (color.getRed() * 255);
                            shapeColor.green = (int) (color.getGreen() * 255);
                            shapeColor.blue = (int) (color.getBlue() * 255);
                            a = color.getAlpha() * paintProps.getAlpha();
                            thick = 7.0f;
                            if (j < 50) {
                                thick = 4.0f;
                            }
                            target.drawWireframeShape(frame.shape, shapeColor, thick, LineStyle.SOLID, a);
                        }
                    }
                }
            }
        }
    }


    /**
     * Adds a new record to this resource
     *
     * @param new ConvectProb record
     */
    protected void addRecord(ConvectProbRecord obj) {
        DataTime dataTime = obj.getDataTime();
        if (dataTime != null) {
            Collection<ConvectProbRecord> records = null;
            boolean brandNew = false;
            synchronized (unprocessedRecords) {
                records = unprocessedRecords.get(dataTime);
                if (records == null) {
                    records = new LinkedHashSet<ConvectProbRecord>();
                    unprocessedRecords.put(dataTime, records);
                    brandNew = true;
                }
            }
            if (brandNew) {
                synchronized (dataTimes) {
                    this.dataTimes.add(dataTime);
                    Collections.sort(this.dataTimes);
                }
            }

            records.add(obj);
        }
    }

    /**
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        return "NOAA/CIMSS Prob Severe Model (%)";
    }

    /**
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        synchronized (frames) {
            disposeFrames();
            // add as unprocessed to make sure frames created
            for (DataTime time : frames.keySet()) {
                DisplayFrame frame = frames.get(time);
                if (frame != null) {
                    List<ConvectProbRecord> copyList = new ArrayList<ConvectProbRecord>(
                            frame.records);
                    synchronized (unprocessedRecords) {
                        unprocessedRecords.put(time, copyList);
                    }
                }
            }
        }
    }

    /**
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#remove(com.raytheon.uf.common.time.DataTime)
     */
    @Override
    public void remove(DataTime time) {
        super.remove(time);
        Collection<ConvectProbRecord> notNeeded = null;
        synchronized (unprocessedRecords) {
            notNeeded = unprocessedRecords.remove(time);
        }
        if (notNeeded != null) {
            notNeeded.clear();
        }

        DisplayFrame frame = null;
        synchronized (frames) {
            frame = frames.remove(time);
        }
        if (frame != null) {
            frame.dispose();
        }
    }

}
