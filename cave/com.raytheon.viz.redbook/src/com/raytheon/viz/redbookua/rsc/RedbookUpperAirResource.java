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
package com.raytheon.viz.redbookua.rsc;

import java.awt.image.BufferedImage;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.Validate;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.redbook.RedbookRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.RasterMode;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.data.prep.IODataPreparer;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.viz.pointdata.PlotData;
import com.raytheon.viz.pointdata.PlotModelFactory2;
import com.raytheon.viz.redbook.Activator;
import com.raytheon.viz.redbook.RedbookWMOMap;
import com.raytheon.viz.redbookua.RedbookUpperAirDecoder;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Resource data for redbook
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 24, 2010 1029       dfriedma    Initial creation
 * Jul 24, 2013 2203       njensen     Synchronized init and dispose of frames
 * Mar 13, 2014 2907       njensen     split edex.redbook plugin into common and
 *                                     edex redbook plugins
 * 
 * </pre>
 * 
 * @author dfriedma
 * @version 1.0
 */
public class RedbookUpperAirResource extends
        AbstractVizResource<RedbookUpperAirResourceData, MapDescriptor>
        implements IResourceDataChanged {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RedbookUpperAirResource.class);

    private final Map<DataTime, UAFrame> frames = new HashMap<DataTime, UAFrame>();

    private DataTime displayedDataTime;

    private IFont font;

    private String humanReadableName;

    private PlotModelFactory2 plotModelFactory;

    private IGraphicsTarget graphicsTarget;

    private double plotWidth;

    private boolean plotSettingsChanged = true;

    private LoadAndRenderJob job = new LoadAndRenderJob();

    protected RedbookUpperAirResource(RedbookUpperAirResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        resourceData.addChangeListener(this);
        dataTimes = new ArrayList<DataTime>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        for (UAFrame frame : this.frames.values())
            frame.dispose();

        if (font != null) {
            font.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        DataTime[] times = descriptor.getTimeMatchingMap().get(this);
        for (int i = 0; i < times.length; i++) {
            if (times[i] != null) {
                times[i] = times[i].clone();
                times[i].getUtilityFlags().remove(DataTime.FLAG.FCST_USED);
            }
        }
        if (this.humanReadableName == null) {
            buildHumanReadableName();
        }

        return this.humanReadableName;
    }

    private void buildHumanReadableName() {
        this.humanReadableName = "Redbook Resource";

        RequestConstraint wmo = this.resourceData.getMetadataMap().get(
                "wmoTTAAii");
        if (wmo != null) {
            RedbookWMOMap map;
            try {
                map = RedbookWMOMap.load();
            } catch (Exception e) {
                VizApp.logAndAlert(Status.ERROR, e,
                        "Error loading redbook mapping",
                        "Unable to load redbook mapping file",
                        Activator.getDefault(), Activator.PLUGIN_ID);
                return;
            }
            RedbookWMOMap.Info info = map.mapping.get(wmo.getConstraintValue());
            if (info != null && info.name != null)
                this.humanReadableName = info.name;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#setDescriptor(com.raytheon
     * .uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public void setDescriptor(MapDescriptor descriptor) {
        super.setDescriptor(descriptor);
        Validate.isTrue(descriptor instanceof MapDescriptor,
                "Redbook resource can only be used on MapDescriptors");
        synchronized (job) {
            plotModelFactory = null;
            plotWidth = 0;
            invalidateAll();
            plotSettingsChanged = true;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getDataTimes()
     */
    @Override
    public DataTime[] getDataTimes() {
        Set<DataTime> dataTimeSet = frames.keySet();
        DataTime[] dataTimes = dataTimeSet.toArray(new DataTime[dataTimeSet
                .size()]);
        return dataTimes;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IUpdateableDataResource#remove
     * (com.raytheon.uf.common.time.DataTime)
     */
    @Override
    public void remove(DataTime dataTime) {
        dataTimes.remove(dataTime);
        UAFrame frame = frames.remove(dataTime);
        if (frame != null)
            frame.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#init(com.raytheon.uf
     * .viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        this.font = target.initializeFont("Monospace", 10, null);
        this.graphicsTarget = target;
    }

    protected PlotModelFactory2 getPlotModelFactory() {
        if (plotModelFactory == null) {
            plotModelFactory = new PlotModelFactory2(getDescriptor(),
                    "redbookuaDesign.svg");
            plotModelFactory.setColor(getCapability(ColorableCapability.class)
                    .getColor());
            plotModelFactory.setUpperLimit(10000); // TODO: correct value
        }
        return plotModelFactory;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderable#paint(com.raytheon.uf.
     * viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        this.displayedDataTime = paintProps.getDataTime();

        UAFrame frame = frames.get(displayedDataTime);
        if (frame != null) {

            if (frame.isReadyToPaint())
                frame.paint(target, paintProps);
            else
                job.upadte();
        }
    }

    private void invalidateAll() {
        synchronized (job) {
            for (UAFrame frame : frames.values())
                frame.invalidate();
        }
    }

    public IFont getRenderingFont() {
        return this.font;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IProjectableResource#project(org
     * .opengis.referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        super.project(mapData);
        setDescriptor(getDescriptor());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IResourceDataChanged#resourceChanged(com
     * .raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            Validate.isTrue(object instanceof PluginDataObject[], "Expected a "
                    + PluginDataObject[].class + ", Got: " + object.getClass());

            PluginDataObject[] pdos = (PluginDataObject[]) object;
            for (PluginDataObject record : pdos) {
                Validate.notNull(record, "PluginDataObject was null");
                Validate.isTrue(record instanceof RedbookRecord,
                        "RedbookResource expects RedbookRecords, got " + record);

                addRecord((RedbookRecord) record);
            }
        } else if (type.equals(ChangeType.CAPABILITY)) {
            if (object instanceof ColorableCapability) {
                synchronized (job) {
                    if (plotModelFactory != null) {
                        plotModelFactory
                                .setColor(((ColorableCapability) object)
                                        .getColor());
                    }
                    invalidateAll();
                    plotSettingsChanged = true;
                }
            } else if (object instanceof MagnificationCapability) {
                synchronized (job) {
                    invalidateAll();
                    plotSettingsChanged = true;
                }
            }
        }
        issueRefresh();
    }

    public void addRecord(RedbookRecord record) {
        DataTime dataTime = record.getDataTime();
        frames.put(dataTime, new UAFrame(dataTime, record));
        dataTimes.add(record.getDataTime());
    }

    private class UAFrame {
        private RedbookRecord redbookRecord;

        String dumpTime;

        PointDataContainer pointData;

        IImage[] images;

        private boolean valid;

        public UAFrame(DataTime dataTime, RedbookRecord record) {
            this.redbookRecord = record;
        }

        public boolean isReadyToPaint() {
            return pointData != null && valid;
        }

        public void invalidate() {
            valid = false;
        }

        public void init(IGraphicsTarget target) {
            if (pointData == null)
                retrieveAndDecodeData();

            PlotModelFactory2 pmf;

            synchronized (job) {
                plotSettingsChanged = false;
                pmf = getPlotModelFactory();
                int actualPlotWidth = pmf.getDefinedPlotModelWidth();
                Double magnification = getCapability(
                        MagnificationCapability.class).getMagnification();
                if (plotWidth != actualPlotWidth * magnification) {
                    plotWidth = actualPlotWidth * magnification;
                    long width = Math.round(plotWidth);
                    pmf.setPlotDimensions(width, width);
                }
            }

            synchronized (this) {
                if (pointData != null) {
                    images = new IImage[pointData.getCurrentSz()];
                    for (int i = 0; i < pointData.getCurrentSz(); ++i) {
                        PointDataView pdv = pointData.readRandom(i);
                        float lat = pdv
                                .getFloat(RedbookUpperAirDecoder.P_LATITUDE);
                        float lon = pdv
                                .getFloat(RedbookUpperAirDecoder.P_LONGITUDE);
                        PlotData pd = new PlotData();
                        pd.addData(pdv);
                        BufferedImage bImage = pmf.getStationPlot(pd, lat, lon);
                        IImage image = null;
                        if (bImage != null)
                            image = target.initializeRaster(new IODataPreparer(
                                    bImage, "rbua"/*
                                                   * UUID.randomUUID().toString()
                                                   */, 0), null);
                        images[i] = image;
                    }
                }
            }

            synchronized (job) {
                if (!plotSettingsChanged)
                    valid = true;
            }
        }

        public void dispose() {
            synchronized (this) {
                if (images != null)
                    for (int i = 0; i < images.length; ++i) {
                        if (images[i] != null) {
                            images[i].dispose();
                            images[i] = null;
                        }
                    }
                valid = false;
                redbookRecord = null;
                pointData = null;
            }
        }

        public void paint(IGraphicsTarget target, PaintProperties paintProps)
                throws VizException {
            if (pointData == null || images == null)
                return;

            IExtent pe = paintProps.getView().getExtent();

            double xRatio = paintProps.getView().getExtent().getWidth()
                    / paintProps.getCanvasBounds().width;
            double yRatio = paintProps.getView().getExtent().getHeight()
                    / paintProps.getCanvasBounds().height;
            double scaleValue = (plotWidth / 2.0) * xRatio;

            double paintZoomLevel = paintProps.getZoomLevel();
            double density = getCapability(DensityCapability.class)
                    .getDensity();
            ;
            double netZoom = (1 / paintZoomLevel)
                    * density
                    / getCapability(MagnificationCapability.class)
                            .getMagnification();

            int threshold;

            if (netZoom <= 0.8)
                threshold = 0;
            else if (netZoom <= 1.5)
                threshold = 1;
            else if (netZoom <= 2.5)
                threshold = 2;
            else
                threshold = 3;

            for (int i = 0; i < pointData.getCurrentSz(); ++i) {
                if (images[i] == null)
                    continue;

                PointDataView pdv = pointData.readRandom(i);

                int zoomLevel = pdv.getInt(RedbookUpperAirDecoder.P_ZOOM_LEVEL);
                if (zoomLevel > threshold)
                    continue;

                double[] worldCoord = new double[] {
                        pdv.getFloat(RedbookUpperAirDecoder.P_LONGITUDE),
                        pdv.getFloat(RedbookUpperAirDecoder.P_LATITUDE) };
                double[] pixelCoord = getDescriptor().worldToPixel(worldCoord);
                double[] stationPixelLocation = pixelCoord;

                double[] ul = new double[] {
                        stationPixelLocation[0] - scaleValue,
                        stationPixelLocation[1] - scaleValue, 0 };

                double[] ur = new double[] {
                        stationPixelLocation[0] + scaleValue,
                        stationPixelLocation[1] - scaleValue, 0 };

                double[] lr = new double[] {
                        stationPixelLocation[0] + scaleValue,
                        stationPixelLocation[1] + scaleValue, 0 };

                double[] ll = new double[] {
                        stationPixelLocation[0] - scaleValue,
                        stationPixelLocation[1] + scaleValue, 0 };

                if (ul[0] > pe.getMaxX() || ul[1] > pe.getMaxY()
                        || lr[0] < pe.getMinX() || lr[1] < pe.getMinY())
                    continue;

                PixelCoverage pc = new PixelCoverage(new Coordinate(ul[0],
                        ul[1], ul[2]), new Coordinate(ur[0], ur[1], ur[2]),
                        new Coordinate(lr[0], lr[1], lr[2]), new Coordinate(
                                ll[0], ll[1], ll[2]));

                target.drawRaster(images[i], pc, paintProps,
                        RasterMode.SYNCHRONOUS);

            }

            if (dumpTime != null && dumpTime.length() > 0) {
                target.clearClippingPlane();
                target.drawString(font, "Dump time: " + dumpTime, pe.getMinX()
                        + 2 * xRatio, pe.getMaxY() - 2 * yRatio, 0,
                        TextStyle.NORMAL,
                        getCapability(ColorableCapability.class).getColor(),
                        HorizontalAlignment.LEFT, VerticalAlignment.BOTTOM, 0.0);
                target.setupClippingPlane(new PixelExtent(getDescriptor()
                        .getGridGeometry().getGridRange()));
            }
        }

        private void retrieveAndDecodeData() {
            // Set fallback values in case of failure
            dumpTime = "";
            pointData = new PointDataContainer();

            if (redbookRecord != null) {
                File hdf5Loc = HDF5Util.findHDF5Location(redbookRecord);

                IDataStore ds = DataStoreFactory.getDataStore(hdf5Loc);
                IDataRecord dr;
                try {
                    dr = ds.retrieve(redbookRecord.getDataURI(), "redbookData",
                            Request.ALL);
                } catch (Exception e) {
                    statusHandler.handle(Priority.SIGNIFICANT,
                            "Unable to retrieve redbook data from repository",
                            e);
                    return;
                }

                if (!(dr instanceof ByteDataRecord)) {
                    statusHandler.handle(Priority.SIGNIFICANT,
                            "Expected to find byteData in repository, but found "
                                    + dr);
                    return;
                }

                RedbookUpperAirDecoder decoder = new RedbookUpperAirDecoder();
                decoder.decode(((ByteDataRecord) dr).getByteData());
                dumpTime = decoder.getDumpTime();
                pointData = decoder.getPointData();
                redbookRecord = null; // The record is no longer needed.
            }
        }
    }

    private class LoadAndRenderJob extends Job {

        public LoadAndRenderJob() {
            super("Creating Plots...");
        }

        public void upadte() {
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    schedule();
                }
            });
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            UAFrame frame = frames.get(displayedDataTime);
            if (frame != null) {
                if (!frame.isReadyToPaint()) {
                    frame.init(graphicsTarget);
                    issueRefresh();
                }
            }
            return Status.OK_STATUS;
        }

    }
}
