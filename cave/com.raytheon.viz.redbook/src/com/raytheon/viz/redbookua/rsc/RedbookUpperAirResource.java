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
import java.awt.image.RenderedImage;
import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.Validate;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.redbook.RedbookRecord;
import com.raytheon.uf.common.dataplugin.redbook.RedbookWMOMap;
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
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.RasterMode;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
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
import com.raytheon.viz.pointdata.IPlotModelFactory;
import com.raytheon.viz.pointdata.PlotData;
import com.raytheon.viz.pointdata.PlotModelFactory;
import com.raytheon.viz.pointdata.PlotModelFactoryDefault;
import com.raytheon.viz.redbookua.RedbookUpperAirDecoder;
import org.locationtech.jts.geom.Coordinate;

/**
 * Resource data for redbook
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 24, 2010  1029     dfriedma  Initial creation
 * Jul 24, 2013  2203     njensen   Synchronized init and dispose of frames
 * Mar 13, 2014  2907     njensen   split edex.redbook plugin into common and
 *                                  edex redbook plugins
 * Jul 29, 2014  3465     mapeters  Updated deprecated drawString() calls.
 * Aug 11, 2014  3504     mapeters  Replaced deprecated IODataPreparer instances
 *                                  with IRenderedImageCallback.
 * Jun 26, 2015  4512     mapeters  Updated for RedbookWMOMap API changes.
 * Oct 27, 2015  4798     bsteffen  Handle VizException for missing svg.
 * Nov 05, 2015  5070     randerso  Adjust font sizes for dpi scaling
 * Dec 03, 2015  5143     kbisanz   Remove unneeded setting of DataTime utility
 *                                  flag in getName() to prevent NPE in case of
 *                                  no data.
 * Nov 08, 2016  5976     bsteffen  Remove VizApp logging
 * Nov 28, 2017  5863     bsteffen  Change dataTimes to a NavigableSet
 * Nov 01, 2019  71272    ksunil    tweaks to accommodate new plot
 *                                     customization changes
 *
 * </pre>
 *
 * @author dfriedma
 */
public class RedbookUpperAirResource
        extends AbstractVizResource<RedbookUpperAirResourceData, MapDescriptor>
        implements IResourceDataChanged {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RedbookUpperAirResource.class);

    private static final String REDBOOK_SVG = "redbookuaDesign.svg";

    private final Map<DataTime, UAFrame> frames = new HashMap<>();

    private DataTime displayedDataTime;

    private IFont font;

    private String humanReadableName;

    private IPlotModelFactory plotModelFactory;

    private IGraphicsTarget graphicsTarget;

    private double plotWidth;

    private boolean plotSettingsChanged = true;

    private LoadAndRenderJob job = new LoadAndRenderJob();

    protected RedbookUpperAirResource(RedbookUpperAirResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties, false);
        resourceData.addChangeListener(this);
    }

    @Override
    protected void disposeInternal() {
        for (UAFrame frame : this.frames.values()) {
            frame.dispose();
        }

        if (font != null) {
            font.dispose();
        }
    }

    @Override
    public String getName() {
        if (this.humanReadableName == null) {
            buildHumanReadableName();
        }

        return this.humanReadableName;
    }

    private void buildHumanReadableName() {
        this.humanReadableName = "Redbook Resource";

        RequestConstraint wmo = this.resourceData.getMetadataMap()
                .get("wmoTTAAii");
        if (wmo != null) {
            RedbookWMOMap map;
            try {
                map = RedbookWMOMap.load();
            } catch (Exception e) {
                statusHandler.error("Error loading redbook mapping", e);
                return;
            }
            RedbookWMOMap.Info info = map.getValue(wmo.getConstraintValue());
            if (info != null && info.name != null) {
                this.humanReadableName = info.name;
            }
        }
    }

    @Override
    public void setDescriptor(MapDescriptor descriptor) {
        super.setDescriptor(descriptor);
        synchronized (job) {
            plotModelFactory = null;
            plotWidth = 0;
            invalidateAll();
            plotSettingsChanged = true;
        }
    }

    @Override
    public DataTime[] getDataTimes() {
        Set<DataTime> dataTimeSet = frames.keySet();
        DataTime[] dataTimes = dataTimeSet
                .toArray(new DataTime[dataTimeSet.size()]);
        return dataTimes;
    }

    @Override
    public void remove(DataTime dataTime) {
        dataTimes.remove(dataTime);
        UAFrame frame = frames.remove(dataTime);
        if (frame != null) {
            frame.dispose();
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        this.font = target.initializeFont("Monospace", 8, null);
        this.graphicsTarget = target;
    }

    protected IPlotModelFactory getPlotModelFactory() throws VizException {
        if (plotModelFactory == null) {

            if (PlotModelFactory.isNewSVGFormat(REDBOOK_SVG)) {
                plotModelFactory = new PlotModelFactory(getDescriptor(),
                        REDBOOK_SVG);
            } else {
                plotModelFactory = new PlotModelFactoryDefault(getDescriptor(),
                        REDBOOK_SVG);
            }
            plotModelFactory.setColor(
                    getCapability(ColorableCapability.class).getColor());
            plotModelFactory.setUpperLimit(10_000);
        }
        return plotModelFactory;
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        this.displayedDataTime = paintProps.getDataTime();

        UAFrame frame = frames.get(displayedDataTime);
        if (frame != null) {

            if (frame.isReadyToPaint()) {
                frame.paint(target, paintProps);
            } else {
                job.upadte();
            }
        }
    }

    private void invalidateAll() {
        synchronized (job) {
            for (UAFrame frame : frames.values()) {
                frame.invalidate();
            }
        }
    }

    public IFont getRenderingFont() {
        return this.font;
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        super.project(mapData);
        setDescriptor(getDescriptor());
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            Validate.isTrue(object instanceof PluginDataObject[], "Expected a "
                    + PluginDataObject[].class + ", Got: " + object.getClass());

            PluginDataObject[] pdos = (PluginDataObject[]) object;
            for (PluginDataObject record : pdos) {
                Validate.notNull(record, "PluginDataObject was null");
                Validate.isTrue(record instanceof RedbookRecord,
                        "RedbookResource expects RedbookRecords, got "
                                + record);

                addRecord((RedbookRecord) record);
            }
        } else if (type.equals(ChangeType.CAPABILITY)) {
            if (object instanceof ColorableCapability) {
                synchronized (job) {
                    if (plotModelFactory != null) {
                        plotModelFactory.setColor(
                                ((ColorableCapability) object).getColor());
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
        frames.put(dataTime, new UAFrame(record));
        dataTimes.add(record.getDataTime());
    }

    private class UAFrame {
        private RedbookRecord redbookRecord;

        public String dumpTime;

        public PointDataContainer pointData;

        public IImage[] images;

        private boolean valid;

        public UAFrame(RedbookRecord record) {
            this.redbookRecord = record;
        }

        public boolean isReadyToPaint() {
            return pointData != null && valid;
        }

        public void invalidate() {
            valid = false;
        }

        public void init(IGraphicsTarget target) {
            if (pointData == null) {
                retrieveAndDecodeData();
            }

            IPlotModelFactory pmf;

            synchronized (job) {
                plotSettingsChanged = false;
                try {
                    pmf = getPlotModelFactory();
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                    return;
                }
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
                        final BufferedImage bImage = pmf.getStationPlot(pd, lat,
                                lon);
                        IImage image = null;
                        if (bImage != null) {
                            image = target.initializeRaster(
                                    new IRenderedImageCallback() {
                                        @Override
                                        public RenderedImage getImage()
                                                throws VizException {
                                            return bImage;
                                        }
                                    });
                        }
                        images[i] = image;
                    }
                }
            }

            synchronized (job) {
                if (!plotSettingsChanged) {
                    valid = true;
                }
            }
        }

        public void dispose() {
            synchronized (this) {
                if (images != null) {
                    for (int i = 0; i < images.length; ++i) {
                        if (images[i] != null) {
                            images[i].dispose();
                            images[i] = null;
                        }
                    }
                }
                valid = false;
                redbookRecord = null;
                pointData = null;
            }
        }

        public void paint(IGraphicsTarget target, PaintProperties paintProps)
                throws VizException {
            if (pointData == null || images == null) {
                return;
            }

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
            double netZoom = (1 / paintZoomLevel) * density
                    / getCapability(MagnificationCapability.class)
                            .getMagnification();

            int threshold;

            if (netZoom <= 0.8) {
                threshold = 0;
            } else if (netZoom <= 1.5) {
                threshold = 1;
            } else if (netZoom <= 2.5) {
                threshold = 2;
            } else {
                threshold = 3;
            }

            for (int i = 0; i < pointData.getCurrentSz(); ++i) {
                if (images[i] == null) {
                    continue;
                }

                PointDataView pdv = pointData.readRandom(i);

                int zoomLevel = pdv.getInt(RedbookUpperAirDecoder.P_ZOOM_LEVEL);
                if (zoomLevel > threshold) {
                    continue;
                }

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
                        || lr[0] < pe.getMinX() || lr[1] < pe.getMinY()) {
                    continue;
                }

                PixelCoverage pc = new PixelCoverage(
                        new Coordinate(ul[0], ul[1], ul[2]),
                        new Coordinate(ur[0], ur[1], ur[2]),
                        new Coordinate(lr[0], lr[1], lr[2]),
                        new Coordinate(ll[0], ll[1], ll[2]));

                target.drawRaster(images[i], pc, paintProps,
                        RasterMode.SYNCHRONOUS);

            }

            if (dumpTime != null && dumpTime.length() > 0) {
                target.clearClippingPlane();

                DrawableString string = new DrawableString(
                        "Dump time: " + dumpTime,
                        getCapability(ColorableCapability.class).getColor());
                string.font = font;
                string.setCoordinates(pe.getMinX() + 2 * xRatio,
                        pe.getMaxY() - 2 * yRatio);
                target.drawStrings(string);

                target.setupClippingPlane(new PixelExtent(
                        getDescriptor().getGridGeometry().getGridRange()));
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
                // The record is no longer needed.
                redbookRecord = null;
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
