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

package com.raytheon.viz.pointdata.rsc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.Validate;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.pointdata.vadriver.VA_Advanced;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.IPlotDataResource;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.pointdata.PlotModelGenerator;
import com.raytheon.viz.pointdata.StaticPlotInfoPV;
import com.raytheon.viz.pointdata.StaticPlotInfoPV.SPIEntry;
import com.raytheon.viz.pointdata.drawables.IPointImageExtension;
import com.raytheon.viz.pointdata.drawables.IPointImageExtension.PointImage;
import com.raytheon.viz.pointdata.units.PlotUnits;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Provides a resource that will display plot data for a given reference time.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  11/20/2006             brockwoo    Initial creation.
 *  02/17/2009             njensen     Refactored to new rsc architecture.
 *  03/17/2009      2105   jsanchez    Plot goessounding/poessounding availability.
 *  03/30/2009      2169   jsanchez    Updated initNewFrame.
 *  04/09/2009       952   jsanchez    Plot acars.   
 *  04/13/2009      2251   jsanchez    Plot profilers.
 *  05/12/2009      2338   jsanchez    Updated resourceChanged. Registered units.
 *  06/08/2009      2450   jsanchez    Updated inpsect method to find closest plot.
 * 
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class PlotResource extends
        AbstractVizResource<PlotResourceData, MapDescriptor> implements
        IPlotDataResource, IResourceDataChanged {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PlotResource.class);

    private boolean hasInited = false;

    private PixelExtent worldExtent;

    private PlotModelGenerator generator;

    private DataTime displayedObsTime;

    private boolean metarEnabled;

    private boolean mesowestEnabled;

    private VA_Advanced progDisc;

    private double actualPlotWidth;

    private double plotWidth;

    private static final double MAX_SAMPLE_DISANCE = 2;

    private HashMap<String, FrameInformation> frameInfo;

    private ArrayList<PluginDataObject> stationsToParse;

    HashMap<String, RequestConstraint> queryList;

    private StaticPlotInfoPV spi;

    // private int range;

    private int dynProgDisc;

    private double distFloor;

    private double screenToWorldRatio;

    private static final String PLUGIN_GOESSOUNDING = "goessounding";

    private static final String PLUGIN_POESSOUNDING = "poessounding";

    private static final String PLUGIN_ACARSSOUNDING = "acarssounding";

    private static final String PLUGIN_PROFILER = "profiler";

    private static final String PLUGIN_ACARS = "acars";

    private static final String PLUGIN_PIREP = "pirep";

    public class FrameInformation {
        boolean computeProgDisc = true;

        int uniqueueStations = 0;

        int dynStations = 0;

        double minDist = 22222.0;

        ArrayList<Double> frameDistValues;

        ArrayList<Double> origFrameDistValues;

        ArrayList<String> frameDataUris;

        ArrayList<Coordinate> frameStationLocations;

        ArrayList<Coordinate> frameStationPixelLocations;

        ArrayList<Integer> frameGoodnessValues;

        HashMap<String, Integer> frameStations;

        ArrayList<Integer> currentStationList = new ArrayList<Integer>();
    }

    public class ObsInformation {
        String id;

        String icao;

        String datauri;

        Coordinate latLon;
    }

    /**
     * Create a surface plot resource.
     * 
     * @param target
     *            The graphic target to draw to
     * @param refTime
     *            The reference time to request data against
     * @throws VizException
     */
    public PlotResource(PlotResourceData data, LoadProperties props) {
        super(data, props);
        this.dataTimes = new ArrayList<DataTime>();
        metarEnabled = true;
        mesowestEnabled = true;
        this.progDisc = new VA_Advanced();
        this.stationsToParse = new ArrayList<PluginDataObject>();
        this.frameInfo = new HashMap<String, FrameInformation>();
        this.dynProgDisc = 1;

        if (!hasCapability(ColorableCapability.class)) {
            // default
            getCapability(ColorableCapability.class).setColor(
                    new RGB(0, 255, 0));
            // this.range = resourceData.getBinOffset().getInterval() * 1000;
        }
        data.addChangeListener(this);
        if (resourceData.getSpiFile() != null) {
            this.spi = StaticPlotInfoPV.readStaticPlotInfoPV(resourceData
                    .getSpiFile());
        }
    }

    @Override
    public String getName() {
        return this.resourceData.getPlotSource();
    }

    @Override
    protected void paintInternal(IGraphicsTarget aTarget,
            PaintProperties paintProps) throws VizException {
        if (!hasInited) {
            return;
        }

        displayedObsTime = paintProps.getDataTime();
        if (this.stationsToParse.size() > 0) {
            this.updateRecords();
        }
        boolean delayLoop = false;
        DataTime thisFrameTime = this.displayedObsTime;
        if (thisFrameTime != null) {
            String thisFrameTimeString = thisFrameTime.toString();
            if (!this.frameInfo.containsKey(thisFrameTimeString)) {
                this.initNewFrame(thisFrameTime);
                delayLoop = paintProps.getLoopProperties().isLooping();
            }

            FrameInformation thisFrameInfo = this.frameInfo
                    .get(thisFrameTimeString);

            IExtent extent = paintProps.getView().getExtent();

            double maxX = extent.getMaxX();
            double minX = extent.getMinX();
            double maxY = extent.getMaxY();
            double minY = extent.getMinY();
            if (maxX < 0) {
                maxX = 0;
            }
            if (maxX > 19999) {
                maxX = 19999;
            }
            if (minX < 0) {
                minX = 0;
            }
            if (minX > 19999) {
                minX = 19999;
            }
            if (maxY < 0) {
                maxY = 0;
            }
            if (maxY > 9999) {
                maxY = 9999;
            }
            if (minY < 0) {
                minY = 0;
            }
            if (minY > 9999) {
                minY = 9999;
            }

            PixelExtent correctedExtent = new PixelExtent(minX, maxX, minY,
                    maxY);

            double magnification = getCapability(MagnificationCapability.class)
                    .getMagnification();

            int displayWidth = (int) (descriptor.getMapWidth() * paintProps
                    .getZoomLevel());
            double kmPerPixel = (displayWidth / paintProps.getCanvasBounds().width) / 1000.0;
            if (this.plotWidth != this.actualPlotWidth * magnification) {
                this.plotWidth = this.actualPlotWidth * magnification;
                this.generator.setPlotModelSize(Math.round(this.plotWidth));
            }
            double displayHintSize = this.resourceData.getPixelSizeHint()
                    * magnification;
            double threshold = (displayHintSize * kmPerPixel)
                    / getCapability(DensityCapability.class).getDensity();

            // Load Information about current frame
            ArrayList<Double> currentDistValues = thisFrameInfo.frameDistValues;
            ArrayList<String> currentDataUris = thisFrameInfo.frameDataUris;
            ArrayList<Coordinate> currentFrameStationLocations = thisFrameInfo.frameStationLocations;
            ArrayList<Coordinate> currentFrameStationPixels = thisFrameInfo.frameStationPixelLocations;

            ArrayList<Integer> stationList = new ArrayList<Integer>();

            for (int counter = 0; counter < currentDistValues.size(); counter++) {
                Coordinate location = currentFrameStationPixels.get(counter);
                if (!correctedExtent.contains(location.x, location.y)) {
                    continue;
                }
                if (currentDistValues.get(counter) >= threshold) {
                    stationList.add(counter);
                }
            }
            thisFrameInfo.currentStationList = stationList;
            ArrayList<String> newStations = new ArrayList<String>();
            int queuedStations = 0;
            long paintStart = System.currentTimeMillis();
            do {
                queuedStations = 0;
                for (int station : stationList) {
                    String currentDataUri = currentDataUris.get(station);
                    double[] stationLocation = {
                            currentFrameStationLocations.get(station).x,
                            currentFrameStationLocations.get(station).y };
                    double[] stationPixelLocation = descriptor
                            .worldToPixel(stationLocation);
                    if (!generator.hasImage(currentDataUri)) {
                        queuedStations++;
                        if (!generator.isQueued(currentDataUri)) {
                            newStations.add(currentDataUri);
                        }
                        continue;
                    }

                    PointImage image = new PointImage(
                            generator.getStation(currentDataUri),
                            stationPixelLocation[0], stationPixelLocation[1]);
                    image.setHeight(this.plotWidth);
                    image.setWidth(this.plotWidth);
                    aTarget.getExtension(IPointImageExtension.class)
                            .drawPointImages(paintProps, image);
                }
                if (newStations.size() > 0) {
                    generator.queueStations(newStations, paintProps
                            .getLoopProperties().isLooping());
                    newStations.clear();
                }
                if ((delayLoop && queuedStations == 0)
                        || (System.currentTimeMillis() - paintStart > 5000)) {
                    delayLoop = false;
                    queuedStations = -9999;
                }
            } while ((queuedStations > 0 && delayLoop));
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget aTarget) throws VizException {
        if (!hasInited) {
            Map<String, RequestConstraint> request = resourceData
                    .getMetadataMap();
            RequestConstraint plugin = request.get("pluginName");
            PlotUnits.register();
            generator = new PlotModelGenerator(aTarget, descriptor,
                    this.resourceData.getPlotModelFile(),
                    plugin.getConstraintValue());
            this.generator.setPlotModelColor(getCapability(
                    ColorableCapability.class).getColor());
            this.generator.setPlotModelLineStyle(getCapability(
                    OutlineCapability.class).getLineStyle());
            this.generator.setPlotModelLineWidth(getCapability(
                    OutlineCapability.class).getOutlineWidth());
            // generator.schedule();
            this.actualPlotWidth = this.plotWidth = generator
                    .getPlotModelWidth();
            this.distFloor = (descriptor.getMapWidth() / 1000.0)
                    * this.resourceData.getPixelSizeHint() / 32000.0;
            this.worldExtent = new PixelExtent(0, descriptor.getGridGeometry()
                    .getGridRange().getHigh(0), 0, descriptor.getGridGeometry()
                    .getGridRange().getHigh(1));
            hasInited = true;
            getCapability(MagnificationCapability.class);
            getCapability(DensityCapability.class);
        }
    }

    protected synchronized void updateRecords() throws VizException {
        if (this.stationsToParse.size() > 0) {
            FrameInformation targetFrame = null;
            for (PluginDataObject ob : this.stationsToParse) {
                DataTime normalized = this.getNormalizedTime(ob.getDataTime());
                if (!this.dataTimes.contains(normalized)) {
                    this.dataTimes.add(normalized);
                } else if ((targetFrame = this.frameInfo.get(normalized
                        .toString())) != null) {
                    ObsInformation station = this.parseDataUri(ob.getDataURI(),
                            String.valueOf(ob.getId()));
                    Integer stationIndex = null;
                    if ((stationIndex = targetFrame.frameStations
                            .get(station.icao)) != null) {
                        PluginDataObject storedOb = this.generator
                                .getStationObject(targetFrame.frameDataUris
                                        .get(stationIndex));
                        if (storedOb == null) {
                            targetFrame.frameDataUris.set(stationIndex,
                                    ob.getDataURI());
                        } else if (storedOb.getDataTime().compareTo(
                                ob.getDataTime()) < 1) {
                            targetFrame.frameDataUris.set(stationIndex,
                                    ob.getDataURI());
                        }
                    } else {
                        // new station!
                        this.calcStaticStationInfo(station, targetFrame);
                        this.generator.setStation(ob.getDataURI());
                        this.calculateProgDisc(targetFrame);
                    }

                }
            }
            this.stationsToParse.clear();
        }
    }

    public void addRecord(PluginDataObject record) throws VizException {
        Validate.notNull(record);
        // Validate.isTrue(record instanceof MetarRecord);
        // this.recordsToParse.add((MetarRecord) record);
        this.stationsToParse.add(record);

    }

    private DataTime getNormalizedTime(DataTime time) {
        // long millis = time.getValidTime().getTimeInMillis();
        // millis -= this.resourceData.getBinOffset().negOffset;
        // millis = ((millis / this.range) * this.range) + this.range;
        // return new DataTime(new Date(millis));

        return this.resourceData.getBinOffset().getNormalizedTime(time);
    }

    @Override
    protected void disposeInternal() {
        generator.shutdown();
    }

    /**
     * Returns if mesowest data is being displayed by the layer .
     * 
     * @return Whether mesowest is enabled or not
     */
    public boolean isMesowestEnabled() {
        return mesowestEnabled;
    }

    /**
     * Returns if metar data is being displayed by the layer.
     * 
     * @return Whether metar is enabled or not
     */
    public boolean isMetarEnabled() {
        return metarEnabled;
    }

    /**
     * Enables/disables mesowest data.
     * 
     * @param flag
     *            Enable or disable mesowest on the display
     */
    public void setMesowestMode(boolean flag) {
        mesowestEnabled = flag;
        // aTarget.setNeedsRefresh(true);
    }

    /**
     * Enables/disables metar data.
     * 
     * @param flag
     *            Enable or disable metar on the display
     */
    public void setMetarMode(boolean flag) {
        metarEnabled = flag;
        // aTarget.setNeedsRefresh(true);
    }

    @Override
    public DataTime[] getDataTimes() {
        try {
            if (this.dataTimes == null || this.dataTimes.size() < 1) {
                this.initDataTimeArray();
            }
            if (this.stationsToParse.size() > 0) {
                this.updateRecords();
            }
        } catch (VizException e) {
            e.printStackTrace();
        }
        Collections.sort(this.dataTimes);
        DataTime[] returnTimes = new DataTime[this.dataTimes.size()];
        return this.dataTimes.toArray(returnTimes);
    }

    public void setDynProgDisc(String progDisc) {
        if ("none".matches(progDisc)) {
            this.dynProgDisc = 0;
        } else if ("missing".matches(progDisc)) {
            this.dynProgDisc = 2;
        } else if ("all".matches(progDisc)) {
            this.dynProgDisc = 3;
        } else if ("off".matches(progDisc)) {
            this.dynProgDisc = 4;
        }
    }

    public String getDynProgDisc() {
        switch (this.dynProgDisc) {
        case 0:
            return "none";
        case 2:
            return "missing";
        case 3:
            return "all";
        case 4:
            return "off";
        }
        return null;
    }

    private void initDataTimeArray() throws VizException {
        HashMap<String, RequestConstraint> queryParams = new HashMap<String, RequestConstraint>(
                resourceData.getMetadataMap());
        DataTime[] times = CatalogQuery.performTimeQuery(queryParams, false,
                this.resourceData.getBinOffset());
        this.dataTimes = new ArrayList<DataTime>(Arrays.asList(times));
    }

    private void calculateProgDisc(FrameInformation fi) {
        // long startProcess = System.currentTimeMillis();
        boolean progressiveDisclosure = fi.dynStations > 0
                && ((fi.uniqueueStations * fi.uniqueueStations / fi.dynStations) < 3000);
        progressiveDisclosure = this.dynProgDisc > 1
                || (progressiveDisclosure && this.dynProgDisc > 0);

        if (progressiveDisclosure) {
            Coordinate[] latLonArray = fi.frameStationLocations
                    .toArray(new Coordinate[fi.frameStationLocations.size()]);
            Integer[] goodnessArray = fi.frameGoodnessValues
                    .toArray(new Integer[fi.frameGoodnessValues.size()]);
            Double[] distArray = fi.origFrameDistValues
                    .toArray(new Double[fi.origFrameDistValues.size()]);
            this.progDisc.setVaJustGoodness(false);
            this.progDisc.setVaDistPass(this.dynProgDisc < 3);
            this.progDisc.getVaAdvanced(latLonArray, goodnessArray, distArray);
            fi.frameDistValues = new ArrayList<Double>(Arrays.asList(distArray));
        } else {
            fi.frameDistValues = new ArrayList<Double>(fi.origFrameDistValues);
            for (int i = 0; i < fi.uniqueueStations; i++) {
                if (fi.frameDistValues.get(i) < 0) {
                    fi.frameDistValues.set(i, fi.minDist);
                }
            }
        }
        fi.computeProgDisc = false;
        // System.out.println("Total time to perform progressive disclosure: " +
        // (System.currentTimeMillis() - startProcess));

    }

    private void initNewFrame(DataTime thisFrameTime) throws VizException {
        boolean useId = false;
        String thisFrameTimeString = thisFrameTime.toString();
        HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>(
                resourceData.getMetadataMap());

        // long frameMillis = thisFrameTime.getValidTime().getTimeInMillis();
        // DataTime start = new DataTime(new Date(frameMillis
        // - this.resourceData.getBinOffset().negOffset * 1000 + 1));
        // DataTime end = new DataTime(new Date(frameMillis
        // + (this.resourceData.getBinOffset().posOffset * 1000)));

        TimeRange tr = this.resourceData.getBinOffset().getTimeRange(
                thisFrameTime);
        DataTime start = new DataTime(tr.getStart());
        DataTime end = new DataTime(tr.getEnd());

        RequestConstraint timeRange = new RequestConstraint();
        String[] constraintList = { start.toString(), end.toString() };
        timeRange.setBetweenValueList(constraintList);
        timeRange.setConstraintType(RequestConstraint.ConstraintType.BETWEEN);

        queryList.put("dataTime", timeRange);

        DbQueryRequest request = new DbQueryRequest();
        request.setConstraints(queryList);
        request.addFields(new String[] { "dataURI", "id" });
        DbQueryResponse response = (DbQueryResponse) ThriftClient
                .sendRequest(request);

        // extract list of results
        List<Map<String, Object>> responseList = null;
        if (response != null) {
            responseList = response.getResults();
        } else {
            // empty list to simplify code
            responseList = new ArrayList<Map<String, Object>>(0);
        }

        if (responseList.size() > 0) {
            Object dURI = responseList.get(0).get("dataURI");
            if (dURI != null && dURI instanceof String) {
                String firstDataURI = (String) dURI;
                String pluginName = (firstDataURI.split("/"))[1];
                if (pluginName.equals(PLUGIN_GOESSOUNDING)
                        || pluginName.equals(PLUGIN_POESSOUNDING)
                        || pluginName.equals(PLUGIN_ACARSSOUNDING)
                        || pluginName.equals(PLUGIN_ACARS)
                        || pluginName.equals(PLUGIN_PIREP)
                        || pluginName.equals(PLUGIN_PROFILER)) {
                    useId = true;
                }
            } else {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Unexpected type/value for dataURI expected String and got "
                                + dURI == null ? "null" : dURI.getClass()
                                .toString(), new Exception());
            }
        }
        queryList = null;

        FrameInformation thisFrameInfo = new FrameInformation();
        thisFrameInfo.uniqueueStations = 0;
        thisFrameInfo.dynStations = 0;
        thisFrameInfo.minDist = 22222.0;
        thisFrameInfo.frameStationLocations = new ArrayList<Coordinate>();
        thisFrameInfo.frameStationPixelLocations = new ArrayList<Coordinate>();
        thisFrameInfo.frameDataUris = new ArrayList<String>();
        thisFrameInfo.frameDistValues = new ArrayList<Double>();
        thisFrameInfo.origFrameDistValues = new ArrayList<Double>();
        thisFrameInfo.frameStations = new HashMap<String, Integer>();
        thisFrameInfo.frameGoodnessValues = new ArrayList<Integer>();

        HashMap<String, ObsInformation> dataUris = new HashMap<String, ObsInformation>();
        String dataUri = null;
        String id = null;

        Iterator<Map<String, Object>> iter = responseList.iterator();
        List<String> errors = new ArrayList<String>(0);
        while (iter.hasNext()) {
            Map<String, Object> result = iter.next();
            Object dURI = result.get("dataURI");
            // check if dataURI exists and is a string
            if (dURI != null && dURI instanceof String) {
                dataUri = (String) dURI;
                Object tmpID = result.get("id");
                id = null;
                // if we need to use the id check that id is valid ( otherwise
                // we leave null )
                if (useId && tmpID != null && tmpID instanceof Integer) {
                    id = ((Integer) tmpID).toString();
                } else if (useId) {
                    errors.add("Unexpected type/value for id expected Integer and got "
                            + tmpID == null ? "null" : tmpID.getClass()
                            .toString());
                }
                ObsInformation station = this.parseDataUri(dataUri, id);
                dataUris.put(station.icao, station);
            } else {
                errors.add("Unexpected type/value for dataURI expected String and got "
                        + dURI == null ? "null" : dURI.getClass().toString());
            }
        }

        // display errors if there are any
        if (errors.size() > 0) {
            Iterator<String> errorIterator = errors.iterator();
            String errorMessage = "";
            while (errorIterator.hasNext()) {
                errorMessage += errorIterator.next();
            }
            statusHandler.handle(Priority.PROBLEM, errorMessage,
                    new Exception());
        }

        Set<String> stationNames = dataUris.keySet();
        // distFloor = (this.theMapDescriptor.getMapWidth() / 1000.0) *
        // this.pixelSizeHint / 32000.0;
        int usedStations = 0;
        int allStations = 0;
        for (String station : stationNames) {
            allStations++;
            if (this.calcStaticStationInfo(dataUris.get(station), thisFrameInfo)) {
                usedStations++;
            }
        }
        this.calculateProgDisc(thisFrameInfo);
        this.frameInfo.put(thisFrameTimeString, thisFrameInfo);
        // System.out.println("Total time to process frame: " +
        // (System.currentTimeMillis() - startProcess));
    }

    private boolean calcStaticStationInfo(ObsInformation oi, FrameInformation fi) {
        SPIEntry obsStation = null;
        Coordinate thisLocation = null;
        Coordinate thisPixelLocation = null;
        if (spi != null) {
            obsStation = spi.getSPIEntry(oi.icao);
        }
        if (obsStation != null) {
            thisLocation = obsStation.latlon;
            double[] thisLocationLatLon = { thisLocation.x, thisLocation.y };
            double[] thisLocationPixel = descriptor
                    .worldToPixel(thisLocationLatLon);
            if (!this.worldExtent.contains(thisLocationPixel[0],
                    thisLocationPixel[1])) {
                return false;
            }
            thisPixelLocation = new Coordinate(thisLocationPixel[0],
                    thisLocationPixel[1]);
            if (obsStation.distance < distFloor) {
                fi.minDist = distFloor;
                fi.origFrameDistValues.add(fi.minDist);
            } else {
                fi.origFrameDistValues.add(obsStation.distance);
                if (obsStation.distance < fi.minDist) {
                    fi.minDist = obsStation.distance;
                }
            }
        } else {
            thisLocation = oi.latLon;
            double[] thisLocationLatLon = { thisLocation.x, thisLocation.y };
            double[] thisLocationPixel = descriptor
                    .worldToPixel(thisLocationLatLon);
            if (thisLocationPixel == null
                    || !this.worldExtent.contains(thisLocationPixel[0],
                            thisLocationPixel[1])) {
                return false;
            }
            thisPixelLocation = new Coordinate(thisLocationPixel[0],
                    thisLocationPixel[1]);
            fi.origFrameDistValues.add(-1.0);
            fi.dynStations++;
        }
        fi.frameStationPixelLocations.add(thisPixelLocation);
        fi.frameStationLocations.add(thisLocation);
        fi.frameDataUris.add(oi.datauri);
        fi.frameStations.put(oi.icao, fi.uniqueueStations);
        fi.frameGoodnessValues.add(0);
        fi.uniqueueStations++;
        return true;
    }

    private ObsInformation parseDataUri(String dataUri, String id) {
        String[] dataUriParts = dataUri.split("/");
        ObsInformation newStation = new ObsInformation();
        String pluginName = dataUriParts[1];
        newStation.datauri = dataUri;

        if (pluginName.equals(PLUGIN_GOESSOUNDING)
                || pluginName.equals(PLUGIN_POESSOUNDING)
                || pluginName.equals(PLUGIN_ACARSSOUNDING)) {
            newStation.icao = id;
            newStation.latLon = new Coordinate(
                    Double.parseDouble(dataUriParts[5]),
                    Double.parseDouble(dataUriParts[4]));
        } else if (pluginName.equals(PLUGIN_ACARS)
                || pluginName.equals(PLUGIN_PROFILER)) {
            newStation.icao = id;
            newStation.latLon = new Coordinate(
                    Double.parseDouble(dataUriParts[6]),
                    Double.parseDouble(dataUriParts[5]));
        } else if (pluginName.equals(PLUGIN_PIREP)) {
            newStation.icao = id;
            newStation.latLon = new Coordinate(
                    Double.parseDouble(dataUriParts[7]),
                    Double.parseDouble(dataUriParts[6]));
        } else {
            newStation.icao = dataUriParts[5];
            newStation.latLon = new Coordinate(
                    Double.parseDouble(dataUriParts[7]),
                    Double.parseDouble(dataUriParts[6]));
        }
        return newStation;
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        if (this.displayedObsTime != null) {
            String intFrmTime = this.displayedObsTime.toString();
            if (!this.frameInfo.containsKey(intFrmTime)) {
                return "NO DATA";
            }

            Coordinate latlon = null;
            try {
                latlon = coord.asLatLon();
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error transforming coordinate", e);
            }
            double[] ml = { latlon.x, latlon.y };
            double[] pml = descriptor.worldToPixel(ml);
            double scaleValue = (this.plotWidth / 2.0)
                    / this.screenToWorldRatio;
            PixelExtent interrogationExtent = new PixelExtent(pml[0]
                    - scaleValue, pml[0] + scaleValue, pml[1] - scaleValue,
                    pml[1] + scaleValue);
            Envelope llExtent = descriptor.pixelToWorld(interrogationExtent);
            ArrayList<Coordinate> locations = this.frameInfo.get(intFrmTime).frameStationLocations;
            ArrayList<String> locationUris = this.frameInfo.get(intFrmTime).frameDataUris;
            Collection<Integer> stationList = this.frameInfo.get(intFrmTime).frameStations
                    .values();
            ArrayList<Integer> availableStnIdx = new ArrayList<Integer>();
            ArrayList<Coordinate> availableCoord = new ArrayList<Coordinate>();

            for (int stationIndex : stationList) {
                Coordinate location = locations.get(stationIndex);
                if (!llExtent.contains(location.x, location.y)) {
                    continue;
                }
                availableStnIdx.add(stationIndex);
                availableCoord.add(location);
            }

            if (availableStnIdx.size() == 1) {
                return this.generator.getStationMessage(locationUris
                        .get(availableStnIdx.get(0)));
            } else if (availableStnIdx.size() > 1) {
                int index = findClosestPlot(latlon, availableCoord);
                if (index != -1) {
                    return this.generator.getStationMessage(locationUris
                            .get(availableStnIdx.get(index)));
                }
            }
        }
        return "NO DATA";
    }

    /**
     * Returns the index of the closest available plot in the same area as the
     * sampling position.
     * 
     * @param latlon
     *            The coordinate of the sampling position.
     * @param availableCoord
     *            List of available plots in the same area as latlon.
     * @return The index of the plot closest to latlon.
     */
    private int findClosestPlot(Coordinate latlon,
            ArrayList<Coordinate> availableCoord) {
        double x1 = latlon.x;
        double y1 = latlon.y;
        double minDistance = MAX_SAMPLE_DISANCE;
        int minIndex = -1;

        for (int i = 0; i < availableCoord.size(); i++) {
            Coordinate coord = availableCoord.get(i);
            double x2 = coord.x;
            double y2 = coord.y;
            double d = Math.sqrt(Math.pow(x2 - x1, 2) + Math.pow(y2 - y1, 2));

            if (d < minDistance) {
                minDistance = d;
                minIndex = i;
            }
        }

        return minIndex;
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type.equals(ChangeType.DATA_UPDATE)) {
            PluginDataObject[] pdos = (PluginDataObject[]) object;
            for (PluginDataObject pdo : pdos) {
                try {
                    addRecord(pdo);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error updating plot resource", e);
                }
            }
        } else if (type.equals(ChangeType.CAPABILITY)) {
            if (object instanceof ColorableCapability) {
                if (generator != null) {
                    generator.setPlotModelColor(((ColorableCapability) object)
                            .getColor());
                }
            } else if (object instanceof OutlineCapability) {
                OutlineCapability cap = (OutlineCapability) object;
                if (generator != null) {
                    generator.setPlotModelLineStyle(cap.getLineStyle());
                    generator.setPlotModelLineWidth(cap.getOutlineWidth());
                }
            }
        }
        issueRefresh();
    }

}
