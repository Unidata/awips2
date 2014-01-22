package gov.noaa.nws.ncep.viz.rsc.solarimage.rsc;

import gov.noaa.nws.ncep.common.dataplugin.solarimage.SolarImageRecord;
import gov.noaa.nws.ncep.viz.common.ColorMapUtil;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarResource;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarResourceData;
import gov.noaa.nws.ncep.viz.rsc.solarimage.actions.CarrLatLonCapability;
import gov.noaa.nws.ncep.viz.rsc.solarimage.actions.CylindricalCedCapability;
import gov.noaa.nws.ncep.viz.rsc.solarimage.actions.StonyLatLonCapability;
import gov.noaa.nws.ncep.viz.rsc.solarimage.display.SolarImageMatchCriteria;
import gov.noaa.nws.ncep.viz.rsc.solarimage.util.ImageFunctionParser;
import gov.noaa.nws.ncep.viz.ui.display.ColorBarFromColormap;
import gov.noaa.nws.ncep.viz.ui.display.NCNonMapDescriptor;

import java.io.File;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.TreeMap;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.AbstractStylePreferences;
import com.raytheon.uf.common.style.LabelingPreferences;
import com.raytheon.uf.common.style.MatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.style.StyleRuleset;
import com.raytheon.uf.common.style.image.DataScale;
import com.raytheon.uf.common.style.image.DataScale.Type;
import com.raytheon.uf.common.style.image.ImagePreferences;
import com.raytheon.uf.common.style.image.SamplePreferences;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The resource class for Solar Image.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer         Description
 * ------------ ---------- -----------      --------------------------
 * 02/21/2013    958       qzhou, sgurung   Initial creation
 * 03/06/2013    972       ghull            make a NatlCntrs resource on an
 *                                          NCNonMapDescriptor
 * 03/19/2013    958       qzhou, sgurung   implemented colormap and colorbar
 * 04/03/2013    958       qzhou            Added cylindrical display to updateFrameData, paintFrame, 
 *                                              inspect, dispose and construct.
 * 11/04/2013    958       qzhou            Combined CylindricalDisplay with SolarImageDisplay
 * 11/12/2013    958       qzhou            Add latlonOverlay dispose
 * 11/27/2013    958       sgurung          Add method setAllFramesColorMapChanged()
 * 12/16/2013    #958      sgurung          Set virtual cursor to point to lat/lon instead of pixel coordinates (in multipnaes)
 * 12/27/2013    #1046     qzhou            Added getFunctioningRecords method
 * Sep 5,2013    2051       mnash           Fixed a deprecated method.
 * </pre>
 * 
 * @author qzhou, sgurung
 * @version 1.0
 */
public class SolarImageResource extends
        AbstractNatlCntrsResource<SolarImageResourceData, NCNonMapDescriptor>
        implements INatlCntrsResource, IResourceDataChanged {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SolarImageResource.class);

    private final SolarImageResourceData solarImgRscData;

    private ColorMapParameters colorMapParams;

    private boolean log10scale = false;

    protected ColorBarResource cbarResource;

    protected ResourcePair cbarRscPair;

    private DataScale scale = null;

    protected IRscDataObject rscDataObj;

    protected int interval;

    // sampling
    boolean sampling = false;

    private final Sampling samplingRsc;

    private final IInputHandler inputAdapter = getSolarImageInputHandler();

    protected ReferencedCoordinate sampleCoord;

    private boolean isColorMapChanged;

    protected Coordinate virtualCursor;// virtual cursor location

    protected TreeMap<Long, SolarImageRecord> functioningRecordMap = new TreeMap<Long, SolarImageRecord>();

    private RGB rgbW;

    private RGB rgbB;

    protected static class SampleResult {

        public SampleResult() {
        }

        public String[] labels;

        public RGB[] colors;
    }

    public boolean isCarrington = false;

    protected boolean displayLatLonOverlay = false;

    // Cylindrical projection
    private int cylindrical = 0; // 0--no cylindrical, 1--stony, 2--carrington

    // image differencing
    private int difference = 1; // 0--no compare, 1--rundiff+frame,
                                // 2--runratio+frame,
                                // 3--rundiff+time, 4--runratio+time,
                                // 5----runbase+frame, 6--runbase+time,

    private String imageFunction;

    protected class FrameData extends AbstractFrameData {

        // save only the image which best time matches to this frame.
        // if this is the dominant resource then this will be an exact match
        // since this record's time was used to generate the timeline.

        private SolarImageDisplay imageDisplay = null;

        private long timeMatch = -1;

        private String legendStr = "No Data";

        private final SimpleDateFormat dateFmt = new SimpleDateFormat(
                "yyMMdd/HHmm");

        protected FrameData(DataTime time, int interval) {
            super(time, interval);
            dateFmt.setTimeZone(TimeZone.getTimeZone("GMT"));

        }

        // use the dfltRecordRscDataObj which just stores the one
        // solar image record
        @Override
        public boolean updateFrameData(IRscDataObject rscDataObj) {
            PluginDataObject pdo = ((DfltRecordRscDataObj) rscDataObj).getPDO();

            if (!(pdo instanceof SolarImageRecord)) {
                System.out.println("Unexpected resource data object, "
                        + pdo.getClass().getName()
                        + ". Expecting SolarImageRecored");
                return false;
            }

            SolarImageRecord imgRec = (SolarImageRecord) pdo;

            long newTimeMatch = timeMatch(pdo.getDataTime());
            long currTime = 0;
            if (newTimeMatch < 0) { // sanity check.
                return false;
            }

            if (imageDisplay == null) {
                try {
                    // setColorMapParametersAndColorBar();
                    currTime = pdo.getDataTime().getValidTime().getTime()
                            .getTime();
                    imageDisplay = new SolarImageDisplay(imgRec, currTime,
                            descriptor.getGridGeometry(), log10scale,
                            cylindrical, imageFunction, functioningRecordMap);
                    setLegendForFrame(imgRec);

                } catch (VizException e) {
                    statusHandler.handle(
                            Priority.PROBLEM,
                            "Error creating SolarImageDisplay"
                                    + e.getLocalizedMessage(), e);
                    return false;
                } catch (Exception e) {
                    statusHandler.handle(
                            Priority.PROBLEM,
                            "Error creating SolarImageDisplay"
                                    + e.getLocalizedMessage(), e);

                    return false;
                }
                timeMatch = newTimeMatch;
                if (imageDisplay != null) {
                    return true;
                }
            }

            // determine if this image is a better time match than current one
            if (newTimeMatch < timeMatch) {
                try {
                    // setColorMapParametersAndColorBar();
                    currTime = pdo.getDataTime().getValidTime().getTime()
                            .getTime();
                    imageDisplay = new SolarImageDisplay(imgRec, currTime,
                            descriptor.getGridGeometry(), log10scale,
                            cylindrical, imageFunction, functioningRecordMap);
                    setLegendForFrame(imgRec);
                } catch (VizException e) {
                    System.out.println("Error creating SolarImageDisplay:"
                            + e.getMessage());
                    return false;
                }

                timeMatch = newTimeMatch;
                return true;
            }
            imgRec.setRawData(null);
            return false;
        }

        public String getLegendForFrame() {
            return legendStr;
        }

        // TODO : probably not correct : fix this as it needs to be.
        public void setLegendForFrame(SolarImageRecord rec) {

            String timeStr = dateFmt.format(rec.getDataTime().getRefTime());

            // from nameGenerator
            String instru = rec.getInstrument();
            if (instru.equalsIgnoreCase("NA")) {
                instru = "";
            }

            String wave = rec.getWavelength();
            if (wave.equalsIgnoreCase("NA")) {
                wave = "";
            }

            String inttime = "";
            Double intTime = rec.getIntTime();
            if (rec.getSatellite().startsWith("GOES")) {
                inttime = intTime.toString() + "s";
            }

            String site = rec.getSite();
            if (site.equalsIgnoreCase("NA") || (site.length() == 0)) {
                site = "";
            }

            legendStr = rec.getSatellite() + " "
                    + (instru == "" ? "" : instru + " ")
                    + (site == "" ? "" : site + " ")
                    + (wave == "" ? "" : wave + " ") + inttime + " " + timeStr;

        }

        @Override
        public void dispose() {

            if (imageDisplay != null) {
                imageDisplay.dispose();
            }
            super.dispose();
        }
    }

    protected SolarImageResource(SolarImageResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        solarImgRscData = resourceData;
        resourceData.addChangeListener(this);
        samplingRsc = new Sampling();
        this.rgbW = new RGB(255, 255, 255);
        this.rgbB = new RGB(0, 0, 0);

        getCapabilities().addCapability(CarrLatLonCapability.class);
        getCapabilities().addCapability(StonyLatLonCapability.class);
        getCapabilities().addCapability(CylindricalCedCapability.class);

        getCapability(ColorMapCapability.class).setSuppressingMenuItems(true);
        getCapability(ImagingCapability.class).setSuppressingMenuItems(true);
        getCapability(ColorableCapability.class).setSuppressingMenuItems(true);

        imageFunction = solarImgRscData.getImageFunction();
    }

    @Override
    protected void disposeInternal() {

        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(inputAdapter);
        }

        // don't we want to remove this as a change listener?
        getDescriptor().getResourceList().remove(cbarRscPair);
        if (cbarResource != null
                && cbarResource.getResourceData().getColorbar() != null) {
            cbarResource.getResourceData().getColorbar().dispose();
            cbarResource.getResourceData().setColorBar(null);
        }

        if (samplingRsc != null) {
            samplingRsc.dispose();
        }

        super.disposeInternal();
    }

    @Override
    public void paintFrame(AbstractFrameData frmData, IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        FrameData currFrame = (FrameData) frmData;

        // NOTE : this assumes that the solarImageDisplay is created
        // when the frame is updated. If this is time consuming we
        // could always just save the record when the frame is updated and
        // create/save the imageDisplay when on the first paint of this frame.

        SolarImageDisplay imageDisplay = currFrame.imageDisplay;
        if (imageDisplay != null) {
            if (hasCapability(ImagingCapability.class)) {
                ImagingCapability imaging = getCapability(ImagingCapability.class);
                imageDisplay.setBrightness(imaging.getBrightness());
                imageDisplay.setContrast(imaging.getContrast());
                imageDisplay.setInterpolationState(imaging
                        .isInterpolationState());
            }

            if (hasCapability(ColorMapCapability.class)) {
                ColorMapCapability cMap = getCapability(ColorMapCapability.class);
                colorMapParams = cMap.getColorMapParameters();
            }

            if (isColorMapChanged) {
                updateColorMap();
                imageDisplay.setColorMapChanged(isColorMapChanged);
            }

            imageDisplay.setCylindrical(cylindrical);
            imageDisplay.setColorMapParameters(colorMapParams);

            imageDisplay.paint(target, paintProps);

            isColorMapChanged = false;

            // draw the VirtualCursor and sampling
            drawVirtualCursor(paintProps, target);
            if (isSampling()) {

                samplingRsc.paintResult(target, descriptor, paintProps,
                        sampleCoord);
            }

            // TODO : draw the lat lon lines even if there is no image?
            if (isLatLonOverlayOn()) {
                try {
                    if (isCarrington) {
                        imageDisplay.setLatLonInterval(Integer
                                .parseInt(getCapabilities().getCapability(
                                        resourceData,
                                        CarrLatLonCapability.class)
                                        .getInterval()));
                    } else {
                        imageDisplay.setLatLonInterval(Integer
                                .parseInt(getCapabilities().getCapability(
                                        resourceData,
                                        StonyLatLonCapability.class)
                                        .getInterval()));
                    }
                } catch (NumberFormatException e) {

                }
                imageDisplay.drawOverlay(target, descriptor, paintProps,
                        isCarrington);

            }
        }
    }

    private void updateColorMap() {
        float scaleMin = scale.getMinValue().floatValue();
        float scaleMax = scale.getMaxValue().floatValue();
        float range = scaleMax - scaleMin;

        float cMapMax = (float) (range / 255.0)
                * colorMapParams.getColorMapMax();
        float cMapMin = (float) (range / 255.0)
                * colorMapParams.getColorMapMin();

        if (range <= 255.0) {
            if ((colorMapParams.getColorMapMax() > range)) {
                colorMapParams.setColorMapMax(cMapMax);
            }
            if ((colorMapParams.getColorMapMin() > range)) {
                colorMapParams.setColorMapMin(cMapMin);
            }
        }

        else {
            if (scaleMin >= 0) {
                if ((colorMapParams.getColorMapMax() >= 0)
                        && (cMapMax <= range)) {
                    colorMapParams.setColorMapMax(cMapMax);
                }

                if ((colorMapParams.getColorMapMin() >= 0)
                        && (cMapMin <= range)) {
                    colorMapParams.setColorMapMin(cMapMin);
                }
            } else {
                if ((colorMapParams.getColorMapMax() >= 0)
                        && (cMapMax <= range)) {
                    colorMapParams.setColorMapMax(cMapMax - scaleMax);
                }

                if ((colorMapParams.getColorMapMin() >= 0)
                        && (cMapMin <= range)) {
                    colorMapParams.setColorMapMin(cMapMin - scaleMax);
                }
            }
        }
    }

    public boolean isLatLonOverlayOn() {
        return displayLatLonOverlay;
    }

    public void setLatLonOverlay(boolean latLon) {
        this.displayLatLonOverlay = latLon;
    }

    public int getCylindrical() {
        return cylindrical;
    }

    public void setCylindrical(int cylind) {
        this.cylindrical = cylind;
    }

    public int getDifference() {
        return difference;
    }

    public void setDifference(int difference) {
        this.difference = difference;
    }

    @Override
    public void initResource(IGraphicsTarget target) throws VizException {

        // create the colorBar Resource and add it to the resourceList for this
        // descriptor.
        cbarRscPair = ResourcePair
                .constructSystemResourcePair(new ColorBarResourceData(
                        solarImgRscData.getColorBar()));

        getDescriptor().getResourceList().add(cbarRscPair);
        getDescriptor().getResourceList().instantiateResources(getDescriptor(),
                true);

        cbarResource = (ColorBarResource) cbarRscPair.getResource();

        if (cbarResource != null) {
            setColorMapParametersAndColorBar();
        }

        if (descriptor.getRenderableDisplay().getContainer().getDisplayPanes().length > 1) {
            descriptor.getTimeMatcher().redoTimeMatching(descriptor);
        }

        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container
                    .registerMouseHandler(inputAdapter, InputPriority.RESOURCE);
        }

        // use the default which just gets all the images in the db regardless
        // of the number of frames selected.
        queryRecords();

        getFunctioningRecords();
    }

    public Map getFunctioningRecords() {
        /*
         * A imageFunction includes function name and parameters. The
         * imageDifferencing is one of imageFunctions. The first parameter in
         * imagedifferencing is time. For runDiffTime, the time is minutes; for
         * basediffTime, the time is a date with yyMMdd/hhmm format.
         */
        String[] elements = ImageFunctionParser.parse(imageFunction);

        long funcTimeDiff = 0;

        if (elements != null && elements[0].startsWith("run")) {// runDiffTime
            if (elements[1] != null) {
                funcTimeDiff = Integer.parseInt(elements[1]) * 60 * 1000;
            }
        }

        else if (elements != null && elements[0].startsWith("base")) { // baseDiff
            if (elements[1] != null) { // 131127/1300
                SimpleDateFormat sdf = new SimpleDateFormat("yyMMdd/HHmm");
                sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

                try {
                    Date date = sdf.parse(elements[1]);
                    funcTimeDiff = date.getTime();

                } catch (ParseException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error parsing date. Use yyMMdd/HHmm format for the date."
                                    + e.getLocalizedMessage(), e);

                }
            }
        }

        // frameDataMap to frameDataTimeList
        List<Calendar> frameDataTimeList = new ArrayList<Calendar>();
        Iterator iterator = frameDataMap.entrySet().iterator();
        while (iterator.hasNext()) {
            @SuppressWarnings("unchecked")
            Map.Entry<Long, AbstractFrameData> entry = (Map.Entry<Long, AbstractFrameData>) iterator
                    .next();
            AbstractFrameData obj = (AbstractFrameData) entry.getValue();
            DataTime time = obj.getFrameTime();

            frameDataTimeList.add((Calendar) time.getRefTimeAsCalendar());
        }

        // construct functioningRecordMap
        SolarImageRecord record = null;
        long functioningRecordTime = 0;

        if (elements != null && elements[0].startsWith("base")) {
            functioningRecordTime = funcTimeDiff;

            for (IRscDataObject obj : newRscDataObjsQueue) {
                Calendar calendar = obj.getDataTime().getValidTime();
                calendar.set(Calendar.SECOND, 0);
                calendar.set(Calendar.MILLISECOND, 0);

                long timeOnQueue = calendar.getTimeInMillis();// trunk time

                if (timeOnQueue == functioningRecordTime) {
                    DfltRecordRscDataObj DfltObj = (DfltRecordRscDataObj) obj;
                    record = (SolarImageRecord) DfltObj.getPDO();

                    break;
                }
            }

            if (record != null) {
                for (int i = 0; i < frameDataTimeList.size(); i++)
                    functioningRecordMap.put(frameDataTimeList.get(i)
                            .getTimeInMillis(), record);// orig time
            }
        }

        else {
            for (int i = 0; i < frameDataTimeList.size(); i++) {
                Calendar cal = frameDataTimeList.get(i);
                Calendar calen = (Calendar) cal.clone();
                calen.set(Calendar.SECOND, 0);
                calen.set(Calendar.MILLISECOND, 0);

                functioningRecordTime = calen.getTimeInMillis() - funcTimeDiff;

                for (IRscDataObject obj : newRscDataObjsQueue) {

                    Calendar calendar = obj.getDataTime().getValidTime();
                    calendar.set(Calendar.SECOND, 0);
                    calendar.set(Calendar.MILLISECOND, 0);

                    long timeOnQueue = calendar.getTimeInMillis();

                    if (timeOnQueue == functioningRecordTime) {
                        // System.out.println("***n " + obj.getDataTime() + " "
                        // + frameDataTimeList.get(i).getTime());
                        DfltRecordRscDataObj DfltObj = (DfltRecordRscDataObj) obj;
                        record = (SolarImageRecord) DfltObj.getPDO();

                        // put frameDataMap time here
                        functioningRecordMap.put(frameDataTimeList.get(i)
                                .getTimeInMillis(), record);
                        break;
                    }
                }
            }
        }

        // System.out.println("*** " + functioningRecordMap.size() + " "
        // + frameDataTimeList.size());

        return functioningRecordMap;
    }

    public String getLegendStr() {
        FrameData curFrame = (FrameData) getCurrentFrame();
        return (curFrame != null ? curFrame.getLegendForFrame()
                : "No Matching Data");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#inspect(com.raytheon
     * .uf.common.geospatial.ReferencedCoordinate)
     */
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {

        if (coord == null)
            return "No Data";

        if (!sampling)
            return "";

        if (cylindrical != 0) {
            if (cylindrical == 1) {
                isCarrington = false;
            } else {
                isCarrington = true;
            }

            try {
                if (coord.asLatLon().y > 750.0 || coord.asLatLon().y < 250.0) {
                    return "";
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        StringBuilder sb = new StringBuilder();
        sb.append("** " + this.getLegendStr() + " **\n");

        try {
            FrameData currFrame = (FrameData) getCurrentFrame();

            SolarImageDisplay imageDisplay = currFrame.imageDisplay;

            if (imageDisplay == null) {
                return "";
            }

            sb.append("Pixel: ");

            double[] coordDbl = new double[2];
            coordDbl[0] = imageDisplay.formatValue(coord.asLatLon().x);
            coordDbl[1] = imageDisplay.formatValue(coord.asLatLon().y);

            sb.append((new Coordinate(coordDbl[0], coordDbl[1])).toString());
            sb.append('\n');

            Map<String, Object> map = imageDisplay.interrogate(coord);

            if (map != null) {
                for (Map.Entry<String, Object> item : map.entrySet()) {
                    sb.append(item.getKey() + ": ");
                    sb.append(item.getValue().toString());
                    sb.append('\n');
                }
            } else {
                return "";
            }

            return sb.toString();
        } catch (TransformException e) {

            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (FactoryException e) {

            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        return "No Data";
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.sampling.ISamplingResource#isSampling()
     */

    public IInputHandler getSolarImageInputHandler() {
        return new SolarImageInputAdapter<SolarImageResource>(this);
    }

    public boolean isSampling() {
        return sampling;
    }

    public void setSampling(boolean sampling) {
        this.sampling = sampling;
    }

    // the colorBar and/or the colormap may have changed so update the
    // colorBarPainter and the colorMapParametersCapability which holds
    // the instance of the colorMap that Raytheon's code needs
    @Override
    public void resourceAttrsModified() {
        // update the colorbarPainter with a possibly new colorbar
        ColorBarFromColormap colorBar = solarImgRscData.getColorBar();

        ColorMapParameters cmapParams = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        cmapParams.setColorMap(colorBar.getColorMap());
        cmapParams.setColorMapName(solarImgRscData.getColorMapName());
        // not currently an attribute but could be.
        cmapParams.setDisplayUnit(solarImgRscData.getDisplayUnit());

        getCapability(ColorMapCapability.class).setColorMapParameters(
                cmapParams);
        cbarResource.setColorBar(colorBar);

        isColorMapChanged = true;
        setAllFramesColorMapChanged(isColorMapChanged);

    }

    @Override
    protected AbstractFrameData createNewFrame(DataTime frameTime,
            int frameInterval) {
        return new FrameData(frameTime, frameInterval);
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        // Don't need this for a DATA_UPDATE but may need it
        // for an image or colormap capability changed.
        if ((type != null) && (type == ChangeType.CAPABILITY)) {
            if (object instanceof ImagingCapability) {
                ImagingCapability imgCap = getCapability(ImagingCapability.class);
                ImagingCapability newImgCap = (ImagingCapability) object;
                imgCap.setBrightness(newImgCap.getBrightness(), false);
                imgCap.setContrast(newImgCap.getContrast(), false);
                imgCap.setAlpha(newImgCap.getAlpha(), false);
                solarImgRscData.setAlpha(imgCap.getAlpha());
                solarImgRscData.setBrightness(imgCap.getBrightness());
                solarImgRscData.setContrast(imgCap.getContrast());
                solarImgRscData.setImageFunction(imageFunction);
                issueRefresh();

            } else if (object instanceof ColorMapCapability) {

                ColorMapCapability colorMapCap = getCapability(ColorMapCapability.class);
                ColorMapCapability newColorMapCap = (ColorMapCapability) object;
                colorMapCap.setColorMapParameters(
                        newColorMapCap.getColorMapParameters(), false);
                ColorMap theColorMap = (ColorMap) colorMapCap
                        .getColorMapParameters().getColorMap();
                String colorMapName = colorMapCap.getColorMapParameters()
                        .getColorMapName();
                solarImgRscData.setColorMapName(colorMapName);
                solarImgRscData.getRscAttrSet().setAttrValue("colorMapName",
                        colorMapName);

                ColorBarFromColormap cBar = solarImgRscData.getColorBar();
                cBar.setColorMap(theColorMap);

                ColorBarFromColormap colorBar = (ColorBarFromColormap) this.cbarResource
                        .getResourceData().getColorbar();
                if (colorBar != null) {
                    if ((colorBar.getImagePreferences() != null)
                            && (cBar.getImagePreferences() == null)) {
                        cBar.setImagePreferences(colorBar.getImagePreferences());
                    }

                    cBar.setIsScalingAttemptedForThisColorMap(colorBar
                            .isScalingAttemptedForThisColorMap());
                    cBar.setNumPixelsToReAlignLabel(colorBar
                            .isAlignLabelInTheMiddleOfInterval());
                }
                solarImgRscData.getRscAttrSet().setAttrValue("colorBar", cBar);
                solarImgRscData.setIsEdited(true);
                isColorMapChanged = true;
                issueRefresh();

            }

        }
    }

    @Override
    public void propertiesChanged(ResourceProperties updatedProps) {
        if (cbarRscPair != null) {
            cbarRscPair.getProperties().setVisible(updatedProps.isVisible());
        }
    }

    private String getLocFilePathForImageryStyleRule() {
        return NcPathConstants.SOLAR_IMG_STYLE_RULES;
    }

    private void setColorMapParametersAndColorBar() throws VizException {
        double minPixVal = Double.NaN;
        double maxPixVal = Double.NaN;

        ColorMap colorMap = null;

        DataMappingPreferences dataMap = null;
        LabelingPreferences labelPrefs = null;

        colorMapParams = getCapability(ColorMapCapability.class)
                .getColorMapParameters();

        try {
            colorMap = (ColorMap) ColorMapUtil.loadColorMap(solarImgRscData
                    .getResourceName().getRscCategory().getCategoryName(),
                    (solarImgRscData.getColorMapName() == null ? "Gray"
                            : solarImgRscData.getColorMapName()));
        } catch (VizException e) {
            throw new VizException("Error loading colormap: "
                    + solarImgRscData.getColorMapName());
        }

        colorMapParams.setColorMap(colorMap);
        getCapability(ColorMapCapability.class).setColorMapParameters(
                colorMapParams);

        ColorBarFromColormap colorBar = (ColorBarFromColormap) this.cbarResource
                .getResourceData().getColorbar();
        if (colorBar.getColorMap() == null) {
            colorBar.setColorMap((ColorMap) getCapability(
                    ColorMapCapability.class).getColorMapParameters()
                    .getColorMap());
        }
        MatchCriteria matchCriteria = SolarImageMatchCriteria
                .constructFromResourceData(solarImgRscData);

        ImagePreferences imgPref = new ImagePreferences();
        String locFileName = getLocFilePathForImageryStyleRule();
        // matchCriteria.setParameterName(parameterList);

        File file = NcPathManager.getInstance().getStaticFile(locFileName);
        StyleRule sRule = null;
        try {
            StyleRuleset styleSet = (StyleRuleset) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(StyleRuleset.class, file);

            if (styleSet != null) {
                List<StyleRule> styleRuleList = styleSet.getStyleRules();

                for (StyleRule sr : styleRuleList) {
                    MatchCriteria styleMatchCriteria = sr.getMatchCriteria();
                    if (styleMatchCriteria.matches(matchCriteria) > 0) {

                        AbstractStylePreferences stylePref = sr
                                .getPreferences();
                        if ((stylePref != null)
                                && (stylePref instanceof ImagePreferences)) {
                            imgPref = (ImagePreferences) stylePref;
                            /*
                             * Might need to change this if/when we use the
                             * data-scaling
                             */
                            SamplePreferences samplePref = imgPref
                                    .getSamplePrefs();
                            if (samplePref != null) {
                                minPixVal = imgPref.getSamplePrefs()
                                        .getMinValue();
                                maxPixVal = imgPref.getSamplePrefs()
                                        .getMaxValue();
                            } else if (imgPref.getDataScale() != null) {
                                DataScale ds = imgPref.getDataScale();
                                if (ds.getMaxValue() != null) {
                                    maxPixVal = ds.getMaxValue().doubleValue();
                                }
                                if (ds.getMinValue() != null) {
                                    minPixVal = ds.getMinValue().doubleValue();
                                }
                            }

                            colorBar.setImagePreferences(imgPref);
                            if (imgPref.getDisplayUnitLabel() != null) {
                                colorBar.setDisplayUnitStr(imgPref
                                        .getDisplayUnitLabel());

                            }
                            sRule = sr;
                            break;
                        }

                    }

                }

            }
        } catch (SerializationException e1) {

            e1.printStackTrace();
        } catch (StyleException e1) {

            e1.printStackTrace();
        } catch (NullPointerException e1) {

            e1.printStackTrace();
        }

        if (sRule == null)
            throw new VizException(
                    "Style Rule does not exist for this resource type (instrument: "
                            + solarImgRscData.getInstrument()
                            + ", wavelength: "
                            + solarImgRscData.getWavelength()
                            + " and intTime: "
                            + solarImgRscData.getIntTime()
                            + "). Please add it to solarImageryStyleRules.xml file located under Localization perspective -> NCEP -> Style Rules.  ");

        scale = ((ImagePreferences) sRule.getPreferences()).getDataScale();
        dataMap = ((ImagePreferences) sRule.getPreferences()).getDataMapping();
        labelPrefs = ((ImagePreferences) sRule.getPreferences())
                .getColorbarLabeling();

        // StyleRule sr = StyleManager.getInstance().getStyleRule(
        // StyleManager.StyleType.IMAGERY, matchCriteria);

        DataMappingPreferences dmPref = new DataMappingPreferences();
        if (imgPref.getDataMapping() != null) {

            dmPref = imgPref.getDataMapping();

            List<DataMappingEntry> dmEntriesList = dmPref.getEntries();

            if (dmEntriesList.size() > 0) {
                DataMappingEntry[] dmEntryArray = new DataMappingEntry[dmEntriesList
                        .size()];
                dmEntriesList.toArray(dmEntryArray);
                dmPref.setSerializableEntries(dmEntryArray);
                imgPref.setDataMapping(dmPref);
                SamplePreferences sampPref = new SamplePreferences();
                sampPref.setMinValue(0);
                sampPref.setMaxValue(255);
                imgPref.setSamplePrefs(sampPref);
                colorBar.setImagePreferences(imgPref);
                // colorBar.setDisplayUnitStr(solarImgRscData.getDisplayUnitStr());
                colorBar.setAlignLabelInTheMiddleOfInterval(false);
            }

        } else if ((minPixVal >= 0) && (maxPixVal <= 255)) {

            double ratio = (maxPixVal - minPixVal) / 255;
            DataMappingEntry dmEntry = new DataMappingEntry();
            double level = -1;
            for (int ii = 0; ii <= 255; ii++) {
                if ((ii % ((int) (255 / maxPixVal))) == 0) {
                    level = Math.round((ii - 1) * ratio) + minPixVal;
                    dmEntry = new DataMappingEntry();
                    dmEntry.setPixelValue((double) ii);
                    dmEntry.setDisplayValue(level);
                    dmEntry.setLabel(Double.toString(level));
                    dmPref.addEntry(dmEntry);
                }
            }

        } else {
            if (labelPrefs != null) {
                float[] labelValues = labelPrefs.getValues();
                int size = labelPrefs.getValues().length;

                DataMappingEntry dmEntry = new DataMappingEntry();
                dmEntry = new DataMappingEntry();
                dmEntry.setPixelValue(0.0);
                dmEntry.setDisplayValue(minPixVal);
                dmEntry.setLabel(Double.toString(minPixVal));
                dmPref.addEntry(dmEntry);

                int count = 0;
                double level = -1;
                for (int ii = 1; ii < 255; ii++) {
                    if (((ii % (255 / (size + 1))) == 0) && (count < size)) {
                        level = labelValues[count];
                        dmEntry = new DataMappingEntry();
                        dmEntry.setPixelValue((double) ii);
                        dmEntry.setDisplayValue(level);
                        dmEntry.setLabel(Double.toString(level));
                        dmPref.addEntry(dmEntry);
                        count++;
                    }
                }

                dmEntry = new DataMappingEntry();
                dmEntry.setPixelValue(255.0);
                dmEntry.setDisplayValue(maxPixVal);
                dmEntry.setLabel(Double.toString(maxPixVal));
                dmPref.addEntry(dmEntry);
            }
        }

        if (!colorBar.isScalingAttemptedForThisColorMap()) {
            imgPref = new ImagePreferences();
            imgPref.setDataMapping(dmPref);
            SamplePreferences sPref = new SamplePreferences();
            sPref.setMaxValue(255);
            sPref.setMinValue(0);
            imgPref.setSamplePrefs(sPref);
            colorBar.setImagePreferences(imgPref);
            colorBar.scalePixelValues();
        }

        colorBar.setAlignLabelInTheMiddleOfInterval(false);

        if (!colorBar.equals(solarImgRscData.getColorBar())) {
            this.solarImgRscData.setColorBar(colorBar);

        }
        this.cbarResource.getResourceData().setColorBar(colorBar);

        if (dataMap != null) {
            colorMapParams.setDataMapping(dataMap);
        }

        if ((labelPrefs != null) && (labelPrefs.getValues() != null)) {
            colorMapParams.setColorBarIntervals(labelPrefs.getValues());
        }

        if (scale != null) {
            colorMapParams.setColorMapMin(scale.getMinValue().floatValue());
            colorMapParams.setColorMapMax(scale.getMaxValue().floatValue());
            colorMapParams.setDataMin(scale.getMinValue().floatValue());
            colorMapParams.setDataMax(scale.getMaxValue().floatValue());
            log10scale = true;
            if ((scale.getScaleType() != null)
                    && (scale.getScaleType() == Type.LINEAR)) {
                log10scale = false;
            }
        } else {
            colorMapParams.setColorMapMin(0.0f);
            colorMapParams.setColorMapMax(255.0f);
            colorMapParams.setDataMin(0.0f);
            colorMapParams.setDataMax(255.0f);
            log10scale = true;
        }
    }

    protected void setAllFramesColorMapChanged(boolean cMapChanged) {
        for (AbstractFrameData frameData : frameDataMap.values()) {
            FrameData frame = (FrameData) frameData;
            if (frame.imageDisplay != null) {
                frame.imageDisplay.setColorMapChanged(cMapChanged);
                if (cMapChanged)
                    frame.imageDisplay.setColorMapParameters(colorMapParams);
            }
        }
    }

    public Coordinate getLatLonFromPixel(Coordinate pixelCoord) {
        FrameData currentFrame = (FrameData) getCurrentFrame();

        if (currentFrame != null) {
            SolarImageDisplay currentDisplay = ((FrameData) getCurrentFrame()).imageDisplay;

            if (currentDisplay != null) {
                return currentDisplay.getLatLonFromPixel(pixelCoord);
            }
        }
        return null;
    }

    public void setVirtualCursor(Coordinate virtualCursorLatLon) {

        FrameData currentFrame = (FrameData) getCurrentFrame();

        if (currentFrame != null) {
            SolarImageDisplay currentDisplay = ((FrameData) getCurrentFrame()).imageDisplay;

            if (currentDisplay != null) {
                virtualCursor = currentDisplay
                        .getPixelCoordFromLatLon(virtualCursorLatLon);
                if (virtualCursor != null) {

                    this.sampleCoord = new ReferencedCoordinate(
                            this.virtualCursor);

                    // }
                    // keep '+' with sampling
                    double[] out = this.getDescriptor().worldToPixel(
                            new double[] { virtualCursor.x, virtualCursor.y });
                    this.virtualCursor = new Coordinate(out[0], out[1]);

                }
            }
        }
    }

    protected void drawVirtualCursor(PaintProperties paintProps,
            IGraphicsTarget target) throws VizException {
        // Calculate scale for image
        double screenToWorldRatio = paintProps.getCanvasBounds().width
                / paintProps.getView().getExtent().getWidth();
        double outsideValue = 6 / screenToWorldRatio;

        if (virtualCursor != null) {

            target.drawLine(virtualCursor.x + outsideValue,
                    virtualCursor.y + 1, 0.0, virtualCursor.x - outsideValue,
                    virtualCursor.y + 1, 0.0, rgbB, 1.0f);
            target.drawLine(virtualCursor.x + 1,
                    virtualCursor.y + outsideValue, 0.0, virtualCursor.x + 1,
                    virtualCursor.y - outsideValue, 0.0, rgbB, 1.0f);

            target.drawLine(virtualCursor.x + outsideValue, virtualCursor.y,
                    0.0, virtualCursor.x - outsideValue, virtualCursor.y, 0.0,
                    rgbW, 1.0f);
            target.drawLine(virtualCursor.x, virtualCursor.y + outsideValue,
                    0.0, virtualCursor.x, virtualCursor.y - outsideValue, 0.0,
                    rgbW, 1.0f);
        }
    }
}
