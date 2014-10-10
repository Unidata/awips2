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
package gov.noaa.nws.ncep.viz.rsc.timeseries.rsc;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagAvg;
import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagRecord;
import gov.noaa.nws.ncep.common.dataplugin.geomag.calculation.CalcEach3hr;
import gov.noaa.nws.ncep.common.dataplugin.geomag.calculation.CalcUtil;
import gov.noaa.nws.ncep.common.dataplugin.geomag.exception.GeoMagException;
import gov.noaa.nws.ncep.common.dataplugin.geomag.request.DatabaseUtil;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.time_match.GraphTimelineUtil;
import gov.noaa.nws.ncep.viz.resources.time_match.NCTimeMatcher;
import gov.noaa.nws.ncep.viz.rsc.timeseries.GeoMagDescriptor;
import gov.noaa.nws.ncep.viz.rsc.timeseries.GeoMagGraph;
import gov.noaa.nws.ncep.viz.rsc.timeseries.view.KTableView;
import gov.noaa.nws.ncep.viz.rsc.timeseries.view.SamplingView;
import gov.noaa.nws.ncep.viz.ui.display.NCTimeSeriesDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NCTimeSeriesRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcPaneID;

import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeSet;

import javax.measure.converter.AddConverter;
import javax.measure.converter.UnitConverter;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.uf.viz.xy.graph.XyGraphDescriptor;
import com.raytheon.uf.viz.xy.graph.labeling.DataTimeLabel;
import com.raytheon.uf.viz.xy.graph.labeling.DoubleLabel;
import com.raytheon.uf.viz.xy.graph.labeling.IGraphLabel;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResource;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYDataList;
import com.raytheon.viz.core.graphing.xy.XYImageData;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The resource class for GeoMagResource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/27/2014              qzhou       Initial
 * 06/04/2014   #1136      qzhou       Added graph paintInternal. Added GeoMagDescriptor
 * 06/24/2014   #1136      qzhou       Modified loadInternal. Modified getTitle
 * 06/25/2014   #1136      qzhou       Fixed graph painting duplicated errors. Qdc subtract component's median.
 * 07/03/2014   R4079      qzhou       Added k-index view.
 * 07/10/2014   R4079      qzhou       Added SamplingView and Sampling related classes.
 * 07/28/2014   R4078      sgurung     Added code changes to support loading GeoMagResource in a new window.
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */
public class GeoMagResource extends
        AbstractNatlCntrsResource<GeoMagResourceData, NCTimeSeriesDescriptor>
        implements IGraphableResource<DataTime, Double>, INatlCntrsResource,
        IResourceDataChanged {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GeoMagResource.class);

    /*
     * sampling related
     */
    // boolean sampling = true;

    private final IInputHandler inputAdapter = getGeoMagSamplingInputHandler();

    private final Sampling samplingRsc;

    protected ReferencedCoordinate sampleCoord;

    private final SimpleDateFormat timeSampleFormat = new SimpleDateFormat(
            "yyyy/MM/dd'T'HH':'mm");

    protected static class SampleResult {

        public SampleResult() {
        }

        public String[] labels;

        public RGB[] colors;
    }

    /*
     * TimeSeries related
     */
    private GeoMagResourceData geoMagData;

    protected IDisplayPane currentPane;

    private final SimpleDateFormat dateFmt = new SimpleDateFormat(
            "yyyyMMdd/HHmm");

    /** The data in xy form */
    protected volatile XYDataList data = new XYDataList();

    protected String units;

    /** Denotes whether shapes are plotted at each data point * */
    protected boolean shapesVisible = false;//

    /** The graph to draw to */
    protected IGraph graph = null;

    private Set<DataTime> dataTimes = new TreeSet<DataTime>();

    protected NCTimeMatcher timeMatcher;

    protected DataTime timelineStart;

    protected DataTime timelineEnd;

    /*
     * geomag related
     */
    protected List<GeoMagRecord> magRecords;

    private UnitConverter toDeltanT;

    protected float[] hQdc;

    protected float[] dQdc;

    private float yMedian;

    /*
     * class FrameData not used except legenda
     */
    protected class FrameData extends AbstractFrameData {

        // save only the image which best time matches to this frame.
        // if this is the dominant resource then this will be an exact match
        // since this record's time was used to generate the timeline.

        private GeoMagGraph geoMagGraph;

        private long timeMatch = -1;

        private String legendStr = "No Data";

        protected FrameData(DataTime time, int interval) {
            super(time, interval);
            dateFmt.setTimeZone(TimeZone.getTimeZone("GMT"));

        }

        // use the dfltRecordRscDataObj which just stores the one
        @Override
        public boolean updateFrameData(IRscDataObject rscDataObj) {
            PluginDataObject pdo = ((DfltRecordRscDataObj) rscDataObj).getPDO();

            GeoMagRecord gmRec = (GeoMagRecord) pdo;

            long newTimeMatch = timeMatch(pdo.getDataTime());
            long currTime = 0;
            if (newTimeMatch < 0) { // sanity check.
                return false;
            }

            if (geoMagGraph == null) {
                try {

                    currTime = pdo.getDataTime().getValidTime().getTime()
                            .getTime();
                    geoMagGraph = new GeoMagGraph(
                            (XyGraphDescriptor) descriptor);
                    setLegendForFrame(gmRec);

                } catch (Exception e) {
                    statusHandler.handle(
                            Priority.PROBLEM,
                            "Error creating NCTimeSeriesDisplay"
                                    + e.getLocalizedMessage(), e);

                    return false;
                }
                timeMatch = newTimeMatch;
                if (geoMagGraph != null) {
                    return true;
                }
            }

            // determine if this image is a better time match than current one
            if (newTimeMatch < timeMatch) {
                currTime = pdo.getDataTime().getValidTime().getTime().getTime();
                geoMagGraph = new GeoMagGraph((XyGraphDescriptor) descriptor);
                setLegendForFrame(gmRec);

                timeMatch = newTimeMatch;
                return true;
            }

            return false;
        }

        public String getLegendForFrame() {
            return legendStr;
        }

        public void setLegendForFrame(GeoMagRecord rec) {

            String timeStr = dateFmt.format(rec.getDataTime().getRefTime());

            // from nameGenerator
            String stationCode = rec.getStationCode();

            String source = "";
            int sourceId = rec.getSourceId();
            if (sourceId == 101) {
                source = "direct";
            }

            legendStr = stationCode + " " + source + " " + timeStr;

        }

        @Override
        public void dispose() {

            super.dispose();
        }
    }

    protected GeoMagResource(GeoMagResourceData resData,
            LoadProperties loadProperties) {

        super(resData, loadProperties);
        geoMagData = resData;

        if (geoMagData.getShowKTableView()) {
            reopenKTableView();
        }

        if (geoMagData.getShowReadoutView()) {
            reopenSamplingView();
        }

        samplingRsc = new Sampling();
    }

    @Override
    protected void disposeInternal() {

        if (data != null) {
            data.dispose();
        }
        this.data = null;

        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(inputAdapter);
        }

        // close associated views
        IWorkbenchWindow win = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow();
        if (win == null)
            return;
        IWorkbenchPage wpage = win.getActivePage();
        if (wpage != null) {
            IViewPart vpart1 = wpage.findView(KTableView.kTableId);
            wpage.hideView(vpart1);

            IViewPart vpart2 = wpage.findView(SamplingView.samplingId);
            wpage.hideView(vpart2);
        }

        if (samplingRsc != null) {
            samplingRsc.dispose();
        }

        if (currentPane != null)
            currentPane.dispose();

        super.disposeInternal();
    }

    @Override
    public void paintFrame(AbstractFrameData frmData, IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

    }

    @Override
    public void initResource(IGraphicsTarget target) throws VizException {
        getDescriptor().getResourceList().instantiateResources(getDescriptor(),
                true);
        if (descriptor.getRenderableDisplay().getContainer().getDisplayPanes().length > 1) {
            ((NCTimeSeriesDescriptor) descriptor).getTimeMatcher()
                    .redoTimeMatching(((NCTimeSeriesDescriptor) descriptor));

        }

        // NCTimeMatcher tm = (NCTimeMatcher) ((NCTimeSeriesDescriptor)
        // descriptor)
        // .getTimeMatcher();

        timeMatcher = (NCTimeMatcher) ((NCTimeSeriesDescriptor) descriptor)
                .getTimeMatcher();

        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container
                    .registerMouseHandler(inputAdapter, InputPriority.RESOURCE);
        }

        DataTime[] timelineStartEnd = getQueryTime(timelineStart, timelineEnd);
        timelineStart = timelineStartEnd[0];
        timelineEnd = timelineStartEnd[1];
        geoMagData.setStartTime(timelineStart);
        queryRecords();
    }

    public DataTime[] getQueryTime(DataTime timelineStart, DataTime timelineEnd) {
        DataTime[] dataTimes = new DataTime[2];
        Long stl = Long.MAX_VALUE;
        Long etl = Long.MIN_VALUE;

        for (AbstractFrameData afd : frameDataMap.values()) {
            if (stl > afd.getFrameStartTime().getRefTime().getTime()) {

                stl = afd.getFrameStartTime().getRefTime().getTime();
            }
            if (etl < afd.getFrameEndTime().getRefTime().getTime()) {

                etl = afd.getFrameEndTime().getRefTime().getTime();
            }

        }
        Date start = new Date(stl);
        start.setSeconds(0);
        Date end = new Date(etl);
        end.setSeconds(0);

        timelineStart = new DataTime(start);
        timelineEnd = new DataTime(end);

        // snap to synoptic point
        // timeMatcher = (NCTimeMatcher) descriptor.getTimeMatcher();
        if (timelineStart.getRefTime().getMinutes() != 0) {

            Calendar tem = GraphTimelineUtil.snapTimeToClosest(
                    timelineStart.getRefTimeAsCalendar(),
                    timeMatcher.getHourSnap());
            timelineStart = new DataTime(tem);

            tem = GraphTimelineUtil.snapTimeToClosest(
                    timelineEnd.getRefTimeAsCalendar(),
                    timeMatcher.getHourSnap());
            timelineEnd = new DataTime(tem);
        }

        dataTimes[0] = timelineStart;
        dataTimes[1] = timelineEnd;

        return dataTimes;
    }

    // override base version to constrain on the selected timeline
    @Override
    public void queryRecords() throws VizException {

        HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>(
                resourceData.getMetadataMap());
        RequestConstraint reqConstr = new RequestConstraint();

        String startTimeStr = timelineStart.toString().substring(0, 19);
        String endTimeStr = timelineEnd.toString().substring(0, 19);

        String[] constraintList = { startTimeStr, endTimeStr };
        reqConstr.setBetweenValueList(constraintList);
        reqConstr.setConstraintType(RequestConstraint.ConstraintType.BETWEEN);

        queryList.put("dataTime.refTime", reqConstr);

        LayerProperty prop = new LayerProperty();
        prop.setDesiredProduct(ResourceType.PLAN_VIEW);
        prop.setEntryQueryParameters(queryList, false);
        prop.setNumberOfImages(100000);

        String script = null;
        script = ScriptCreator.createScript(prop);

        if (script == null)
            return;

        magRecords = new ArrayList<GeoMagRecord>();
        Object[] pdoList = Connector.getInstance().connect(script, null, 60000);

        for (Object pdo : pdoList) {
            for (IRscDataObject dataObject : processRecord(pdo)) {
                newRscDataObjsQueue.add(dataObject);
                magRecords
                        .add((GeoMagRecord) ((DfltRecordRscDataObj) dataObject)
                                .getPDO());
            }
        }

        sortRecord();
        data = loadInternal(magRecords);

        // get median before filling null to data
        String geoMagType = ((GeoMagResourceData) resourceData).getYAxesData();
        float[] temp = RetrieveUtils.getMedian(magRecords);

        if (geoMagType.startsWith("H")) {
            yMedian = temp[0];

        } else {
            yMedian = temp[1];
        }

        // Often magRecord is not end at a synoptic point. So append null to
        // magRecords if it does not end at a synoptic point.

        int recSize = data.getData().size();
        if (recSize > 0) {
            DataTime last = (DataTime) data.getData().get(recSize - 1).getX();
            Calendar lastCal = last.getRefTimeAsCalendar();
            Calendar cal = (Calendar) lastCal.clone();
            cal = GraphTimelineUtil.snapTimeToNext(cal,
                    timeMatcher.getHourSnap());
            int fill = (int) (cal.getTimeInMillis() - lastCal.getTimeInMillis()) / 60000;

            for (int i = 0; i < fill; i++) {
                lastCal.add(Calendar.MINUTE, 1);
                Calendar appendCal = (Calendar) lastCal.clone();

                XYData d = new XYData(new DataTime(appendCal), null);
                data.getData().add(d);
            }

            dataTimes = new TreeSet<DataTime>();
            for (XYData d : data.getData()) {
                dataTimes.add((DataTime) d.getX());
            }
        }

    }

    private XYDataList loadInternal(List<GeoMagRecord> recordsList) {

        ArrayList<XYData> data = new ArrayList<XYData>();
        if (recordsList.size() <= 1)
            return new XYDataList();

        int recordSize = recordsList.size();
        String geoMagType = ((GeoMagResourceData) resourceData).getYAxesData();

        Calendar endTime = recordsList.get(recordSize - 1).getDataTime()
                .getValidTime();

        Date spTime = CalcUtil.getSPTime(RetrieveUtils.getUtcDate(endTime));
        Date start = new Date(spTime.getTime() - 30l * 24l * 3600l * 1000l);

        // get qdc from database
        List<GeoMagAvg> avgList = null;

        try {
            avgList = RetrieveUtils.retrieveHrAvgs(recordsList.get(0)
                    .getStationCode(), start, spTime);

        } catch (GeoMagException e) {
            System.out
                    .println("GeoMagResource: Error retrieving average record.");
        }

        if (avgList != null && avgList.size() >= 5) {
            List<Date> dateListFinal = new ArrayList<Date>();
            List<Float> hHrAvgListFinal = new ArrayList<Float>();
            List<Float> dHrAvgListFinal = new ArrayList<Float>();

            DatabaseUtil.fillHrAvgTimeGaps(avgList, dateListFinal,
                    hHrAvgListFinal, dHrAvgListFinal, spTime);

            float[] hHrAvgs = CalcUtil.toFloatArray(hHrAvgListFinal);
            float[] dHrAvgs = CalcUtil.toFloatArray(dHrAvgListFinal);

            hQdc = CalcEach3hr.getHQdcOrDQdc(hHrAvgs, dHrAvgs);
            dQdc = CalcEach3hr.getHQdcOrDQdc(dHrAvgs, hHrAvgs);
            // qdcSize = 1440
        }

        // NCTimeMatcher tm = (NCTimeMatcher) ((NCTimeSeriesDescriptor)
        // descriptor)
        // .getTimeMatcher();

        int graphSize = timeMatcher.getGraphRange() * 60;

        /*
         * If load 12 hour record, calculate Qdc. To get Qdc, get 30 days
         * hourAvgs before the spTime.
         */
        if (recordSize <= 721
                && ((geoMagType.equalsIgnoreCase("HQdc") || geoMagType
                        .equalsIgnoreCase("DQdc")))) {

            int i = 0;
            Float y = 0f;
            for (i = 0; i < recordSize; i++) {

                DataTime x = recordsList.get(i).getDataTime();

                if (geoMagType.equals("HQdc")) {
                    if (i == graphSize)
                        y = hQdc[1440 - 1];
                    else
                        y = hQdc[1440 - graphSize + i];

                } else if (geoMagType.equals("DQdc")) {
                    if (i == graphSize)
                        y = dQdc[1440 - 1];
                    else
                        y = dQdc[1440 - graphSize + i];
                }

                data.add(new XYData(x, y));

            }

            // if !(hour % 3 == 2 && minute == 59), add Qdc to end of synoptic
            // time. Not for others
            DataTime extent = recordsList.get(i - 1).getDataTime();

            int hour = ((Calendar) extent.getValidTime()).get(Calendar.HOUR);// _OF_DAY);
            int minute = ((Calendar) extent.getValidTime())
                    .get(Calendar.MINUTE);

            if (!(hour % 3 == 2 && minute == 59)) {// && !(hour == 0 && minute
                                                   // == 0)) {

                int fillSize = 3 * 60 - minute;// (hour * 60 + minute);
                Calendar cal = extent.getValidTime();

                for (int j = 0; j < fillSize
                        && cal.getTime().before(timelineEnd.getRefTime()); j++) {
                    cal.add(Calendar.MINUTE, 1);
                    Calendar time = (Calendar) cal.clone();
                    DataTime x = new DataTime(time);

                    if (geoMagType.equalsIgnoreCase("HQdc")) {
                        y = hQdc[1440 - fillSize + j];

                    } else if (geoMagType.equalsIgnoreCase("DQdc")) {
                        y = dQdc[1440 - fillSize + j];
                    }

                    data.add(new XYData(x, y));
                }
            }
        }

        else {
            for (GeoMagRecord record : recordsList) {
                DataTime x = record.getDataTime();
                Float y = 0f;
                if (geoMagType.equalsIgnoreCase("H"))
                    y = record.getComponent_1();
                else if (geoMagType.equalsIgnoreCase("D"))
                    y = record.getComponent_2();

                data.add(new XYData(x, y));
            }

        }
        XYDataList list = new XYDataList();
        list.setData(data);

        return list;
    }

    public void autoupdateRecords() {

    }

    public void reopenKTableView() {

        IWorkbenchPage wpage = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();

        IViewPart vpart = wpage.findView(KTableView.kTableId);
        if (!wpage.isPartVisible(vpart)) {
            KTableView paletteWin = KTableView.getAccess();
            if (paletteWin != null)
                paletteWin.setEditorVisible(true);

            try {
                vpart = wpage.showView(KTableView.kTableId);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public void reopenSamplingView() {

        IWorkbenchPage wpage = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();

        IViewPart vpart = wpage.findView(SamplingView.samplingId);
        if (!wpage.isPartVisible(vpart)) {
            SamplingView paletteWin = SamplingView.getAccess();
            if (paletteWin != null)
                paletteWin.setEditorVisible(true);

            try {
                vpart = wpage.showView(SamplingView.samplingId);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.sampling.ISamplingResource#isSampling()
     */

    public IInputHandler getGeoMagSamplingInputHandler() {
        return new SamplingInputAdapter<GeoMagResource>(this);
    }

    @Override
    protected AbstractFrameData createNewFrame(DataTime frameTime,
            int frameInterval) {
        return new FrameData(frameTime, frameInterval);
    }

    @Override
    public void propertiesChanged(ResourceProperties updatedProps) {

    }

    // public static synchronized JAXBManager getJaxbManager()
    // throws JAXBException {
    // if (jaxb == null) {
    // SubClassLocator locator = new SubClassLocator();
    // Collection<Class<?>> classes = JAXBClassLocator.getJAXBClasses(
    // locator, StyleRuleset.class, StyleRule.class, Level.class,
    // AbstractStylePreferences.class, MatchCriteria.class);
    //
    // locator.save();
    //
    // Class<?>[] jaxbClasses = new Class<?>[classes.size() + 1];
    // classes.toArray(jaxbClasses);
    //
    // /*
    // * Add JaxbDummyObject at the beginning so properties are loaded
    // * correctly
    // */
    // jaxbClasses[jaxbClasses.length - 1] = jaxbClasses[0];
    // jaxbClasses[0] = JaxbDummyObject.class;
    //
    // jaxb = new JAXBManager(jaxbClasses);
    // }
    // return jaxb;
    // }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            PluginDataObject[] objects = (PluginDataObject[]) object;
            // for (PluginDataObject pdo : objects) {
            // addRecord(pdo);
            // }
        }
        issueRefresh();
    }

    private void sortRecord() {
        Collections.sort(magRecords, new Comparator<GeoMagRecord>() {

            @Override
            public int compare(GeoMagRecord g1, GeoMagRecord g2) {
                DataTime t1 = (DataTime) g1.getDataTime();
                DataTime t2 = (DataTime) g2.getDataTime();
                return t1.compareTo(t2);
            }
        });
    }

    public String getLegendStr() {
        FrameData curFrame = (FrameData) getCurrentFrame();
        return (curFrame != null ? curFrame.getLegendForFrame()
                : "No Matching Data");
    }

    public String getUnits() {
        return units;
    }

    /**
     * @return the data
     */
    public XYDataList getData() {
        return data;
    }

    /**
     * @param data
     *            the data to set
     */
    public void setData(XYDataList data) {
        this.data = data;
    }

    @Override
    public Object getGraphKey() {
        return (units != null ? units : "D");// adapter.getParameterName());
    }

    public double getDelta() {
        double delta = getMaxDataValue() - getMinDataValue();

        if (delta <= 20)
            return 40;
        else if (delta <= 60)
            return 80;
        else if (delta <= 100)
            return 120;
        else if (delta <= 140)
            return 160;
        else if (delta <= 200)
            return 200;
        else if (delta <= 400)
            return 400;
        else if (delta <= 800)
            return 800;
        else if (delta <= 1600)
            return 1600;
        else
            return 3200;
    }

    /*
     * don't use paintFrame since we don't want to updateFrame many times
     * 
     * @see
     * gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource#paintInternal
     * (com.raytheon.uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        Double magnification = getCapability(MagnificationCapability.class)
                .getMagnification();

        if (data == null) {
            return;
        }

        /*
         * if autoupdate, reconstruct graph extent. Only display the graph when
         * the synoptic time changes.
         */

        if (graph != null && timelineStart != geoMagData.getStartTime()) {

            graph.reconstruct();
            geoMagData.setStartTime(timelineStart);

        }

        boolean changeExtent = false;
        // timeMatcher = (NCTimeMatcher) descriptor.getTimeMatcher();

        if (timeMatcher.isAutoUpdateable()) {

            while (!newRscDataObjsQueue.isEmpty()) {
                IRscDataObject rscDataObj = newRscDataObjsQueue.poll();

                if (rscDataObj.getDataTime().compareTo(timelineEnd) > 0) {
                    changeExtent = true;
                    timelineStart = RetrieveUtils.moveToNextSynop(
                            timelineStart, timeMatcher.getHourSnap());

                    timelineEnd = RetrieveUtils.moveToNextSynop(timelineEnd,
                            timeMatcher.getHourSnap());

                    break;
                }
            }

            if (changeExtent) {
                ArrayList<DataTime> newList = new ArrayList<DataTime>();
                for (int i = 0; i < newRscDataObjsQueue.size(); i++) {
                    DataTime dataTime = newRscDataObjsQueue.poll()
                            .getDataTime();

                    if (dataTime.compareTo(timelineStart) >= 0
                            && dataTime.compareTo(timelineEnd) <= 0) {

                        newList.add(dataTime);
                    }
                }
                timeMatcher.updateTimeline(newList);

            }

            newRscDataObjsQueue.clear();
            queryRecords();

        }

        /*
         * get gmDescriptor and graph
         */
        IDisplayPane[] pane = GeoMagDescriptor.getDisplayPane();

        if (pane[0].getRenderableDisplay() instanceof NCTimeSeriesRenderableDisplay) {
            for (int i = 0; i < pane.length; i++) {
                if (checkPaneId((NCTimeSeriesDescriptor) descriptor, pane[i])) {
                    GeoMagDescriptor gmDescriptor = new GeoMagDescriptor();
                    gmDescriptor.setResourcePair(gmDescriptor, pane[i]);
                    gmDescriptor.setNCTimeMatcher(gmDescriptor, pane[i]);
                    gmDescriptor.addDescriptor(gmDescriptor, pane[i]);
                    gmDescriptor.setAutoUpdate(true);
                    graph = gmDescriptor.getGraph(this);
                    currentPane = pane[i];
                    break;
                }
            }
        } else {
            GeoMagDescriptor gmDescriptor = new GeoMagDescriptor(
                    (NCTimeSeriesDescriptor) this.descriptor);
            gmDescriptor.setAutoUpdate(true);
            graph = gmDescriptor.getGraph(this);
        }
        /*
         * Wait for graph to initialize before plotting to it, TODO: better
         */
        if (graph.isReady() == false) {
            return;
        }

        if (toDeltanT == null) {
            toDeltanT = createDataConverter();
        }

        graph.setCurrentMagnification(magnification);
        IExtent extent = ((GeoMagGraph) graph).getExtent();
        target.setupClippingPlane(extent);

        /*
         * display data
         */
        double[] prevScreen = null;
        String dataColor = ((GeoMagResourceData) resourceData).getDataColor();

        RGB color = RGBColors.getRGBColor(dataColor);

        for (int i = 0; i < data.getData().size(); i++) {

            XYData point = data.getData().get(i);

            if (point.getY() != null) {
                double newY = toDeltanT.convert(((Number) point.getY())
                        .doubleValue());

                double[] screen = getScreenPosition(point.getX(), newY);

                // Draws shapes for each data point
                // draw wind Data
                if (point instanceof XYImageData) {
                    // Draw all images in a striaight line. Move the line to be
                    // able to accomodate multiple resources.
                    List<AbstractVizResource<?, ?>> tsrs = descriptor
                            .getResourceList().getResourcesByType(
                                    TimeSeriesResource.class);
                    int index = tsrs.indexOf(this);
                    // IExtent extent = graph.getExtent();
                    screen[1] = extent.getMinY() + (index + 1)
                            * (extent.getHeight() / (tsrs.size() + 1));
                    XYImageData imageData = (XYImageData) point;
                    imageData.setColor(color);
                    imageData.setTarget(target);
                    PaintProperties imagePaintProperties = new PaintProperties(
                            paintProps);
                    imagePaintProperties.setAlpha(1.0f);
                    double ratio = paintProps.getView().getExtent().getWidth()
                            / paintProps.getCanvasBounds().width;

                    int[] dims = imageData.getDefaultSize();
                    double adjDims[] = new double[2];
                    adjDims[0] = (dims[0] * 0.5 * ratio) * magnification;
                    adjDims[1] = (dims[1] * 0.5 * ratio) * magnification;

                    Coordinate ul = new Coordinate(screen[0] - adjDims[0],
                            screen[1] - adjDims[1]);
                    Coordinate ur = new Coordinate(screen[0] + adjDims[0],
                            screen[1] - adjDims[1]);
                    Coordinate lr = new Coordinate(screen[0] + adjDims[0],
                            screen[1] + adjDims[1]);
                    Coordinate ll = new Coordinate(screen[0] - adjDims[0],
                            screen[1] + adjDims[1]);
                    PixelCoverage coverage = new PixelCoverage(ul, ur, lr, ll);

                    target.drawRaster(imageData.getImage(), coverage,
                            imagePaintProperties);
                    continue;
                }

                if (shapesVisible) {
                    target.drawRect(new PixelExtent(screen[0] - 3,
                            screen[0] + 3, screen[1] - 3, screen[1] + 3),
                            color, 1.0f, 1.0);
                }

                // Connects adjacent data points with a line
                if (prevScreen != null) {
                    OutlineCapability lineCap = getCapability(OutlineCapability.class);
                    target.drawLine(screen[0], screen[1], 0.0, prevScreen[0],
                            prevScreen[1], 0.0, color,
                            lineCap.getOutlineWidth(), lineCap.getLineStyle());
                }

                prevScreen = screen;
            }

            target.clearClippingPlane();

            /*
             * draw delts data
             */
            // IFont unitsFont = target.initializeFont((String) null, 14.0f,
            // new IFont.Style[] {});
            // target.drawString(unitsFont, this.getBaseline(),
            // graph.getExtent()
            // .getMaxX() + 100, graph.getExtent().getMinY() - 100, 0.0,
            // TextStyle.DROP_SHADOW, RGBColors.getRGBColor("orange"),
            // HorizontalAlignment.CENTER, VerticalAlignment.BOTTOM, 0.0);

        }

        /*
         * display k-index view
         */
        if (geoMagData.getShowKTableView()) {
            KTableView kTableWin = KTableView.getAccess();
            if (kTableWin != null && kTableWin.getEditorVisible()) {

                KTableView kTableViewWin = KTableView.getAccess();

                if (kTableViewWin != null) {
                    AbstractEditor editor = NcDisplayMngr
                            .getActiveNatlCntrsEditor();
                    IDisplayPane activePane = editor.getActiveDisplayPane();

                    if (activePane.getRenderableDisplay() instanceof NCTimeSeriesRenderableDisplay) {
                        NCTimeSeriesRenderableDisplay activeDisplay = (NCTimeSeriesRenderableDisplay) activePane
                                .getRenderableDisplay();

                        NCTimeSeriesRenderableDisplay currDisplay = (NCTimeSeriesRenderableDisplay) currentPane
                                .getRenderableDisplay();

                        if (activeDisplay.getPaneId().equals(
                                currDisplay.getPaneId()))

                            kTableViewWin.paintKTable(magRecords);
                    }
                }
            }
        }

        /*
         * display Readout view
         */
        if (geoMagData.getShowReadoutView()) {
            SamplingView samplinWin = SamplingView.getAccess();
            if (samplinWin != null && samplinWin.getEditorVisible()) {

                SamplingView samplingViewWin = SamplingView.getAccess();

                if (samplingViewWin != null) {
                    AbstractEditor editor = NcDisplayMngr
                            .getActiveNatlCntrsEditor();
                    IDisplayPane activePane = editor.getActiveDisplayPane();
                    if (activePane.getRenderableDisplay() instanceof NCTimeSeriesRenderableDisplay) {
                        NCTimeSeriesRenderableDisplay activeDisplay = (NCTimeSeriesRenderableDisplay) activePane
                                .getRenderableDisplay();

                        NCTimeSeriesRenderableDisplay currDisplay = (NCTimeSeriesRenderableDisplay) currentPane
                                .getRenderableDisplay();

                        if (activeDisplay.getPaneId().equals(
                                currDisplay.getPaneId())) {

                            samplingRsc.getResult(target, descriptor,
                                    paintProps, sampleCoord);
                        }
                    }
                }
            }
        }
    }

    protected boolean checkPaneId(NCTimeSeriesDescriptor desc, IDisplayPane pane) {

        NcPaneID paneId = ((NCTimeSeriesRenderableDisplay) desc
                .getRenderableDisplay()).getPaneId();

        NcPaneID newPaneId = ((NCTimeSeriesRenderableDisplay) pane
                .getRenderableDisplay()).getPaneId();

        if (newPaneId.getColumn() == paneId.getColumn()
                && newPaneId.getRow() == paneId.getRow()) {
            return true;

        } else {
            return false;
        }

    }

    private UnitConverter createDataConverter() {
        // yMedian() =0 causes alert
        UnitConverter conv = null;
        double temp = -0.0001;

        if (yMedian == 0)
            conv = new AddConverter(temp);
        else
            conv = new AddConverter(-1.0 * yMedian);

        return conv;
    }

    private double[] getScreenPosition(Object x, Object y) {
        double valY = ((Number) y).doubleValue();
        double valX = ((DataTime) x).getValidTime().getTimeInMillis();
        return graph.getGridLocation(valX, valY);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {

        StringBuilder sb = new StringBuilder(
                ((GeoMagResourceData) resourceData).getStation());
        if (data == null || data.getData().size() == 0) {
            sb.append(" ");
            sb.append(((GeoMagResourceData) resourceData).getSourceId());
            sb.append(" ");
            sb.append(((GeoMagResourceData) resourceData).getYAxesData());
            sb.append(" - NO DATA");

        } else {
            XYData point = data.getData().get(0);
            DataTime time = (DataTime) point.getX();

            dateFmt.format(time.getValidTimeAsDate());

            sb.append(" ");
            sb.append(((GeoMagResourceData) resourceData).getSourceId());
            sb.append(" ");
            sb.append(((GeoMagResourceData) resourceData).getYAxesData());
            sb.append(" - Begin: ");
            sb.append(time);

            sb.append(" " + this.getBaseline());
        }

        return sb.toString();
    }

    public String getTitle() {

        String yTitle = ((GeoMagResourceData) this.getResourceData())
                .getYAxesTitle();
        return (yTitle); // + "  " + baseline);
    }

    public String getBaseline() {

        String baseline = ((GeoMagResourceData) this.getResourceData())
                .getYDescription() + " " + String.format("%.1f", yMedian);

        return (baseline);
    }

    @Override
    public IGraphLabel<DataTime>[] getXRangeData() {
        DataTimeLabel[] labels = new DataTimeLabel[dataTimes.size()];
        int i = 0;
        for (DataTime time : dataTimes) {
            labels[i++] = new DataTimeLabel(time);
        }
        return labels;
    }

    @Override
    public IGraphLabel<Double>[] getYRangeData() {
        double min = this.getMinDataValue();
        double max = this.getMaxDataValue();
        return new DoubleLabel[] { new DoubleLabel(min), new DoubleLabel(max) };
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        if (coord == null)
            return "No Data";

        // if (!sampling)
        // return "";

        String inspect = null;
        double[] worldCoord = descriptor.pixelToWorld(new double[] {
                coord.getObject().x, coord.getObject().y });

        Coordinate c = ((XyGraphDescriptor) descriptor).getGraphCoordiante(
                this, new Coordinate(worldCoord[0], worldCoord[1]));

        if (c != null && data != null) {

            double[] vals = data.inspectXY(c);
            NumberFormat nf = NumberFormat.getInstance();
            nf.setMaximumFractionDigits(2);
            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            cal.setTimeInMillis((long) c.x);
            timeSampleFormat.setCalendar(cal);

            // inspect = nf.format(vals[1])
            // + ((units != null && units.equals("") == false) ? "("
            // + units + ")   " : "   ")
            // + timeSampleFormat.format(cal.getTime());

            SamplingView samplingViewWin = SamplingView.getAccess();

            if (samplingViewWin != null) {
                AbstractEditor editor = NcDisplayMngr
                        .getActiveNatlCntrsEditor();
                IDisplayPane activePane = editor.getActiveDisplayPane();
                NCTimeSeriesRenderableDisplay activeDisplay = (NCTimeSeriesRenderableDisplay) activePane
                        .getRenderableDisplay();

                NCTimeSeriesRenderableDisplay currDisplay = (NCTimeSeriesRenderableDisplay) currentPane
                        .getRenderableDisplay();

                if (activeDisplay.getPaneId().equals(currDisplay.getPaneId())) {

                    List<Float> hQdcList = new ArrayList<Float>();
                    List<Float> dQdcList = new ArrayList<Float>();

                    int hrExtra = (hQdc.length - magRecords.size() + 1) / 60;
                    int minExtra = hrExtra * 60;
                    int newSize = hQdc.length - minExtra;

                    for (int i = 0; i < newSize; i++) {
                        hQdcList.add(hQdc[hQdc.length - minExtra + i]);
                        dQdcList.add(dQdc[dQdc.length - minExtra + i]);
                    }

                    samplingViewWin.paintSampling(magRecords, hQdcList,
                            dQdcList, cal);
                }
            }

        }

        return inspect;
    }

    @Override
    public DataTime[] getDataTimes() {
        return dataTimes.toArray(new DataTime[0]);
    }

    public double getMinDataValue() {
        double min = Double.POSITIVE_INFINITY;
        if (data != null) {
            for (XYData d : data.getData()) {
                if (d.getY() instanceof Number) {
                    double y = ((Number) d.getY()).doubleValue();
                    min = Math.min(min, y);
                }
            }
        }
        return min;
    }

    public double getMaxDataValue() {
        double max = Double.NEGATIVE_INFINITY;
        if (data != null) {
            for (XYData d : data.getData()) {
                if (d.getY() instanceof Number) {
                    double y = ((Number) d.getY()).doubleValue();
                    max = Math.max(max, y);
                }
            }
        }
        return max;
    }

    @Override
    public void redraw() {

    }

}