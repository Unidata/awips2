/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.viz.rtkp.rsc;

import gov.noaa.nws.ncep.viz.rtkp.KpPlotCapability;
import gov.noaa.nws.ncep.viz.rtkp.KsPlotCapability;
import gov.noaa.nws.ncep.viz.rtkp.util.GeoMagTimeSeriesDataException;
import gov.noaa.nws.ncep.viz.rtkp.util.RTKpTimeSeriesZoomHandler;
import gov.noaa.nws.ncep.viz.rtkp.util.RTKpUtil;

import java.math.BigInteger;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;
import java.util.TreeSet;

import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.xy.timeseries.TimeSeriesEditor;
import com.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResource;
import com.raytheon.viz.core.graphing.util.GraphPrefsFactory;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYDataList;
import com.raytheon.viz.core.rsc.ICombinedResourceData.CombineOperation;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Provides a resource for displaying RTKP time series plot.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer    Description
 * ------------  ---------- ----------- --------------------------
 * March 3, 2014 1122       sgurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class GeoMagRTKpResource extends TimeSeriesResource {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GeoMagRTKpResource.class);

    public static SimpleDateFormat sdf = new SimpleDateFormat(
            "HHmm 'UTC' dd MMM yyyy");

    private static Unit<?> unit = SI.NANO(SI.TESLA);

    static final long ONE_MINUTE_IN_MILLIS = 60000;// millisecs

    private Set<DataTime> dataTimes = new TreeSet<DataTime>();

    protected Timer timer;

    protected TimerTask timerTask;

    protected boolean loadDone = false;

    protected Date prevSynPerStartTime = null;

    protected Date updatedDate = null;

    /**
     * @param data
     * @param props
     * @param adapter
     */
    public GeoMagRTKpResource(GeoMagRTKpResourceData data,
            LoadProperties props, AbstractTimeSeriesAdapter<?> adapter) {
        super(data, props, adapter);

        data.addChangeListener(new IResourceDataChanged() {

            @Override
            public void resourceChanged(ChangeType type, Object object) {
                // do nothing
            }

        });

        if (RTKpUtil.KS_PLOT.equals(resourceData.getSource())) {
            getCapabilities().addCapability(KsPlotCapability.class);
            getCapabilities().removeCapability(KpPlotCapability.class);
        } else if (RTKpUtil.KP_PLOT.equals(resourceData.getSource())) {
            getCapabilities().addCapability(KpPlotCapability.class);
            getCapabilities().removeCapability(KsPlotCapability.class);
        }

        loadData();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {

        if (secondaryResource != null) {
            secondaryResource.setDescriptor(this.descriptor);
            secondaryResource.init(target);
        }

        constructDataTimer();

        // Load the Graph Preferences
        if (prefs == null) {
            try {

                SingleLevel level = new SingleLevel("SURFACE");
                level.setValue(0.0);

                prefs = GraphPrefsFactory.buildPreferences(
                        resourceData.getYParameter().code, level);
            } catch (StyleException e) {
                throw new VizException(e.getLocalizedMessage(), e);
            }
        }

        if (prefs != null && prefs.getDisplayUnits() != null) {
            units = prefs.getDisplayUnitLabel();
        }

        prevSynPerStartTime = ((GeoMagRTKpResourceData) resourceData)
                .getStartTime().getRefTime();

        updatedDate = RTKpUtil.getCurrentTime();

        if (EditorUtil.getActiveEditor() != null) {
            if (EditorUtil.getActiveEditor() instanceof TimeSeriesEditor) {
                TimeSeriesEditor editor = (TimeSeriesEditor) EditorUtil
                        .getActiveEditor();

                // Register RTKpTimeSeriesZoomHandler mouse handler that does
                // nothing when mouse wheel moves
                RTKpTimeSeriesZoomHandler zoomHandler = new RTKpTimeSeriesZoomHandler(
                        this.descriptor.getRenderableDisplay());
                editor.registerMouseHandler(zoomHandler);
            }
        }
    }

    protected void constructDataTimer() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                timer = new Timer("rtkpDataRetrieve");
                timerTask = new TimerTask() {
                    @Override
                    public void run() {
                        // try {

                        if (prevSynPerStartTime != null) {
                            Date newSynPerStartTime = RTKpUtil
                                    .calcPrevSynPerStartTime();

                            if (!prevSynPerStartTime.equals(newSynPerStartTime)) {
                                ((GeoMagRTKpResourceData) resourceData)
                                        .setStartTime(new DataTime(
                                                newSynPerStartTime));
                                prevSynPerStartTime = newSynPerStartTime;
                            }
                        }

                        updatedDate = RTKpUtil.getCurrentTime();
                        loadData();
                        loadDone = true;

                        // updatedDate = RTKpUtil.getCurrentTime();
                        // issueRefresh();
                    }
                };
                // update data every 10 seconds
                timer.schedule(timerTask, 0, 10000);
            }
        });
    }

    protected void loadData() {

        XYDataList oldData = data;
        try {
            data = getXYDataList();
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Error loading data.", e);
        }
        sortData();

        dataTimes = new TreeSet<DataTime>();
        for (XYData d : data.getData()) {
            dataTimes.add((DataTime) d.getX());
        }
        dataTimes = new TreeSet<DataTime>();
        for (XYData d : data.getData()) {
            dataTimes.add((DataTime) d.getX());
        }

        if (graph != null) {
            graph.reconstruct();
        }
        if (oldData != null) {
            oldData.dispose();
        }

        issueRefresh();
    }

    public XYDataList getXYDataList() throws VizException {
        ArrayList<XYData> xyData = new ArrayList<XYData>();

        if ((GeoMagRTKpResourceData) resourceData != null
                && ((GeoMagRTKpResourceData) resourceData).getStartTime() != null) {
            try {

                Date startTime = ((GeoMagRTKpResourceData) resourceData)
                        .getStartTime().getRefTime();

                Date endTime = ((GeoMagRTKpResourceData) resourceData)
                        .getEndTime().getRefTime();

                if (endTime == null) {
                    Calendar cal = ((GeoMagRTKpResourceData) resourceData)
                            .getStartTime().getRefTimeAsCalendar();
                    cal.add(Calendar.HOUR, 6);
                    endTime = cal.getTime();
                }
                SimpleDateFormat f = new SimpleDateFormat(
                        "yyyy-MMM-dd HH:mm:ss");
                f.setTimeZone(TimeZone.getTimeZone("GMT"));

                List<Map<String, Object>> rsltsList = RTKpUtil
                        .getEstKpIndex1min(startTime, endTime);

                int rsltsListSize = (rsltsList != null) ? rsltsList.size() : 0;

                ArrayList<BigInteger> stationCountsList = new ArrayList<BigInteger>();

                for (int i = 0; i < rsltsListSize; i++) {
                    Map<String, Object> map = rsltsList.get(i);
                    Date refTime = (Date) map.get("reftime");
                    Double ks_avg = (Double) map.get("ks_avg");
                    Double Kp_est = (Double) map.get("Kp_est");
                    BigInteger station_count = (BigInteger) map
                            .get("station_count");
                    stationCountsList.add(station_count);

                    DataTime x = new DataTime(refTime);

                    if (RTKpUtil.KP_PLOT.equals(resourceData.getSource())) {
                        xyData.add(new XYData(x, Kp_est));
                    } else {
                        xyData.add(new XYData(x, ks_avg));
                    }
                }

                ((GeoMagRTKpResourceData) resourceData)
                        .setStationCountsList(stationCountsList);

            } catch (GeoMagTimeSeriesDataException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error retrieving data from geomag_k1min table.", e);
            }
        }

        XYDataList list = new XYDataList();
        list.setData(xyData);

        return list;
    }

    /**
     * 
     */
    private void sortData() {
        Collections.sort(data.getData(), new Comparator<XYData>() {

            @Override
            public int compare(XYData xy1, XYData xy2) {
                DataTime t1 = (DataTime) xy1.getX();
                DataTime t2 = (DataTime) xy2.getX();
                return t1.compareTo(t2);
            }

        });

    }

    public DataTime[] getDataTimes() {
        return dataTimes.toArray(new DataTime[0]);
    }

    @Override
    public void addRecord(PluginDataObject record) {
        // do nothing
    }

    @Override
    public Object getGraphKey() {
        return resourceData.getSource();
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        if (RTKpUtil.KS_PLOT.equals(resourceData.getSource())) {
            getCapabilities().removeCapability(KpPlotCapability.class);
        } else if (RTKpUtil.KP_PLOT.equals(resourceData.getSource())) {
            getCapabilities().removeCapability(KsPlotCapability.class);
        }

        if (data == null) {
            return;
        }

        if (secondaryResource != null) {
            secondaryResource.paint(target, paintProps);
        }

        if (combineOperation == CombineOperation.NONE) {
            return;
        }

        if (graph == null) {
            graph = descriptor.getGraph(this);
        }

        if (graph == null) {
            return;
        }

        // Wait for graph to initialize before plotting to it, TODO: do better
        if (graph.isReady() == false) {
            return;
        }

        ArrayList<BigInteger> stationCountsList = ((GeoMagRTKpResourceData) resourceData)
                .getStationCountsList();

        target.setupClippingPlane(graph.getExtent());
        double[] prevScreen = null;
        for (int i = 0; i < data.getData().size(); i++) {

            XYData point = data.getData().get(i);

            if (point.getY() != null) {
                // double newY = toDeltanT.convert(((Number) point.getY())
                // .doubleValue());

                double[] screen = getScreenPosition(point.getX(), point.getY());

                RGB color = RGBColors.getRGBColor("white");
                float pointSize = 1.75f;
                PointStyle pointStyle = PointStyle.POINT;

                // Connects adjacent data points with a line
                if (prevScreen != null) {

                    if (RTKpUtil.KS_PLOT.equals(resourceData.getSource())) {
                        if (stationCountsList != null
                                && stationCountsList.size() > 0) {
                            int colIndex = stationCountsList.get(i).intValue();
                            color = RTKpUtil.getStationCountColor(
                                    colIndex,
                                    getCapabilities().getCapability(
                                            resourceData,
                                            KsPlotCapability.class)
                                            .getPlotColors());
                        }
                        pointSize = (float) Float.parseFloat(getCapability(
                                KsPlotCapability.class).getPointSize());
                        pointStyle = PointStyle.valueOf(getCapability(
                                KsPlotCapability.class).getPointStyle());
                    } else {
                        color = getCapability(KpPlotCapability.class)
                                .getPlotColor();
                        pointSize = (float) Float.parseFloat(getCapability(
                                KpPlotCapability.class).getPointSize());
                        pointStyle = PointStyle.valueOf(getCapability(
                                KpPlotCapability.class).getPointStyle());
                    }

                    target.drawPoint(screen[0], screen[1], 0.0, color,
                            pointStyle, pointSize);

                }

                prevScreen = screen;
            }
        }
        target.clearClippingPlane();
    }

    @Override
    public void paintInsetMap(IGraphicsTarget target,
            PaintProperties paintProps, MapDescriptor insetMapDescriptor)
            throws VizException {

        // Paint the border (in black to override the previous border painted in
        // grey)
        IExtent extent = paintProps.getView().getExtent();

        target.drawRect(extent, new RGB(0, 0, 0), 2.0f, 0.0);
        paintProps.setAlpha(0.0f);
    }

    private double[] getScreenPosition(Object x, Object y) {
        double valY = ((Number) y).doubleValue();
        double valX = ((DataTime) x).getValidTime().getTimeInMillis();
        return graph.getGridLocation(valX, valY);
    }

    public DataTime getStartTime() {
        return ((GeoMagRTKpResourceData) resourceData).getStartTime();
    }

    public DataTime getEndTime() {
        return ((GeoMagRTKpResourceData) resourceData).getEndTime();
    }

    public Timer getTimer() {
        return timer;
    }

    public void setTimer(Timer timer) {
        this.timer = timer;
    }

    public TimerTask getTimerTask() {
        return timerTask;
    }

    public void setTimerTask(TimerTask timerTask) {
        this.timerTask = timerTask;
    }

    public boolean isLoadDone() {
        return loadDone;
    }

    public void setLoadDone(boolean loadDone) {
        this.loadDone = loadDone;
    }

    public Date getPrevSynPerStartTime() {
        return prevSynPerStartTime;
    }

    public void setPrevSynPerStartTime(Date prevSynPerStartTime) {
        this.prevSynPerStartTime = prevSynPerStartTime;
    }

    public Date getUpdatedDate() {
        return updatedDate;
    }

    public void setUpdatedDate(Date updatedDate) {
        this.updatedDate = updatedDate;
    }

    @Override
    public String getName() {
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        StringBuilder sb = new StringBuilder(resourceData.getSource());
        if (data.getData().size() == 0)
            sb.append(" - NO DATA");
        return sb.toString();
    }

    @Override
    public String[] getTitles() {
        return new String[] { "NOAA estimated real-time Kp index" };
    }

    /**
     * Called when resource is disposed
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @Override
    public void disposeInternal() {
        resourceData = null;
        timerTask.cancel();
        timer.cancel();
    }
}
