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
package gov.noaa.nws.ncep.viz.timeseries.rsc;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResourceData;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 30, 2013            sgilbert     Initial creation
 * 
 * </pre>
 * 
 * @author sgilbert
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GeoMagResourceData extends TimeSeriesResourceData {

    private static final int MILLISECONDS_PER_HOUR = 60 * 60 * 1000;

    @XmlAttribute
    private boolean updating = true;

    @XmlAttribute
    /**
     * length of plot in hours
     */
    private int plotLengthInHours = 12;

    // private DataTime startTime = new DataTime("2013-05-19_00:00:00.000");
    private DataTime startTime = null;

    /**
     * 
     */
    public GeoMagResourceData() {
        super();
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        if (objects.length > 0) {
            AbstractTimeSeriesAdapter<?> adapter = getAdapter(objects[0]);
            adapter.setResourceData(this);
            GeoMagResource resource = new GeoMagResource(this, loadProperties,
                    adapter);
            for (PluginDataObject object : objects) {
                resource.addRecord(object);
            }
            return resource;
        }
        throw new VizException(
                "No Data Available: unable to determine resource type");

    }

    /*
     * Copied here from TimeSeriesResourceData since it is declared private. Can
     * remove this if changed to protected.
     */
    private static final String TIME_SERIES_ADAPTER_EXTENSION = "com.raytheon.uf.viz.xy.timeseries.timeseriesadapter";

    /*
     * Copied here from TimeSeriesResourceData.getAdapter() since it is private.
     * Can remove this if overridden method changed to protected.
     */
    private AbstractTimeSeriesAdapter<?> getAdapter(PluginDataObject object)
            throws VizException {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        if (registry == null) {
            throw new VizException("Error loading ExtensionRegistry");
        }
        IExtensionPoint point = registry
                .getExtensionPoint(TIME_SERIES_ADAPTER_EXTENSION);
        if (point == null) {
            throw new VizException(
                    "Error loading Extension points for Time Series Adapters");
        }
        Map<String, Object> uriFields = RecordFactory.getInstance()
                .loadMapFromUri(object.getDataURI());
        IExtension[] extensions = point.getExtensions();

        for (IExtension ext : extensions) {
            IConfigurationElement[] config = ext.getConfigurationElements();

            for (IConfigurationElement cfg : config) {
                boolean useAdapter = false;
                String targetClass = cfg.getAttribute("class");
                for (Class<?> clazz : object.getClass().getInterfaces()) {
                    if (clazz.getName().equals(targetClass)) {
                        useAdapter = true;
                        break;
                    }
                }
                if (!useAdapter) {
                    for (Class<?> clazz = object.getClass(); clazz != PluginDataObject.class; clazz = clazz
                            .getSuperclass()) {
                        if (clazz.getName().equals(targetClass)) {
                            useAdapter = true;
                            break;
                        }
                    }
                }

                IConfigurationElement[] constraints = cfg
                        .getChildren("constraint");
                for (IConfigurationElement constraint : constraints) {
                    Object value = uriFields
                            .get(constraint.getAttribute("key"));
                    if (value == null) {
                        value = "null";
                    }
                    if (!value.toString().equals(
                            constraint.getAttribute("value"))) {
                        useAdapter = false;
                        break;
                    }
                }
                if (useAdapter) {
                    try {
                        return (AbstractTimeSeriesAdapter<?>) cfg
                                .createExecutableExtension("adapter");
                    } catch (CoreException e) {
                        throw new VizException(
                                "Error constructing Time Series adapter", e);
                    }
                }

            }
        }

        throw new VizException("No Time Series adapter registered for: "
                + object.getClass().getSimpleName());
    }

    public String getStation() {
        RequestConstraint rc = getMetadataMap().get("stationCode");
        return rc.getConstraintValue();
    }

    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        DataTime[] times = super.getAvailableTimes();
        if (updating) {
            this.startTime = calculateStartTime();
        }
        times = filterTimes(times, startTime, getEndTime());
        // System.out.println("START = " + startTime);
        // System.out.println(" END  = " + getEndTime());
        return times;
    }

    /**
     * Given the times, filter them to only return times between given times
     * 
     * @param times
     * @param start
     * @param end
     * @return
     */
    public DataTime[] filterTimes(DataTime[] times, DataTime startTime,
            DataTime endTime) {
        List<DataTime> validTimes = new ArrayList<DataTime>();
        for (DataTime time : times) {
            if (time.compareTo(startTime) >= 0 && time.compareTo(endTime) <= 0) {
                validTimes.add(time);
            }
        }
        return validTimes.toArray(new DataTime[validTimes.size()]);
    }

    private DataTime calculateStartTime() {
        Calendar ttime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        ttime.set(Calendar.MINUTE, 0);
        ttime.set(Calendar.SECOND, 0);
        ttime.set(Calendar.MILLISECOND, 0);
        int currentHour = ttime.get(Calendar.HOUR_OF_DAY);
        // Find next synoptic time as endtime
        int hoursToAdd = 3 - (currentHour % 3);
        ttime.add(Calendar.HOUR_OF_DAY, hoursToAdd);
        // subtract plot length time to get starttime
        ttime.add(Calendar.HOUR_OF_DAY, -plotLengthInHours);
        // long tmilli = ttime.getTimeInMillis();
        // tmilli -= plotLengthInHours * MILLISECONDS_PER_HOUR;

        return new DataTime(ttime.getTime());
    }

    public int getPlotLengthInHours() {
        return plotLengthInHours;
    }

    public void setPlotLengthInHours(int duration) {
        this.plotLengthInHours = duration;
    }

    public DataTime getStartTime() {
        return startTime;
    }

    public void setStartTime(DataTime startTime) {
        this.startTime = startTime;
    }

    public DataTime getEndTime() {
        DataTime endTime = null;
        if (startTime != null) {
            long stime = startTime.getRefTime().getTime();
            // add in milliseconds
            stime += plotLengthInHours * MILLISECONDS_PER_HOUR;
            endTime = new DataTime(new Date(stime));
        }
        return endTime;
    }

    public boolean isUpdating() {
        return updating;
    }

    public void setUpdating(boolean updating) {
        this.updating = updating;
    }

}
