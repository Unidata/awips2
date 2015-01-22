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

import java.math.BigInteger;
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
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResourceData;

/**
 * Resource Data for RTKP resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer    Description
 * ------------  ---------- ----------- --------------------------
 * April 4, 2014 1122       sgurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GeoMagRTKpResourceData extends TimeSeriesResourceData {

    private static final int MILLISECONDS_PER_HOUR = 60 * 60 * 1000;

    @XmlAttribute
    private boolean updating = true;

    /**
     * length of plot in hours
     */
    @XmlAttribute
    private int plotLengthInHours = 6;

    private DataTime startTime = null;

    private ArrayList<BigInteger> stationCountsList = new ArrayList<BigInteger>();

    public GeoMagRTKpResourceData() {
        super();
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        if (objects.length > 0) {
            AbstractTimeSeriesAdapter<?> adapter = getAdapter(objects[0]);
            adapter.setResourceData(this);
            GeoMagRTKpResource resource = new GeoMagRTKpResource(this,
                    loadProperties, adapter);
            // for (PluginDataObject object : objects) {
            // resource.addRecord(object);
            // }
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

    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        DataTime[] times = super.getAvailableTimes();
        times = filterTimes(times, startTime, getEndTime());
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

    @Override
    public void update(Object updateData) {
        // do nothing
    }

    public void setUpdating(boolean updating) {
        this.updating = updating;
    }

    public ArrayList<BigInteger> getStationCountsList() {
        return stationCountsList;
    }

    public void setStationCountsList(ArrayList<BigInteger> stationCountsList) {
        this.stationCountsList = stationCountsList;
    }
}
