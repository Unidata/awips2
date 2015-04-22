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

import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IPointsToolContainer;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/24/2014   #1136       qzhou      Initial creation
 * 07/28/2014   R4079       sgurung    Added showReadoutView and showKTableView
 * 
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "GeoMagResourceData")
public class GeoMagResourceData extends
        AbstractNatlCntrsRequestableResourceData implements
        IPointsToolContainer {

    private static final String stationCodeParam = "stationCode";

    private static final String sourceIdParam = "sourceId";

    @XmlElement
    private String sourceId;

    @XmlElement
    private String yAxesData;

    @XmlElement
    private String xAxesTitle;

    @XmlElement
    private String yAxesTitle;

    @XmlElement
    private String yDescription;

    @XmlElement
    private String dataColor;

    @XmlElement
    private Coordinate coordinate;

    @XmlElement
    private boolean showReadoutView;

    @XmlElement
    private boolean showKTableView;

    protected DataTime startTime;

    protected DataTime endTime;

    /**
     * 
     */
    public GeoMagResourceData() {
        super();

        // called by AbstractVizResource.getName()
        // and we delegate back to the resource
        this.nameGenerator = new AbstractNameGenerator() {

            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                return ((GeoMagResource) resource).getLegendStr();
            }
        };
    }

    @Override
    public NcDisplayType[] getSupportedDisplayTypes() {
        return new NcDisplayType[] { NcDisplayType.GRAPH_DISPLAY };
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {

        return new GeoMagResource(this, loadProperties);
    }

    /*
     * Copied here from TimeSeriesResourceData since it is declared private. Can
     * remove this if changed to protected.
     */
    private static final String TIME_SERIES_ADAPTER_EXTENSION = "com.raytheon.uf.viz.xy.timeseries.timeseriesadapter";

    public String getStation() {
        RequestConstraint rc = getMetadataMap().get("stationCode");
        return rc.getConstraintValue();
    }

    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        DataTime[] times = super.getAvailableTimes();
        // if (updating) {
        // this.startTime = calculateStartTime();
        // }
        // times = filterTimes(times, startTime, getEndTime()); quan start
        // =current time
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

    // private DataTime calculateStartTime() {
    // Calendar ttime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
    // ttime.set(Calendar.MINUTE, 0);
    // ttime.set(Calendar.SECOND, 0);
    // ttime.set(Calendar.MILLISECOND, 0);
    // int currentHour = ttime.get(Calendar.HOUR_OF_DAY);
    // // Find next synoptic time as endtime
    // int hoursToAdd = 3 - (currentHour % 3);
    // ttime.add(Calendar.HOUR_OF_DAY, hoursToAdd);
    // // subtract plot length time to get starttime
    // ttime.add(Calendar.HOUR_OF_DAY, -plotLengthInHours);
    // // long tmilli = ttime.getTimeInMillis();
    // // tmilli -= plotLengthInHours * MILLISECONDS_PER_HOUR;
    //
    // return new DataTime(ttime.getTime());
    // }
    public DataTime getStartTime() {
        return startTime;
    }

    public void setStartTime(DataTime startTime) {
        this.startTime = startTime;
    }

    // public boolean isUpdating() {
    // return updating;
    // }
    //
    // public void setUpdating(boolean updating) {
    // this.updating = updating;
    // }

    public String getStationCode() {
        return (metadataMap.containsKey(stationCodeParam) ? metadataMap.get(
                stationCodeParam).getConstraintValue() : "");
    }

    public String getSourceId() {
        return (metadataMap.containsKey(sourceIdParam) ? metadataMap.get(
                sourceIdParam).getConstraintValue() : "");
    }

    public String getYAxesData() {
        return yAxesData;
    }

    public void setYAxesData(String yAxesData) {
        this.yAxesData = yAxesData;
    }

    public String getXAxesTitle() {
        return xAxesTitle;
    }

    public void setXAxesTitle(String xAxesTitle) {
        this.xAxesTitle = xAxesTitle;
    }

    public String getYAxesTitle() {
        return yAxesTitle;
    }

    public void setYAxesTitle(String yAxesTitle) {
        this.yAxesTitle = yAxesTitle;
    }

    public String getYDescription() {
        return yDescription;
    }

    public void setYDescription(String yDescription) {
        this.yDescription = yDescription;
    }

    public String getDataColor() {
        return dataColor;
    }

    public void setDataColor(String dataColor) {
        this.dataColor = dataColor;
    }

    @Override
    public void setPointCoordinate(Coordinate pointCoordinate) {
        this.coordinate = pointCoordinate;
    }

    @Override
    public Coordinate getPointCoordinate() {
        return coordinate;
    }

    @Override
    public void setPointLetter(String pointLetter) {
        // this.pointLetter = pointLetter;
    }

    @Override
    public String getPointLetter() {
        return null;
    }

    public boolean getShowReadoutView() {
        return showReadoutView;
    }

    public void setShowReadoutView(boolean showReadoutView) {
        this.showReadoutView = showReadoutView;
    }

    public boolean getShowKTableView() {
        return showKTableView;
    }

    public void setShowKTableView(boolean showKTableView) {
        this.showKTableView = showKTableView;
    }

}
