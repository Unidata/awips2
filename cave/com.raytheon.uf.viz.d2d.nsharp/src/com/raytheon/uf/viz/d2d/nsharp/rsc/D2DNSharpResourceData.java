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
package com.raytheon.uf.viz.d2d.nsharp.rsc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TimeZone;
import java.util.TreeSet;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.locationtech.jts.geom.Coordinate;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.geospatial.adapter.CoordAdapter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.FLAG;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.d2d.core.procedures.IPointsToolContainer;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpDataHandling;

/**
 * 
 * A requestable resource data for ncep nsharp resources.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 12, 2011           bsteffen  Initial creation
 * May 31, 2013  1847     bsteffen  D2D nsharp will now format Lat/Lons as
 *                                  stationId like NC ncharp.
 * May 08, 2014  2060     njensen   Constructor sets alert parser
 * Jul 23, 2014  3410     bclement  added unpackResultLocation()
 * Aug 05, 2015  4486     rjpeter   Changed Timestamp to Date.
 * Mar 01, 2016  14647    mgamazay  Added soundingTitle.
 * Mar 28, 2018  6800     bsteffen  Implement IPointsToolContainer.
 * Oct 05, 2018  7480     bsteffen  Fix updates and removes.
 * Apr 15, 2019  7596     tgurney   Added xml adapter to Coordinate class as
 *                                  part of the Geotools upgrade.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class D2DNSharpResourceData extends
        AbstractRequestableResourceData implements IPointsToolContainer {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(D2DNSharpResourceData.class);

    private static final String UNKNOWN = "UNKNOWN";

    @XmlAttribute
    protected String soundingType = UNKNOWN;

    @XmlAttribute
    protected String soundingTitle = UNKNOWN;

    @XmlElement
    @XmlJavaTypeAdapter(value = CoordAdapter.class)
    protected Coordinate coordinate;

    @XmlAttribute
    protected String pointName;

    public D2DNSharpResourceData() {
        super();
    }

    public D2DNSharpResourceData(String soundingType) {
        this();
        this.soundingType = soundingType;
    }

    public D2DNSharpResourceData(String soundingType, String soundingTitle) {
        this();
        this.soundingType = soundingType;
        this.soundingTitle = soundingTitle;
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        D2DNSharpResource skewRsc = new D2DNSharpResource(this, loadProperties);
        // sort the objects so that the latest datatime gets loaded first.
        Arrays.sort(objects, new Comparator<PluginDataObject>() {

            @Override
            public int compare(PluginDataObject o1, PluginDataObject o2) {
                Long v1 = o1.getDataTime().getMatchValid();
                Long v2 = o2.getDataTime().getMatchValid();
                return v2.compareTo(v1);
            }
        });
        for (PluginDataObject pdo : objects) {
            if (pdo instanceof D2DNSharpDataObject) {
                skewRsc.addDataObject((D2DNSharpDataObject) pdo);
            }
        }
        return skewRsc;
    }

    @Override
    protected void update(AlertMessage... messages) {
        SortedSet<DataTime> timeSet = new TreeSet<>();
        for (AlertMessage message : messages) {
            Object time = message.decodedAlert
                    .get(PluginDataObject.DATATIME_ID);
            if (time instanceof DataTime) {
                timeSet.add((DataTime) time);
            }
        }
        DataTime[] timesArray = timeSet.toArray(new DataTime[0]);
        try {
            PluginDataObject[] newPDOs = getLatestPluginDataObjects(timesArray,
                    new DataTime[0]);
            update(newPDOs);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error updating NSharp display", e);
            fireChangeListeners(ChangeType.DATA_REMOVE, timesArray);
        }
    }

    @Override
    public PluginDataObject[] getLatestPluginDataObjects(DataTime[] desired,
            DataTime[] current) throws VizException {
        // retrieve data is set to false so that the metadataMap is not checked,
        // there needs to be a better way of doing this.
        this.retrieveData = false;
        preparePointInfo();
        List<D2DNSharpDataObject> data = new ArrayList<>(desired.length);
        for (DataTime time : desired) {
            if (time == null) {
                continue;
            }
            boolean found = false;
            for (DataTime cur : current) {
                if (time.equals(cur)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                NsharpStationInfo stnInfo = createStationInfo(time);
                D2DNSharpDataObject dataObj = new D2DNSharpDataObject();
                dataObj.setDataTime(time);
                dataObj.setStationInfo(stnInfo);
                data.add(dataObj);
            }

        }
        return data.toArray(new D2DNSharpDataObject[0]);
    }

    protected abstract void preparePointInfo() throws VizException;

    /**
     * Creates a new coordinate from query result map
     * 
     * @see DbQueryResponse#getResults()
     * @param result
     * @param lonKey
     *            longitude field
     * @param latKey
     *            latitude field
     * @return
     */
    protected static Coordinate unpackResultLocation(Map<String, Object> result,
            String lonKey, String latKey) {
        Coordinate rval = new Coordinate();
        rval.x = ((Number) result.get(lonKey)).doubleValue();
        rval.y = ((Number) result.get(latKey)).doubleValue();
        return rval;
    }

    protected abstract NcSoundingCube getSoundingCube(
            NsharpStationInfo stnInfo);

    protected void populateDataObject(D2DNSharpDataObject dataObject) {
        NcSoundingCube cube = getSoundingCube(dataObject.getStationInfo());
        if ((cube == null) || cube.getSoundingProfileList().isEmpty()) {
            return;
        }
        NcSoundingProfile profileList = cube.getSoundingProfileList().get(0);
        if (profileList == null) {
            return;
        }
        List<NcSoundingLayer> layers = profileList.getSoundingLyLst();
        layers = NsharpDataHandling.organizeSoundingDataForShow(layers,
                profileList.getStationElevation());
        if ((layers != null) && (layers.size() < 3)) {
            // set invalid data to null
            layers = null;
        }
        dataObject.setLayers(layers);
    }

    protected NsharpStationInfo createStationInfo(DataTime time) {
        NsharpStationInfo stnInfo = new NsharpStationInfo();
        stnInfo.setSndType(getPreferedSoundingTitle());
        Date refTime = new Date(time.getRefTime().getTime());
        stnInfo.setReftime(refTime);
        Date fcstTime = refTime;
        if (time.getUtilityFlags().contains(FLAG.FCST_USED)) {
            fcstTime = new Date(time.getValidPeriod().getStart().getTime());
            stnInfo.setRangestarttime(fcstTime);
        }
        String pointName = this.pointName;
        if (coordinate != null) {
            stnInfo.setLongitude(coordinate.x);
            stnInfo.setLatitude(coordinate.y);
            if (pointName == null) {
                pointName = String.format("%.2f/%.2f", coordinate.y,
                        coordinate.x);
            }
        }
        if (pointName != null) {
            stnInfo.setStnDisplayInfo(
                    pointName + " " + formatTimestamp(fcstTime));
            stnInfo.setStnId(pointName);
        }
        return stnInfo;
    }

    protected String formatTimestamp(Date time) {
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTimeInMillis(time.getTime());
        return String.format("%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS", cal);
    }

    public String getSoundingType() {
        return soundingType;
    }

    public void setSoundingType(String soundingType) {
        this.soundingType = soundingType;
    }

    public String getSoundingTitle() {
        return soundingTitle;
    }

    public void setSoundingTitle(String soundingTitle) {
        this.soundingTitle = soundingTitle;
    }

    /**
     * Safe method for getting the sounding title. If the title is null or
     * unknown then the sounding type is returned. This method never returns
     * null, instead returning UNKNOWN if both the title and type are null.
     * 
     * @return a title.
     */
    public String getPreferedSoundingTitle() {
        if (this.soundingTitle != null
                && !UNKNOWN.equalsIgnoreCase(this.soundingTitle)) {
            return this.soundingTitle;
        } else if (soundingType != null) {
            return this.soundingType;
        } else {
            return UNKNOWN;
        }

    }

    public Coordinate getCoordinate() {
        return coordinate;
    }

    public void setCoordinate(Coordinate coordinate) {
        this.coordinate = coordinate;
    }

    public String getPointName() {
        return pointName;
    }

    public void setPointName(String pointName) {
        this.pointName = pointName;
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
        this.pointName = "Point" + pointLetter;
    }

    @Override
    public String getPointLetter() {
        if (this.pointName != null && this.pointName.startsWith("Point")) {
            return this.pointName.substring(5);
        } else {
            return this.pointName;
        }
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = (prime * result)
                + ((coordinate == null) ? 0 : coordinate.hashCode());
        result = (prime * result)
                + ((pointName == null) ? 0 : pointName.hashCode());
        result = (prime * result)
                + ((soundingType == null) ? 0 : soundingType.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        D2DNSharpResourceData other = (D2DNSharpResourceData) obj;
        if (coordinate == null) {
            if (other.coordinate != null) {
                return false;
            }
        } else if (!coordinate.equals(other.coordinate)) {
            return false;
        }
        if (pointName == null) {
            if (other.pointName != null) {
                return false;
            }
        } else if (!pointName.equals(other.pointName)) {
            return false;
        }
        if (soundingType == null) {
            if (other.soundingType != null) {
                return false;
            }
        } else if (!soundingType.equals(other.soundingType)) {
            return false;
        }
        return true;
    }

}
