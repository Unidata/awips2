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

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpDataHandling;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Comparator;
import java.util.List;
import java.util.TimeZone;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.FLAG;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * A requestable resource data for ncep nsharp resources.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 12, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class D2DNSharpResourceData extends
        AbstractRequestableResourceData {

    @XmlAttribute
    protected String soundingType = "UNKNOWN";

    @XmlElement
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
    public PluginDataObject[] getLatestPluginDataObjects(DataTime[] desired,
            DataTime[] current) throws VizException {
        // retrieve data is set to false so that the metadataMap is not checked,
        // there needs to be a better way of doing this.
        this.retrieveData = false;
        preparePointInfo();
        List<D2DNSharpDataObject> data = new ArrayList<D2DNSharpDataObject>(
                desired.length);
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

    protected abstract NcSoundingCube getSoundingCube(NsharpStationInfo stnInfo);

    protected void populateDataObject(D2DNSharpDataObject dataObject) {
        NcSoundingCube cube = getSoundingCube(dataObject.getStationInfo());
        if (cube == null || cube.getSoundingProfileList().isEmpty()) {
            return;
        }
        NcSoundingProfile profileList = cube.getSoundingProfileList().get(0);
        if (profileList == null) {
            return;
        }
        List<NcSoundingLayer> layers = profileList.getSoundingLyLst();
        layers = NsharpDataHandling.organizeSoundingDataForShow(layers,
                profileList.getStationElevation());
        // TODO remove this, as it only exists to help resolve crashes.
        logSoundingFiles(dataObject.getStationInfo(), layers);
        dataObject.setLayers(layers);
    }

    protected NsharpStationInfo createStationInfo(DataTime time) {
        NsharpStationInfo stnInfo = new NsharpStationInfo();
        stnInfo.setSndType(soundingType);
        Timestamp refTime = new Timestamp(time.getRefTime().getTime());
        stnInfo.setReftime(refTime);
        Timestamp fcstTime = refTime;
        if (time.getUtilityFlags().contains(FLAG.FCST_USED)) {
            fcstTime = new Timestamp(time.getValidPeriod().getStart().getTime());
            stnInfo.setRangestarttime(fcstTime);
        }
        if (coordinate != null) {
            stnInfo.setLongitude((float) coordinate.x);
            stnInfo.setLatitude((float) coordinate.y);
        }
        if (pointName != null) {
            stnInfo.setStnDisplayInfo(pointName + " "
                    + formatTimestamp(fcstTime));
        } else {
            stnInfo.setStnDisplayInfo(formatTimestamp(fcstTime));
        }
        return stnInfo;
    }

    protected String formatTimestamp(Timestamp time) {

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
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((coordinate == null) ? 0 : coordinate.hashCode());
        result = prime * result
                + ((pointName == null) ? 0 : pointName.hashCode());
        result = prime * result
                + ((soundingType == null) ? 0 : soundingType.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        D2DNSharpResourceData other = (D2DNSharpResourceData) obj;
        if (coordinate == null) {
            if (other.coordinate != null)
                return false;
        } else if (!coordinate.equals(other.coordinate))
            return false;
        if (pointName == null) {
            if (other.pointName != null)
                return false;
        } else if (!pointName.equals(other.pointName))
            return false;
        if (soundingType == null) {
            if (other.soundingType != null)
                return false;
        } else if (!soundingType.equals(other.soundingType))
            return false;
        return true;
    }

    // TODO this purges debugging files, it can be removed when debugging is
    // removed
    private static long nextPurgeTime = 0;

    /**
     * 
     * This function exists only to help identify data that can crash nsharp, it
     * can be removed once nsharp is considered more stable.
     * 
     * @param info
     * @param layers
     */
    private static void logSoundingFiles(NsharpStationInfo info,
            List<NcSoundingLayer> layers) {
        if (layers.isEmpty()) {
            return;
        }
        try {
            File caveData = new File(Platform.getUserLocation().getURL()
                    .getPath());
            File logs = new File(caveData, "logs");
            if (!logs.exists()) {
                logs.mkdir();
            }
            File nsharpFiles = new File(logs, "nsharpFiles");
            if (!nsharpFiles.exists()) {
                nsharpFiles.mkdir();
            }
            if (nextPurgeTime < System.currentTimeMillis()) {
                // delete anything more than 3 hours old.
                long lastValidTime = System.currentTimeMillis() - 3 * 60 * 60
                        * 1000l;
                for (File file : nsharpFiles.listFiles()) {
                    if (file.lastModified() < lastValidTime) {
                        file.delete();
                    }
                }
                // run again in an hour.
                nextPurgeTime = System.currentTimeMillis() + 60 * 60 * 1000l;
            }

            // EGM: Win32 can't have ':' in filenames
            String logFilename = info.getStnDisplayInfo() + ".nsp";
            logFilename = logFilename.replace(":", "_");
            File nspFile = new File(nsharpFiles, logFilename);

            FileWriter fstream = new FileWriter(nspFile);
            BufferedWriter out = new BufferedWriter(fstream);
            String textToSave = new String("");

            List<NcSoundingLayer> soundLyList = layers;
            String latlonstr;
            latlonstr = "  LAT=" + info.getLatitude() + " LON="
                    + info.getLongitude();
            textToSave = info.getSndType()
                    + " "
                    + info.getSndType()
                    + "  "
                    + info.getStnDisplayInfo()
                    + latlonstr
                    + "\n"
                    + "PRESSURE  HGHT\t   TEMP\t  DWPT    WDIR     WSPD    OMEG\n";

            String tempText = "";
            for (NcSoundingLayer layer : soundLyList) {
                tempText = String.format("%f  %f  %f  %f  %f  %f  %f\n",
                        layer.getPressure(), layer.getGeoHeight(),
                        layer.getTemperature(), layer.getDewpoint(),
                        layer.getWindDirection(), layer.getWindSpeed(),
                        layer.getOmega());
                textToSave = textToSave + tempText;
            }
            out.write(textToSave);
            // Close the output stream
            out.close();
            nspFile.deleteOnExit();
        } catch (Throwable e) {
            // Ignore all errors, this operation does not need to complete
            // normally if something goes wrong
            e.printStackTrace();
        }
    }

}
