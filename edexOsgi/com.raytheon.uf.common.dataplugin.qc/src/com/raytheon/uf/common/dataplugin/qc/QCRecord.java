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

package com.raytheon.uf.common.dataplugin.qc;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Record class for QC mesonet data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 07, 2009 3408       bphillip    Initial creation
 * Apr 04, 2013 1846       bkowal      Added an index on refTime and
 *                                     forecastTime
 * Apr 12, 2013 1857       bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * May 16, 2013 1869       bsteffen    Remove DataURI column from qc.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Feb 27, 2014 2852       rferrel     Add getter/setter to FakePointDataView.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "qcseq")
@Table(name = "qc", uniqueConstraints = { @UniqueConstraint(columnNames = {
        "stationid", "reftime", "qcType", "latitude", "longitude" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "qc", indexes = { @Index(name = "qc_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@DynamicSerialize
public class QCRecord extends PluginDataObject implements ISpatialEnabled {

    private static final long serialVersionUID = -8836262244188895665L;

    @Transient
    private Date invTime;

    /** Time to process stages 1 and 2 */
    @Transient
    private int secondsStage1_2;

    /** Time to process stage 3 */
    @Transient
    private int secondsStage3;

    /** Data provider station id */
    @Transient
    private String providerId;

    /** Home WFO id */
    @Transient
    private String homeWFO;

    /** Numeric WMO identification */
    @Transient
    private int numericWMOid;

    /** LDAD Station Type */
    @Transient
    private String stationType;

    /** Local data provider */
    @Transient
    private String dataProvider;

    @Transient
    private int filterSetNum;

    /** List of possible QC checks */
    @Transient
    private String qct;

    /** List of possible IC checks */
    @Transient
    private String ict;

    @Embedded
    @DataURI(position = 2, embedded = true)
    @DynamicSerializeElement
    private SurfaceObsLocation location; // latitude, longitude, elevation,

    @Column(nullable = false, length = 20)
    @DataURI(position = 1)
    @DynamicSerializeElement
    private String qcType;

    /** Data Platform Type */
    @Transient
    private int dataPlatformType;

    /** Data Platform true direction */
    @Transient
    private int platformTrueDirection;

    /** Data platform true speed */
    @Transient
    private int platformTrueSpeed;

    /** Time of observation */
    @Transient
    private Date observationTime;

    /** Time data was processed by the provider */
    @Transient
    private Date reportTime;

    /** Time data was received */
    @Transient
    private Date receivedTime;

    /** Temperature reading */
    @Transient
    private float temperature;

    /** Temperature time of last change */
    @Transient
    private Date tempChangeTime;

    /** Temperature QC summary value */
    @Transient
    private String temperatureDD;

    /** Temperature QC applied word */
    @Transient
    private String[] temperatureQCA;

    /** Temperature QC Results word */
    @Transient
    private String[] temperatureQCR;

    /** Temperature QC departures */
    @Transient
    private int[] temperatureQCD;

    /** Temperature IC applied word */
    @Transient
    private String temperatureICA;

    /** Temperature IC results word */
    @Transient
    private String temperatureICR;

    /** Dew Point temperature */
    @Transient
    private float dewpoint;

    /** Dew Point QC summary value */
    @Transient
    private String dewpointDD;

    /** Dew point QC applied word */
    @Transient
    private String[] dewpointQCA;

    /** dew point QC results word */
    @Transient
    private String[] dewpointQCR;

    /** dew point QC departures */
    @Transient
    private int[] dewpointQCD;

    /** dew point IC applied word */
    @Transient
    private String dewpointICA;

    /** dew point IC results word */
    @Transient
    private String dewpointICR;

    /** wet bulb temperature */
    @Transient
    private float wetBulbTemperature;

    /** relative humidity */
    @Transient
    private float relHumidity;

    /** relative humidity time of last change */
    @Transient
    private Date rhChangeTime;

    /** relative humidity QC summary value */
    @Transient
    private String relHumidityDD;

    /** relative humidity QC applied word */
    @Transient
    private String[] relHumidityQCA;

    /** relative humidity QC results word */
    @Transient
    private String[] relHumidityQCR;

    /** relative humidity QC departures */
    @Transient
    private int[] relHumidityQCD;

    /** station pressure */
    @Transient
    private float stationPressure;

    /** station press time of last change */
    @Transient
    private Date stationPressChangeTime;

    /** station pressure QC summary value */
    @Transient
    private String stationPressureDD;

    /** station pressure QC applied word */
    @Transient
    private String[] stationPressureQCA;

    /** station pressure QC results word */
    @Transient
    private String[] stationPressureQCR;

    /** station pressure QC departures */
    @Transient
    private int[] stationPressureQCD;

    /** station pressure IC applied word */
    @Transient
    private String stationPressureICA;

    /** station pressure IC results word */
    @Transient
    private String stationPressureICR;

    /** sea level pressure */
    @Transient
    private float seaLevelPressure;

    /** Sea level pressure QC summary value */
    @Transient
    private String seaLevelPressureDD;

    /** sea level pressure QC applied word */
    @Transient
    private String[] seaLevelPressureQCA;

    /** sea level pressure QC results word */
    @Transient
    private String[] seaLevelPressureQCR;

    /** sea level pressure QC departures */
    @Transient
    private int[] seaLevelPressureQCD;

    /** sea level pressure IC applied word */
    @Transient
    private String seaLevelPressureICA;

    /** sea level pressure IC results word */
    @Transient
    private String seaLevelPressureICR;

    /** character of pressure change */
    @Transient
    private String pressChangeChar;

    /** 3 hour pressure change */
    @Transient
    private float pressChange3Hour;

    /** 3h pressure change QC summary value */
    @Transient
    private String pressChange3HourDD;

    /** 3h pressure change QC applied word */
    @Transient
    private String[] pressChange3HourQCA;

    /** 3h pressure change QC results word */
    @Transient
    private String[] pressChange3HourQCR;

    /** 3h pressure change QC departures */
    @Transient
    private int[] pressChange3HourQCD;

    /** 3h pressure change IC applied word */
    @Transient
    private String pressChange3HourICA;

    /** 3h pressure change IC results word */
    @Transient
    private String pressChange3HourICR;

    /** altimeter setting */
    @Transient
    private float altimeter;

    /** altimeter setting QC summary value */
    @Transient
    private String altimeterDD;

    /** altimeter setting QC applied word */
    @Transient
    private String[] altimeterQCA;

    /** altimeter setting QC results word */
    @Transient
    private String[] altimeterQCR;

    /** altimeter setting QC departures */
    @Transient
    private int[] altimeterQCD;

    /** wind direction */
    @Transient
    private float windDir;

    /** wind direction time of last change */
    @Transient
    private Date windDirChangeTime;

    /** wind direction QC summary value */
    @Transient
    private String windDirDD;

    /** wind direction QC applied word */
    @Transient
    private String[] windDirQCA;

    /** wind direction QC results word */
    @Transient
    private String[] windDirQCR;

    /** wind direction QC departures */
    @Transient
    private int[] windDirQCD;

    /** wind direction IC applied word */
    @Transient
    private String windDirICA;

    /** wind direction IC results word */
    @Transient
    private String windDirICR;

    /** wind speed */
    @Transient
    private float windSpeed;

    /** wind speed time of last change */
    @Transient
    private Date windSpeedChangeTime;

    /** wind speed QC summary value */
    @Transient
    private String windSpeedDD;

    /** wind speed QC applied word */
    @Transient
    private String[] windSpeedQCA;

    /** wind speed QC results word */
    @Transient
    private String[] windSpeedQCR;

    /** wind speed QC departures */
    @Transient
    private int[] windSpeedQCD;

    /** wind speed IC applied word */
    @Transient
    private String windSpeedICA;

    /** wind speed IC results word */
    @Transient
    private String windSpeedICR;

    /** wind gust */
    @Transient
    private float windGust;

    /** Wind gust QC applied word */
    @Transient
    private String[] windGustQCA;

    /** Wind gust QC results word */
    @Transient
    private String[] windGustQCR;

    /** Wind gust QC departures */
    @Transient
    private int[] windGustQCD;

    /** wind direction at mininum windspeed */
    @Transient
    private float windDirMin;

    /** wind direction at gust */
    @Transient
    private float windDirMax;

    /** wind direction at gust QC summary value */
    @Transient
    private String windDirMaxDD;

    /** wind direction at gust QC applied word */
    @Transient
    private String[] windDirMaxQCA;

    /** wind direction at gust QC results word */
    @Transient
    private String[] windDirMaxQCR;

    /** wind direction at gust QC departures */
    @Transient
    private int[] windDirMaxQCD;

    /** sky cover */
    @Transient
    private String skyCover;

    /** sky cover layer base */
    @Transient
    private float[] skyLayerBase;

    /** visibility */
    @Transient
    private float visibility;

    /** visibility QC summary value */
    @Transient
    private String visibilityDD;

    /** visibility QC applied word */
    @Transient
    private String[] visibilityQCA;

    /** visibility QC results word */
    @Transient
    private String[] visibilityQCR;

    /** visibility QC departures */
    @Transient
    private int[] visibilityQCD;

    /** visibility QC applied word */
    @Transient
    private String visibilityICA;

    /** visibility IC results word */
    @Transient
    private String visibilityICR;

    /** fraction of sky covered by clouds */
    @Transient
    private float totalCloudCover;

    /** height of the lowest cloud layer */
    @Transient
    private String cloudBaseHeight;

    /** present weather */
    @Transient
    private String presWeather;

    /** low level cloud type */
    @Transient
    private String lowLevelCloudType;

    /** middle level cloud type */
    @Transient
    private String midLevelCloudType;

    /** high level cloud type */
    @Transient
    private String highLevelCloudType;

    /** maximum temperature recording period */
    @Transient
    private String maxTempRecordPeriod;

    /** minimum temperature recording period */
    @Transient
    private String minTempRecordPeriod;

    /** minimum temperature */
    @Transient
    private float minimumTemperature;

    /** raw precip accumulation */
    @Transient
    private float rawPrecip;

    /** precip accumulation */
    @Transient
    private float precipAccum;

    /** precip amount QC summary value */
    @Transient
    private String precipAccumDD;

    /** precip amount QC applied word */
    @Transient
    private String[] precipAccumQCA;

    /** precip amount QC results word */
    @Transient
    private String[] precipAccumQCR;

    /** precip amount QC departures */
    @Transient
    private int[] precipAccumQCD;

    /** precip amount IC applied word */
    @Transient
    private String precipAccumICA;

    /** precip amount IC results word */
    @Transient
    private String precipAccumICR;

    /** precipitation rate */
    @Transient
    private float precipRate;

    /** precip rate QC summary value */
    @Transient
    private String precipRateDD;

    /** precip rate QC applied word */
    @Transient
    private String[] precipRateQCA;

    /** precip rate QC results word */
    @Transient
    private String[] precipRateQCR;

    /** precip rate QC departures */
    @Transient
    private int[] precipRateQCD;

    /** precipitation type */
    @Transient
    private String precipType;

    /** precipitation intensity */
    @Transient
    private String precipIntensity;

    /** time since last precip */
    @Transient
    private int timeSinceLastPcp;

    /** solar radiation */
    @Transient
    private float solarRadiation;

    /** solar radiation time of last change */
    @Transient
    private Date solarRadiationChangeTime;

    /** sea surface temperature */
    @Transient
    private float seaSurfaceTemp;

    /** Sea surface temperature QC summary value */
    @Transient
    private String seaSurfaceTempDD;

    /** Sea surface temperature QC applied word */
    @Transient
    private String[] seaSurfaceTempQCA;

    /** SeaSurfaceTemp QC Results word */
    @Transient
    private String[] seaSurfaceTempQCR;

    /** Sea surface temperature QC departures */
    @Transient
    private int[] seaSurfaceTempQCD;

    /** Sea surface temperature IC applied word */
    @Transient
    private String seaSurfaceTempICA;

    /** Sea surface temperature IC results word */
    @Transient
    private String seaSurfaceTempICR;

    /** wave period */
    @Transient
    private float wavePeriod;

    /** wave height */
    @Transient
    private float waveHeight;

    /** raw text LDAD mesonet message */
    @Transient
    private String rawMessage;

    /** Total column precipitable water vapor */
    @Transient
    private float totalColumnPWV;

    /** Total GPS signal delay */
    @Transient
    private float totalSignalDelay;

    /** Dry component GPS signal delay */
    @Transient
    private float drySignalDelay;

    /** Wet component GPS signal delay */
    @Transient
    private float wetSignalDelay;

    /** Mean weighted temperature */
    @Transient
    private float meanWeightedTemperature;

    /** Formal Error */
    @Transient
    private float formalError;

    /** Wet delay mapping function */
    @Transient
    private float capPi;

    @Column(length = 15)
    private String ncSet;

    @Embedded
    @DynamicSerializeElement
    private FakePointDataView pointDataView;

    @Embeddable
    @DynamicSerialize
    private static class FakePointDataView {
        @DynamicSerializeElement
        @Column(name = "idx")
        int curIdx;

        public int getCurIdx() {
            return curIdx;
        }

        public void setCurIdx(int curIdx) {
            this.curIdx = curIdx;
        }
    }

    public QCRecord() {

    }

    public QCRecord(String uri) {
        super(uri);
    }

    public Date getInvTime() {
        return invTime;
    }

    public void setInvTime(Date invTime) {
        this.invTime = invTime;
    }

    public int getSecondsStage1_2() {
        return secondsStage1_2;
    }

    public void setSecondsStage1_2(int secondsStage1_2) {
        this.secondsStage1_2 = secondsStage1_2;
    }

    public int getSecondsStage3() {
        return secondsStage3;
    }

    public void setSecondsStage3(int secondsStage3) {
        this.secondsStage3 = secondsStage3;
    }

    public String getProviderId() {
        return providerId;
    }

    public void setProviderId(String providerId) {
        this.providerId = providerId;
    }

    public String getStationId() {
        return location.getStationId();
    }

    public String getHomeWFO() {
        return homeWFO;
    }

    public void setHomeWFO(String homeWFO) {
        this.homeWFO = homeWFO;
    }

    public int getNumericWMOid() {
        return numericWMOid;
    }

    public void setNumericWMOid(int numericWMOid) {
        this.numericWMOid = numericWMOid;
    }

    public String getStationType() {
        return stationType;
    }

    public void setStationType(String stationType) {
        this.stationType = stationType;
    }

    public String getDataProvider() {
        return dataProvider;
    }

    public void setDataProvider(String dataProvider) {
        this.dataProvider = dataProvider;
    }

    public int getFilterSetNum() {
        return filterSetNum;
    }

    public void setFilterSetNum(int filterSetNum) {
        this.filterSetNum = filterSetNum;
    }

    public String getQct() {
        return qct;
    }

    public void setQct(String qct) {
        this.qct = qct;
    }

    public String getIct() {
        return ict;
    }

    public void setIct(String ict) {
        this.ict = ict;
    }

    /**
     * @return the location
     */
    public SurfaceObsLocation getLocation() {
        return location;
    }

    /**
     * @param location
     *            the location to set
     */
    public void setLocation(SurfaceObsLocation location) {
        this.location = location;
    }

    public float getLatitude() {
        return location.getLatitude().floatValue();
    }

    public float getLongitude() {
        return location.getLongitude().floatValue();
    }

    public float getElevation() {
        return location.getElevation();
    }

    public int getDataPlatformType() {
        return dataPlatformType;
    }

    public void setDataPlatformType(int dataPlatformType) {
        this.dataPlatformType = dataPlatformType;
    }

    public int getPlatformTrueDirection() {
        return platformTrueDirection;
    }

    public void setPlatformTrueDirection(int platformTrueDirection) {
        this.platformTrueDirection = platformTrueDirection;
    }

    public int getPlatformTrueSpeed() {
        return platformTrueSpeed;
    }

    public void setPlatformTrueSpeed(int platformTrueSpeed) {
        this.platformTrueSpeed = platformTrueSpeed;
    }

    public Date getObservationTime() {
        return observationTime;
    }

    public void setObservationTime(Date observationTime) {
        this.observationTime = observationTime;
    }

    public Date getReportTime() {
        return reportTime;
    }

    public void setReportTime(Date reportTime) {
        this.reportTime = reportTime;
    }

    public Date getReceivedTime() {
        return receivedTime;
    }

    public void setReceivedTime(Date receivedTime) {
        this.receivedTime = receivedTime;
    }

    public float getTemperature() {
        return temperature;
    }

    public void setTemperature(float temperature) {
        this.temperature = temperature;
    }

    public Date getTempChangeTime() {
        return tempChangeTime;
    }

    public void setTempChangeTime(Date tempChangeTime) {
        this.tempChangeTime = tempChangeTime;
    }

    public String getTemperatureDD() {
        return temperatureDD;
    }

    public void setTemperatureDD(String temperatureDD) {
        this.temperatureDD = temperatureDD;
    }

    public String[] getTemperatureQCA() {
        return temperatureQCA;
    }

    public void setTemperatureQCA(String[] temperatureQCA) {
        this.temperatureQCA = temperatureQCA;
    }

    public String[] getTemperatureQCR() {
        return temperatureQCR;
    }

    public void setTemperatureQCR(String[] temperatureQCR) {
        this.temperatureQCR = temperatureQCR;
    }

    public int[] getTemperatureQCD() {
        return temperatureQCD;
    }

    public void setTemperatureQCD(int[] temperatureQCD) {
        this.temperatureQCD = temperatureQCD;
    }

    public String getTemperatureICA() {
        return temperatureICA;
    }

    public void setTemperatureICA(String temperatureICA) {
        this.temperatureICA = temperatureICA;
    }

    public String getTemperatureICR() {
        return temperatureICR;
    }

    public void setTemperatureICR(String temperatureICR) {
        this.temperatureICR = temperatureICR;
    }

    public float getDewpoint() {
        return dewpoint;
    }

    public void setDewpoint(float dewpoint) {
        this.dewpoint = dewpoint;
    }

    public String getDewpointDD() {
        return dewpointDD;
    }

    public void setDewpointDD(String dewpointDD) {
        this.dewpointDD = dewpointDD;
    }

    public String[] getDewpointQCA() {
        return dewpointQCA;
    }

    public void setDewpointQCA(String[] dewpointQCA) {
        this.dewpointQCA = dewpointQCA;
    }

    public String[] getDewpointQCR() {
        return dewpointQCR;
    }

    public void setDewpointQCR(String[] dewpointQCR) {
        this.dewpointQCR = dewpointQCR;
    }

    public int[] getDewpointQCD() {
        return dewpointQCD;
    }

    public void setDewpointQCD(int[] dewpointQCD) {
        this.dewpointQCD = dewpointQCD;
    }

    public String getDewpointICA() {
        return dewpointICA;
    }

    public void setDewpointICA(String dewpointICA) {
        this.dewpointICA = dewpointICA;
    }

    public String getDewpointICR() {
        return dewpointICR;
    }

    public void setDewpointICR(String dewpointICR) {
        this.dewpointICR = dewpointICR;
    }

    public float getWetBulbTemperature() {
        return wetBulbTemperature;
    }

    public void setWetBulbTemperature(float wetBulbTemperature) {
        this.wetBulbTemperature = wetBulbTemperature;
    }

    public float getRelHumidity() {
        return relHumidity;
    }

    public void setRelHumidity(float relHumidity) {
        this.relHumidity = relHumidity;
    }

    public Date getRhChangeTime() {
        return rhChangeTime;
    }

    public void setRhChangeTime(Date rhChangeTime) {
        this.rhChangeTime = rhChangeTime;
    }

    public String getRelHumidityDD() {
        return relHumidityDD;
    }

    public void setRelHumidityDD(String relHumidityDD) {
        this.relHumidityDD = relHumidityDD;
    }

    public String[] getRelHumidityQCA() {
        return relHumidityQCA;
    }

    public void setRelHumidityQCA(String[] relHumidityQCA) {
        this.relHumidityQCA = relHumidityQCA;
    }

    public String[] getRelHumidityQCR() {
        return relHumidityQCR;
    }

    public void setRelHumidityQCR(String[] relHumidityQCR) {
        this.relHumidityQCR = relHumidityQCR;
    }

    public int[] getRelHumidityQCD() {
        return relHumidityQCD;
    }

    public void setRelHumidityQCD(int[] relHumidityQCD) {
        this.relHumidityQCD = relHumidityQCD;
    }

    public float getStationPressure() {
        return stationPressure;
    }

    public void setStationPressure(float stationPressure) {
        this.stationPressure = stationPressure;
    }

    public Date getStationPressChangeTime() {
        return stationPressChangeTime;
    }

    public void setStationPressChangeTime(Date stationPressChangeTime) {
        this.stationPressChangeTime = stationPressChangeTime;
    }

    public String getStationPressureDD() {
        return stationPressureDD;
    }

    public void setStationPressureDD(String stationPressureDD) {
        this.stationPressureDD = stationPressureDD;
    }

    public String[] getStationPressureQCA() {
        return stationPressureQCA;
    }

    public void setStationPressureQCA(String[] stationPressureQCA) {
        this.stationPressureQCA = stationPressureQCA;
    }

    public String[] getStationPressureQCR() {
        return stationPressureQCR;
    }

    public void setStationPressureQCR(String[] stationPressureQCR) {
        this.stationPressureQCR = stationPressureQCR;
    }

    public int[] getStationPressureQCD() {
        return stationPressureQCD;
    }

    public void setStationPressureQCD(int[] stationPressureQCD) {
        this.stationPressureQCD = stationPressureQCD;
    }

    public String getStationPressureICA() {
        return stationPressureICA;
    }

    public void setStationPressureICA(String stationPressureICA) {
        this.stationPressureICA = stationPressureICA;
    }

    public String getStationPressureICR() {
        return stationPressureICR;
    }

    public void setStationPressureICR(String stationPressureICR) {
        this.stationPressureICR = stationPressureICR;
    }

    public float getSeaLevelPressure() {
        return seaLevelPressure;
    }

    public void setSeaLevelPressure(float seaLevelPressure) {
        this.seaLevelPressure = seaLevelPressure;
    }

    public String getSeaLevelPressureDD() {
        return seaLevelPressureDD;
    }

    public void setSeaLevelPressureDD(String seaLevelPressureDD) {
        this.seaLevelPressureDD = seaLevelPressureDD;
    }

    public String[] getSeaLevelPressureQCA() {
        return seaLevelPressureQCA;
    }

    public void setSeaLevelPressureQCA(String[] seaLevelPressureQCA) {
        this.seaLevelPressureQCA = seaLevelPressureQCA;
    }

    public String[] getSeaLevelPressureQCR() {
        return seaLevelPressureQCR;
    }

    public void setSeaLevelPressureQCR(String[] seaLevelPressureQCR) {
        this.seaLevelPressureQCR = seaLevelPressureQCR;
    }

    public int[] getSeaLevelPressureQCD() {
        return seaLevelPressureQCD;
    }

    public void setSeaLevelPressureQCD(int[] seaLevelPressureQCD) {
        this.seaLevelPressureQCD = seaLevelPressureQCD;
    }

    public String getSeaLevelPressureICA() {
        return seaLevelPressureICA;
    }

    public void setSeaLevelPressureICA(String seaLevelPressureICA) {
        this.seaLevelPressureICA = seaLevelPressureICA;
    }

    public String getSeaLevelPressureICR() {
        return seaLevelPressureICR;
    }

    public void setSeaLevelPressureICR(String seaLevelPressureICR) {
        this.seaLevelPressureICR = seaLevelPressureICR;
    }

    public String getPressChangeChar() {
        return pressChangeChar;
    }

    public void setPressChangeChar(String pressChangeChar) {
        this.pressChangeChar = pressChangeChar;
    }

    public float getPressChange3Hour() {
        return pressChange3Hour;
    }

    public void setPressChange3Hour(float pressChange3Hour) {
        this.pressChange3Hour = pressChange3Hour;
    }

    public String getPressChange3HourDD() {
        return pressChange3HourDD;
    }

    public void setPressChange3HourDD(String pressChange3HourDD) {
        this.pressChange3HourDD = pressChange3HourDD;
    }

    public String[] getPressChange3HourQCA() {
        return pressChange3HourQCA;
    }

    public void setPressChange3HourQCA(String[] pressChange3HourQCA) {
        this.pressChange3HourQCA = pressChange3HourQCA;
    }

    public String[] getPressChange3HourQCR() {
        return pressChange3HourQCR;
    }

    public void setPressChange3HourQCR(String[] pressChange3HourQCR) {
        this.pressChange3HourQCR = pressChange3HourQCR;
    }

    public int[] getPressChange3HourQCD() {
        return pressChange3HourQCD;
    }

    public void setPressChange3HourQCD(int[] pressChange3HourQCD) {
        this.pressChange3HourQCD = pressChange3HourQCD;
    }

    public String getPressChange3HourICA() {
        return pressChange3HourICA;
    }

    public void setPressChange3HourICA(String pressChange3HourICA) {
        this.pressChange3HourICA = pressChange3HourICA;
    }

    public String getPressChange3HourICR() {
        return pressChange3HourICR;
    }

    public void setPressChange3HourICR(String pressChange3HourICR) {
        this.pressChange3HourICR = pressChange3HourICR;
    }

    public float getAltimeter() {
        return altimeter;
    }

    public void setAltimeter(float altimeter) {
        this.altimeter = altimeter;
    }

    public String getAltimeterDD() {
        return altimeterDD;
    }

    public void setAltimeterDD(String altimeterDD) {
        this.altimeterDD = altimeterDD;
    }

    public String[] getAltimeterQCA() {
        return altimeterQCA;
    }

    public void setAltimeterQCA(String[] altimeterQCA) {
        this.altimeterQCA = altimeterQCA;
    }

    public String[] getAltimeterQCR() {
        return altimeterQCR;
    }

    public void setAltimeterQCR(String[] altimeterQCR) {
        this.altimeterQCR = altimeterQCR;
    }

    public int[] getAltimeterQCD() {
        return altimeterQCD;
    }

    public void setAltimeterQCD(int[] altimeterQCD) {
        this.altimeterQCD = altimeterQCD;
    }

    public float getWindDir() {
        return windDir;
    }

    public void setWindDir(float windDir) {
        this.windDir = windDir;
    }

    public Date getWindDirChangeTime() {
        return windDirChangeTime;
    }

    public void setWindDirChangeTime(Date windDirChangeTime) {
        this.windDirChangeTime = windDirChangeTime;
    }

    public String getWindDirDD() {
        return windDirDD;
    }

    public void setWindDirDD(String windDirDD) {
        this.windDirDD = windDirDD;
    }

    public String[] getWindDirQCA() {
        return windDirQCA;
    }

    public void setWindDirQCA(String[] windDirQCA) {
        this.windDirQCA = windDirQCA;
    }

    public String[] getWindDirQCR() {
        return windDirQCR;
    }

    public void setWindDirQCR(String[] windDirQCR) {
        this.windDirQCR = windDirQCR;
    }

    public int[] getWindDirQCD() {
        return windDirQCD;
    }

    public void setWindDirQCD(int[] windDirQCD) {
        this.windDirQCD = windDirQCD;
    }

    public String getWindDirICA() {
        return windDirICA;
    }

    public void setWindDirICA(String windDirICA) {
        this.windDirICA = windDirICA;
    }

    public String getWindDirICR() {
        return windDirICR;
    }

    public void setWindDirICR(String windDirICR) {
        this.windDirICR = windDirICR;
    }

    public float getWindSpeed() {
        return windSpeed;
    }

    public void setWindSpeed(float windSpeed) {
        this.windSpeed = windSpeed;
    }

    public Date getWindSpeedChangeTime() {
        return windSpeedChangeTime;
    }

    public void setWindSpeedChangeTime(Date windSpeedChangeTime) {
        this.windSpeedChangeTime = windSpeedChangeTime;
    }

    public String getWindSpeedDD() {
        return windSpeedDD;
    }

    public void setWindSpeedDD(String windSpeedDD) {
        this.windSpeedDD = windSpeedDD;
    }

    public String[] getWindSpeedQCA() {
        return windSpeedQCA;
    }

    public void setWindSpeedQCA(String[] windSpeedQCA) {
        this.windSpeedQCA = windSpeedQCA;
    }

    public String[] getWindSpeedQCR() {
        return windSpeedQCR;
    }

    public void setWindSpeedQCR(String[] windSpeedQCR) {
        this.windSpeedQCR = windSpeedQCR;
    }

    public int[] getWindSpeedQCD() {
        return windSpeedQCD;
    }

    public void setWindSpeedQCD(int[] windSpeedQCD) {
        this.windSpeedQCD = windSpeedQCD;
    }

    public String getWindSpeedICA() {
        return windSpeedICA;
    }

    public void setWindSpeedICA(String windSpeedICA) {
        this.windSpeedICA = windSpeedICA;
    }

    public String getWindSpeedICR() {
        return windSpeedICR;
    }

    public void setWindSpeedICR(String windSpeedICR) {
        this.windSpeedICR = windSpeedICR;
    }

    public float getWindGust() {
        return windGust;
    }

    public void setWindGust(float windGust) {
        this.windGust = windGust;
    }

    public String[] getWindGustQCA() {
        return windGustQCA;
    }

    public void setWindGustQCA(String[] windGustQCA) {
        this.windGustQCA = windGustQCA;
    }

    public String[] getWindGustQCR() {
        return windGustQCR;
    }

    public void setWindGustQCR(String[] windGustQCR) {
        this.windGustQCR = windGustQCR;
    }

    public int[] getWindGustQCD() {
        return windGustQCD;
    }

    public void setWindGustQCD(int[] windGustQCD) {
        this.windGustQCD = windGustQCD;
    }

    public float getWindDirMin() {
        return windDirMin;
    }

    public void setWindDirMin(float windDirMin) {
        this.windDirMin = windDirMin;
    }

    public float getWindDirMax() {
        return windDirMax;
    }

    public void setWindDirMax(float windDirMax) {
        this.windDirMax = windDirMax;
    }

    public String getWindDirMaxDD() {
        return windDirMaxDD;
    }

    public void setWindDirMaxDD(String windDirMaxDD) {
        this.windDirMaxDD = windDirMaxDD;
    }

    public String[] getWindDirMaxQCA() {
        return windDirMaxQCA;
    }

    public void setWindDirMaxQCA(String[] windDirMaxQCA) {
        this.windDirMaxQCA = windDirMaxQCA;
    }

    public String[] getWindDirMaxQCR() {
        return windDirMaxQCR;
    }

    public void setWindDirMaxQCR(String[] windDirMaxQCR) {
        this.windDirMaxQCR = windDirMaxQCR;
    }

    public int[] getWindDirMaxQCD() {
        return windDirMaxQCD;
    }

    public void setWindDirMaxQCD(int[] windDirMaxQCD) {
        this.windDirMaxQCD = windDirMaxQCD;
    }

    public String getSkyCover() {
        return skyCover;
    }

    public void setSkyCover(String skyCover) {
        this.skyCover = skyCover;
    }

    public float[] getSkyLayerBase() {
        return skyLayerBase;
    }

    public void setSkyLayerBase(float[] skyLayerBase) {
        this.skyLayerBase = skyLayerBase;
    }

    public float getVisibility() {
        return visibility;
    }

    public void setVisibility(float visibility) {
        this.visibility = visibility;
    }

    public String getVisibilityDD() {
        return visibilityDD;
    }

    public void setVisibilityDD(String visibilityDD) {
        this.visibilityDD = visibilityDD;
    }

    public String[] getVisibilityQCA() {
        return visibilityQCA;
    }

    public void setVisibilityQCA(String[] visibilityQCA) {
        this.visibilityQCA = visibilityQCA;
    }

    public String[] getVisibilityQCR() {
        return visibilityQCR;
    }

    public void setVisibilityQCR(String[] visibilityQCR) {
        this.visibilityQCR = visibilityQCR;
    }

    public int[] getVisibilityQCD() {
        return visibilityQCD;
    }

    public void setVisibilityQCD(int[] visibilityQCD) {
        this.visibilityQCD = visibilityQCD;
    }

    public String getVisibilityICA() {
        return visibilityICA;
    }

    public void setVisibilityICA(String visibilityICA) {
        this.visibilityICA = visibilityICA;
    }

    public String getVisibilityICR() {
        return visibilityICR;
    }

    public void setVisibilityICR(String visibilityICR) {
        this.visibilityICR = visibilityICR;
    }

    public float getTotalCloudCover() {
        return totalCloudCover;
    }

    public void setTotalCloudCover(float totalCloudCover) {
        this.totalCloudCover = totalCloudCover;
    }

    public String getCloudBaseHeight() {
        return cloudBaseHeight;
    }

    public void setCloudBaseHeight(String cloudBaseHeight) {
        this.cloudBaseHeight = cloudBaseHeight;
    }

    public String getPresWeather() {
        return presWeather;
    }

    public void setPresWeather(String presWeather) {
        this.presWeather = presWeather;
    }

    public String getLowLevelCloudType() {
        return lowLevelCloudType;
    }

    public void setLowLevelCloudType(String lowLevelCloudType) {
        this.lowLevelCloudType = lowLevelCloudType;
    }

    public String getMidLevelCloudType() {
        return midLevelCloudType;
    }

    public void setMidLevelCloudType(String midLevelCloudType) {
        this.midLevelCloudType = midLevelCloudType;
    }

    public String getHighLevelCloudType() {
        return highLevelCloudType;
    }

    public void setHighLevelCloudType(String highLevelCloudType) {
        this.highLevelCloudType = highLevelCloudType;
    }

    public String getMaxTempRecordPeriod() {
        return maxTempRecordPeriod;
    }

    public void setMaxTempRecordPeriod(String maxTempRecordPeriod) {
        this.maxTempRecordPeriod = maxTempRecordPeriod;
    }

    public String getMinTempRecordPeriod() {
        return minTempRecordPeriod;
    }

    public void setMinTempRecordPeriod(String minTempRecordPeriod) {
        this.minTempRecordPeriod = minTempRecordPeriod;
    }

    public float getMinimumTemperature() {
        return minimumTemperature;
    }

    public void setMinimumTemperature(float minimumTemperature) {
        this.minimumTemperature = minimumTemperature;
    }

    public float getRawPrecip() {
        return rawPrecip;
    }

    public void setRawPrecip(float rawPrecip) {
        this.rawPrecip = rawPrecip;
    }

    public float getPrecipAccum() {
        return precipAccum;
    }

    public void setPrecipAccum(float precipAccum) {
        this.precipAccum = precipAccum;
    }

    public String getPrecipAccumDD() {
        return precipAccumDD;
    }

    public void setPrecipAccumDD(String precipAccumDD) {
        this.precipAccumDD = precipAccumDD;
    }

    public String[] getPrecipAccumQCA() {
        return precipAccumQCA;
    }

    public void setPrecipAccumQCA(String[] precipAccumQCA) {
        this.precipAccumQCA = precipAccumQCA;
    }

    public String[] getPrecipAccumQCR() {
        return precipAccumQCR;
    }

    public void setPrecipAccumQCR(String[] precipAccumQCR) {
        this.precipAccumQCR = precipAccumQCR;
    }

    public int[] getPrecipAccumQCD() {
        return precipAccumQCD;
    }

    public void setPrecipAccumQCD(int[] precipAccumQCD) {
        this.precipAccumQCD = precipAccumQCD;
    }

    public String getPrecipAccumICA() {
        return precipAccumICA;
    }

    public void setPrecipAccumICA(String precipAccumICA) {
        this.precipAccumICA = precipAccumICA;
    }

    public String getPrecipAccumICR() {
        return precipAccumICR;
    }

    public void setPrecipAccumICR(String precipAccumICR) {
        this.precipAccumICR = precipAccumICR;
    }

    public float getPrecipRate() {
        return precipRate;
    }

    public void setPrecipRate(float precipRate) {
        this.precipRate = precipRate;
    }

    public String getPrecipRateDD() {
        return precipRateDD;
    }

    public void setPrecipRateDD(String precipRateDD) {
        this.precipRateDD = precipRateDD;
    }

    public String[] getPrecipRateQCA() {
        return precipRateQCA;
    }

    public void setPrecipRateQCA(String[] precipRateQCA) {
        this.precipRateQCA = precipRateQCA;
    }

    public String[] getPrecipRateQCR() {
        return precipRateQCR;
    }

    public void setPrecipRateQCR(String[] precipRateQCR) {
        this.precipRateQCR = precipRateQCR;
    }

    public int[] getPrecipRateQCD() {
        return precipRateQCD;
    }

    public void setPrecipRateQCD(int[] precipRateQCD) {
        this.precipRateQCD = precipRateQCD;
    }

    public String getPrecipType() {
        return precipType;
    }

    public void setPrecipType(String precipType) {
        this.precipType = precipType;
    }

    public String getPrecipIntensity() {
        return precipIntensity;
    }

    public void setPrecipIntensity(String precipIntensity) {
        this.precipIntensity = precipIntensity;
    }

    public int getTimeSinceLastPcp() {
        return timeSinceLastPcp;
    }

    public void setTimeSinceLastPcp(int timeSinceLastPcp) {
        this.timeSinceLastPcp = timeSinceLastPcp;
    }

    public float getSolarRadiation() {
        return solarRadiation;
    }

    public void setSolarRadiation(float solarRadiation) {
        this.solarRadiation = solarRadiation;
    }

    public Date getSolarRadiationChangeTime() {
        return solarRadiationChangeTime;
    }

    public void setSolarRadiationChangeTime(Date solarRadiationChangeTime) {
        this.solarRadiationChangeTime = solarRadiationChangeTime;
    }

    public float getSeaSurfaceTemp() {
        return seaSurfaceTemp;
    }

    public void setSeaSurfaceTemp(float seaSurfaceTemp) {
        this.seaSurfaceTemp = seaSurfaceTemp;
    }

    public String getSeaSurfaceTempDD() {
        return seaSurfaceTempDD;
    }

    public void setSeaSurfaceTempDD(String seaSurfaceTempDD) {
        this.seaSurfaceTempDD = seaSurfaceTempDD;
    }

    public String[] getSeaSurfaceTempQCA() {
        return seaSurfaceTempQCA;
    }

    public void setSeaSurfaceTempQCA(String[] seaSurfaceTempQCA) {
        this.seaSurfaceTempQCA = seaSurfaceTempQCA;
    }

    public String[] getSeaSurfaceTempQCR() {
        return seaSurfaceTempQCR;
    }

    public void setSeaSurfaceTempQCR(String[] seaSurfaceTempQCR) {
        this.seaSurfaceTempQCR = seaSurfaceTempQCR;
    }

    public int[] getSeaSurfaceTempQCD() {
        return seaSurfaceTempQCD;
    }

    public void setSeaSurfaceTempQCD(int[] seaSurfaceTempQCD) {
        this.seaSurfaceTempQCD = seaSurfaceTempQCD;
    }

    public String getSeaSurfaceTempICA() {
        return seaSurfaceTempICA;
    }

    public void setSeaSurfaceTempICA(String seaSurfaceTempICA) {
        this.seaSurfaceTempICA = seaSurfaceTempICA;
    }

    public String getSeaSurfaceTempICR() {
        return seaSurfaceTempICR;
    }

    public void setSeaSurfaceTempICR(String seaSurfaceTempICR) {
        this.seaSurfaceTempICR = seaSurfaceTempICR;
    }

    public float getWavePeriod() {
        return wavePeriod;
    }

    public void setWavePeriod(float wavePeriod) {
        this.wavePeriod = wavePeriod;
    }

    public float getWaveHeight() {
        return waveHeight;
    }

    public void setWaveHeight(float waveHeight) {
        this.waveHeight = waveHeight;
    }

    public String getRawMessage() {
        return rawMessage;
    }

    public void setRawMessage(String rawMessage) {
        this.rawMessage = rawMessage;
    }

    public float getTotalColumnPWV() {
        return totalColumnPWV;
    }

    public void setTotalColumnPWV(float totalColumnPWV) {
        this.totalColumnPWV = totalColumnPWV;
    }

    public float getTotalSignalDelay() {
        return totalSignalDelay;
    }

    public void setTotalSignalDelay(float totalSignalDelay) {
        this.totalSignalDelay = totalSignalDelay;
    }

    public float getDrySignalDelay() {
        return drySignalDelay;
    }

    public void setDrySignalDelay(float drySignalDelay) {
        this.drySignalDelay = drySignalDelay;
    }

    public float getWetSignalDelay() {
        return wetSignalDelay;
    }

    public void setWetSignalDelay(float wetSignalDelay) {
        this.wetSignalDelay = wetSignalDelay;
    }

    public float getMeanWeightedTemperature() {
        return meanWeightedTemperature;
    }

    public void setMeanWeightedTemperature(float meanWeightedTemperature) {
        this.meanWeightedTemperature = meanWeightedTemperature;
    }

    public float getFormalError() {
        return formalError;
    }

    public void setFormalError(float formalError) {
        this.formalError = formalError;
    }

    public float getCapPi() {
        return capPi;
    }

    public void setCapPi(float capPi) {
        this.capPi = capPi;
    }

    public static long getSerialVersionUID() {
        return serialVersionUID;
    }

    /**
     * @return the ncSet
     */
    public String getNcSet() {
        return ncSet;
    }

    /**
     * @param ncSet
     *            the ncSet to set
     */
    public void setNcSet(String ncSet) {
        this.ncSet = ncSet;
    }

    /**
     * @return the ncIndex
     */
    public int getNcIndex() {
        return pointDataView != null ? pointDataView.curIdx : -1;
    }

    /**
     * @param ncIndex
     *            the ncIndex to set
     */
    public void setNcIndex(int ncIndex) {
        if (this.pointDataView == null) {
            pointDataView = new FakePointDataView();
        }
        this.pointDataView.curIdx = ncIndex;
    }

    @Override
    public ISpatialObject getSpatialObject() {
        return location;
    }

    /**
     * @return the qcType
     */
    public String getQcType() {
        return qcType;
    }

    /**
     * @param qcType
     *            the qcType to set
     */
    public void setQcType(String qcType) {
        this.qcType = qcType;
    }

    public FakePointDataView getPointDataView() {
        return pointDataView;
    }

    public void setPointDataView(FakePointDataView pointDataView) {
        this.pointDataView = pointDataView;
    }

    @Override
    public String getPluginName() {
        return "qc";
    }
}
