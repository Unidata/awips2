package com.raytheon.uf.edex.plugin.madis.ogc.feature;

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

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.TimeZone;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import net.opengis.gml.v_3_1_1.AbstractFeatureType;

import com.raytheon.uf.common.dataplugin.madis.MadisRecord;
import com.raytheon.uf.common.dataplugin.madis.MadisRecord.QCD;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.ogc.common.feature.ObsLocation;

/**
 * Madis Feature Type
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * Mar 27, 2013 1746        dhladky     Initial creation
 * Jun 03, 2013 1763        dhladky     Altered QCD values map
 * Aug 30, 2013 2298        rjpeter     Make getPluginName abstract
 * Sept 19,2013 2388        dhladky     Fixed creation of geometry (location assignment)
 * jan 22, 2014 2713       dhladky     Calendar conversion.
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "madis", propOrder = { "provider", "sub_provider", "dataset",
        "restriction", "obsLocation", "timeObs", "dewpoint", "dewpoint_qcd",
        "dewpoint_qca", "dewpoint_qcr", "rh", "rh_qcd", "rh_qca", "rh_qcr",
        "altimeter", "altimeter_qcd", "altimeter_qca", "altimeter_qcr",
        "temperature", "temperature_qcd", "temperature_qca", "temperature_qcr",
        "windDirection", "windDirection_qcd", "windDirection_qca",
        "windDirection_qcr", "precipRate", "precipRate_qcd", "precipRate_qca",
        "precipRate_qcr", "windSpeed", "windSpeed_qcd", "windSpeed_qca",
        "windSpeed_qcr", "windGust", "windGust_qcd", "windGust_qca",
        "windGust_qcr", "precipitalWater", "precipitalWater_qcd",
        "precipitalWater_qca", "precipitalWater_qcr", "pressure",
        "pressure_qcd", "pressure_qca", "pressure_qcr" })
public class Madis extends AbstractFeatureType {

    private static final int intNullVal = -9999;

    private static final float floatNullVal = -9999;

    protected String provider = "";

    protected String sub_provider = "";

    protected int restriction = intNullVal;

    protected int dataset = intNullVal;

    protected ObsLocation obsLocation;

    @XmlSchemaType(name = "dateTime")
    protected XMLGregorianCalendar timeObs;

    protected float dewpoint = floatNullVal;

    protected String dewpoint_qcd = QCD.MISSING.name();

    protected int dewpoint_qca = intNullVal;

    protected int dewpoint_qcr = intNullVal;

    protected float rh = floatNullVal;

    protected String rh_qcd = QCD.MISSING.name();

    protected int rh_qca = intNullVal;

    protected int rh_qcr = intNullVal;

    protected float altimeter = floatNullVal;

    protected String altimeter_qcd = QCD.MISSING.name();

    protected int altimeter_qca = intNullVal;

    protected int altimeter_qcr = intNullVal;

    protected float temperature = floatNullVal;

    protected String temperature_qcd = QCD.MISSING.name();

    protected int temperature_qca = intNullVal;

    protected int temperature_qcr = intNullVal;

    protected int windDirection = intNullVal;

    protected String windDirection_qcd = QCD.MISSING.name();

    protected int windDirection_qca = intNullVal;

    protected int windDirection_qcr = intNullVal;

    protected float precipRate = floatNullVal;

    protected String precipRate_qcd = QCD.MISSING.name();

    protected int precipRate_qca = intNullVal;

    protected int precipRate_qcr = intNullVal;

    protected float windSpeed = floatNullVal;

    protected String windSpeed_qcd = QCD.MISSING.name();

    protected int windSpeed_qca = intNullVal;

    protected int windSpeed_qcr = intNullVal;

    protected float windGust = floatNullVal;

    protected String windGust_qcd = QCD.MISSING.name();

    protected int windGust_qca = intNullVal;

    protected int windGust_qcr = intNullVal;

    protected float precipitalWater = floatNullVal;

    protected String precipitalWater_qcd = QCD.MISSING.name();

    protected int precipitalWater_qca = intNullVal;

    protected int precipitalWater_qcr = intNullVal;

    protected float pressure = floatNullVal;

    protected String pressure_qcd = QCD.MISSING.name();

    protected int pressure_qca = intNullVal;

    protected int pressure_qcr = intNullVal;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(Madis.class);
    
    /**
     * Converts a MADIS record to a MADIS feature
     * 
     * @param record
     */
    public Madis(MadisRecord record) {

        this.setProvider(record.getProvider());
        this.setSub_provider(record.getSubProvider());
        this.setDataset(getInt(record.getDataset(), intNullVal));
        this.setRestriction(getInt(record.getRestriction(), intNullVal));
        Calendar cal = record.getDataTime().getRefTimeAsCalendar();
        this.setTimeObs(getCalendar(cal));
        this.setObsLocation(getObsLocation(record.getLocation()));
        this.setDewpoint(getFloat(record.getDewpoint(), floatNullVal));
        this.setDewpoint_qcd(record.getDewpoint_qcd().name());
        this.setDewpoint_qca(getInt(record.getDewpoint_qca(), intNullVal));
        this.setDewpoint_qcr(getInt(record.getDewpoint_qcr(), intNullVal));
        this.setRh(getFloat(record.getRh(), floatNullVal));
        this.setRh_qcd(record.getRh_qcd().name());
        this.setRh_qca(getInt(record.getRh_qca(), intNullVal));
        this.setRh_qcr(getInt(record.getRh_qcr(), intNullVal));
        this.setAltimeter(getFloat(record.getAltimeter(), floatNullVal));
        this.setAltimeter_qcd(record.getAltimeter_qcd().name());
        this.setAltimeter_qca(getInt(record.getAltimeter_qca(), intNullVal));
        this.setAltimeter_qcr(getInt(record.getAltimeter_qcr(), intNullVal));
        this.setTemperature(getFloat(record.getTemperature(), floatNullVal));
        this.setTemperature_qcd(record.getTemperature_qcd().name());
        this.setTemperature_qca(getInt(record.getTemperature_qca(), intNullVal));
        this.setTemperature_qcr(getInt(record.getTemperature_qcr(), intNullVal));
        this.setWindDirection(getInt(record.getWindDirection(), intNullVal));
        this.setWindDirection_qcd(record.getWindDirection_qcd().name());
        this.setWindDirection_qca(getInt(record.getWindDirection_qca(),
                intNullVal));
        this.setWindDirection_qcr(getInt(record.getWindDirection_qcr(),
                intNullVal));
        this.setPrecipRate(getFloat(record.getPrecipRate(), floatNullVal));
        this.setPrecipRate_qcd(record.getPrecipRate_qcd().name());
        this.setPrecipRate_qca(getInt(record.getPrecipRate_qca(), intNullVal));
        this.setPrecipRate_qcr(getInt(record.getPrecipRate_qcr(), intNullVal));
        this.setWindSpeed(getFloat(record.getWindSpeed(), floatNullVal));
        this.setWindSpeed_qcd(record.getWindSpeed_qcd().name());
        this.setWindSpeed_qca(getInt(record.getWindSpeed_qca(), intNullVal));
        this.setWindSpeed_qcr(getInt(record.getWindSpeed_qcr(), intNullVal));
        this.setWindGust(getFloat(record.getWindGust(), floatNullVal));
        this.setWindGust_qcd(record.getWindGust_qcd().name());
        this.setWindGust_qca(getInt(record.getWindGust_qca(), intNullVal));
        this.setWindGust_qcr(getInt(record.getWindGust_qcr(), intNullVal));
        this.setPrecipitalWater(getFloat(record.getPrecipitalWater(),
                floatNullVal));
        this.setPrecipitalWater_qcd(record.getPrecipitalWater_qcd().name());
        this.setPrecipitalWater_qca(getInt(record.getPrecipitalWater_qca(),
                intNullVal));
        this.setPrecipitalWater_qcr(getInt(record.getPrecipitalWater_qcr(),
                intNullVal));
        this.setPressure(getFloat(record.getPressure(), floatNullVal));
        this.setPressure_qcd(record.getPressure_qcd().name());
        this.setPressure_qca(getInt(record.getPressure_qca(), intNullVal));
        this.setPressure_qcr(getInt(record.getPressure_qcr(), intNullVal));
    }

    /**
     * Converts a MADIS feature to a MADIS record
     * 
     * @return
     */
    public MadisRecord getRecord() {

        MadisRecord record = new MadisRecord();
        record.setProvider(this.getProvider());
        record.setSubProvider(this.getSub_provider());
        record.setDataset(this.getDataset());
        record.setRestriction(this.getRestriction());
        Calendar cal = this.getTimeObs().toGregorianCalendar();
        record.setDataTime(new DataTime(cal.getTime()));
        record.setTimeObs(cal);
        record.setLocation(getSfcObsLocation(this.getObsLocation()));
        record.setDewpoint(this.getDewpoint());
        record.setDewpoint_qcd(QCD.fromVal(this.getDewpoint_qcd()));
        record.setDewpoint_qca(this.getDewpoint_qca());
        record.setDewpoint_qcr(this.getDewpoint_qcr());
        record.setRh(this.getRh());
        record.setRh_qcd(QCD.fromVal(this.getRh_qcd()));
        record.setRh_qca(this.getRh_qca());
        record.setRh_qcr(this.getRh_qcr());
        record.setAltimeter(this.getAltimeter());
        record.setAltimeter_qcd(QCD.fromVal(this.getAltimeter_qcd()));
        record.setAltimeter_qca(this.getAltimeter_qca());
        record.setAltimeter_qcr(this.getAltimeter_qcr());
        record.setTemperature(this.getTemperature());
        record.setTemperature_qcd(QCD.fromVal(this.getTemperature_qcd()));
        record.setTemperature_qca(this.getTemperature_qca());
        record.setTemperature_qcr(this.getTemperature_qcr());
        record.setWindDirection(this.getWindDirection());
        record.setWindDirection_qcd(QCD.fromVal(this.getWindDirection_qcd()));
        record.setWindDirection_qca(this.getWindDirection_qca());
        record.setWindDirection_qcr(this.getWindDirection_qcr());
        record.setPrecipRate(this.getPrecipRate());
        record.setPrecipRate_qcd(QCD.fromVal(this.getPrecipRate_qcd()));
        record.setPrecipRate_qca(this.getPrecipRate_qca());
        record.setPrecipRate_qcr(this.getPrecipRate_qcr());
        record.setWindSpeed(this.getWindSpeed());
        record.setWindSpeed_qcd(QCD.fromVal(this.getWindSpeed_qcd()));
        record.setWindSpeed_qca(this.getWindSpeed_qca());
        record.setWindSpeed_qcr(this.getWindSpeed_qcr());
        record.setWindGust(this.getWindGust());
        record.setWindGust_qcd(QCD.fromVal(this.getWindGust_qcd()));
        record.setWindGust_qca(this.getWindGust_qca());
        record.setWindGust_qcr(this.getWindGust_qcr());
        record.setPrecipitalWater(this.getPrecipitalWater());
        record.setPrecipitalWater_qcd(QCD.fromVal(this.getPrecipitalWater_qcd()));
        record.setPrecipitalWater_qca(this.getPrecipitalWater_qca());
        record.setPrecipitalWater_qcr(this.getPrecipitalWater_qcr());
        record.setPressure(this.getPressure());
        record.setPressure_qcd(QCD.fromVal(this.getPressure_qcd()));
        record.setPressure_qca(this.getPressure_qca());
        record.setPressure_qcr(this.getPressure_qcr());

        return record;
    }

    public Madis() {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.jvnet.jaxb2_commons.lang.CopyTo#createNewInstance()
     */
    @Override
    public Madis createNewInstance() {
        return new Madis();
    }

    public String getProvider() {
        return provider;
    }

    public void setProvider(String provider) {
        this.provider = provider;
    }

    public String getSub_provider() {
        return sub_provider;
    }

    public void setSub_provider(String sub_provider) {
        this.sub_provider = sub_provider;
    }

    public float getDewpoint() {
        return dewpoint;
    }

    public void setDewpoint(float dewpoint) {
        this.dewpoint = dewpoint;
    }

    public String getDewpoint_qcd() {
        return dewpoint_qcd;
    }

    public void setDewpoint_qcd(String dewpoint_qcd) {
        this.dewpoint_qcd = dewpoint_qcd;
    }

    public float getRh() {
        return rh;
    }

    public void setRh(float rh) {
        this.rh = rh;
    }

    public String getRh_qcd() {
        return rh_qcd;
    }

    public void setRh_qcd(String rh_qcd) {
        this.rh_qcd = rh_qcd;
    }

    public float getAltimeter() {
        return altimeter;
    }

    public void setAltimeter(float altimeter) {
        this.altimeter = altimeter;
    }

    public String getAltimeter_qcd() {
        return altimeter_qcd;
    }

    public void setAltimeter_qcd(String altimeter_qcd) {
        this.altimeter_qcd = altimeter_qcd;
    }

    public float getTemperature() {
        return temperature;
    }

    public void setTemperature(float temperature) {
        this.temperature = temperature;
    }

    public String getTemperature_qcd() {
        return temperature_qcd;
    }

    public void setTemperature_qcd(String temperature_qcd) {
        this.temperature_qcd = temperature_qcd;
    }

    public int getWindDirection() {
        return windDirection;
    }

    public void setWindDirection(int windDirection) {
        this.windDirection = windDirection;
    }

    public String getWindDirection_qcd() {
        return windDirection_qcd;
    }

    public void setWindDirection_qcd(String windDirection_qcd) {
        this.windDirection_qcd = windDirection_qcd;
    }

    public float getPrecipRate() {
        return precipRate;
    }

    public void setPrecipRate(float precipRate) {
        this.precipRate = precipRate;
    }

    public String getPrecipRate_qcd() {
        return precipRate_qcd;
    }

    public void setPrecipRate_qcd(String precipRate_qcd) {
        this.precipRate_qcd = precipRate_qcd;
    }

    public float getWindSpeed() {
        return windSpeed;
    }

    public void setWindSpeed(float windSpeed) {
        this.windSpeed = windSpeed;
    }

    public String getWindSpeed_qcd() {
        return windSpeed_qcd;
    }

    public void setWindSpeed_qcd(String windSpeed_qcd) {
        this.windSpeed_qcd = windSpeed_qcd;
    }

    public float getWindGust() {
        return windGust;
    }

    public void setWindGust(float windGust) {
        this.windGust = windGust;
    }

    public String getWindGust_qcd() {
        return windGust_qcd;
    }

    public void setWindGust_qcd(String windGust_qcd) {
        this.windGust_qcd = windGust_qcd;
    }

    public float getPrecipitalWater() {
        return precipitalWater;
    }

    public void setPrecipitalWater(float precipitalWater) {
        this.precipitalWater = precipitalWater;
    }

    public String getPrecipitalWater_qcd() {
        return precipitalWater_qcd;
    }

    public void setPrecipitalWater_qcd(String precipitalWater_qcd) {
        this.precipitalWater_qcd = precipitalWater_qcd;
    }

    public float getPressure() {
        return pressure;
    }

    public void setPressure(float pressure) {
        this.pressure = pressure;
    }

    public String getPressure_qcd() {
        return pressure_qcd;
    }

    public void setPressure_qcd(String pressure_qcd) {
        this.pressure_qcd = pressure_qcd;
    }

    public int getDewpoint_qca() {
        return dewpoint_qca;
    }

    public void setDewpoint_qca(int dewpoint_qca) {
        this.dewpoint_qca = dewpoint_qca;
    }

    public int getDewpoint_qcr() {
        return dewpoint_qcr;
    }

    public void setDewpoint_qcr(int dewpoint_qcr) {
        this.dewpoint_qcr = dewpoint_qcr;
    }

    public int getRh_qca() {
        return rh_qca;
    }

    public void setRh_qca(int rh_qca) {
        this.rh_qca = rh_qca;
    }

    public int getRh_qcr() {
        return rh_qcr;
    }

    public void setRh_qcr(int rh_qcr) {
        this.rh_qcr = rh_qcr;
    }

    public int getAltimeter_qca() {
        return altimeter_qca;
    }

    public void setAltimeter_qca(int altimeter_qca) {
        this.altimeter_qca = altimeter_qca;
    }

    public int getAltimeter_qcr() {
        return altimeter_qcr;
    }

    public void setAltimeter_qcr(int altimeter_qcr) {
        this.altimeter_qcr = altimeter_qcr;
    }

    public int getTemperature_qca() {
        return temperature_qca;
    }

    public void setTemperature_qca(int temperature_qca) {
        this.temperature_qca = temperature_qca;
    }

    public int getTemperature_qcr() {
        return temperature_qcr;
    }

    public void setTemperature_qcr(int temperature_qcr) {
        this.temperature_qcr = temperature_qcr;
    }

    public int getWindDirection_qca() {
        return windDirection_qca;
    }

    public void setWindDirection_qca(int windDirection_qca) {
        this.windDirection_qca = windDirection_qca;
    }

    public int getWindDirection_qcr() {
        return windDirection_qcr;
    }

    public void setWindDirection_qcr(int windDirection_qcr) {
        this.windDirection_qcr = windDirection_qcr;
    }

    public int getPrecipRate_qca() {
        return precipRate_qca;
    }

    public void setPrecipRate_qca(int precipRate_qca) {
        this.precipRate_qca = precipRate_qca;
    }

    public int getPrecipRate_qcr() {
        return precipRate_qcr;
    }

    public void setPrecipRate_qcr(int precipRate_qcr) {
        this.precipRate_qcr = precipRate_qcr;
    }

    public int getWindSpeed_qca() {
        return windSpeed_qca;
    }

    public void setWindSpeed_qca(int windSpeed_qca) {
        this.windSpeed_qca = windSpeed_qca;
    }

    public int getWindSpeed_qcr() {
        return windSpeed_qcr;
    }

    public void setWindSpeed_qcr(int windSpeed_qcr) {
        this.windSpeed_qcr = windSpeed_qcr;
    }

    public int getWindGust_qca() {
        return windGust_qca;
    }

    public void setWindGust_qca(int windGust_qca) {
        this.windGust_qca = windGust_qca;
    }

    public int getWindGust_qcr() {
        return windGust_qcr;
    }

    public void setWindGust_qcr(int windGust_qcr) {
        this.windGust_qcr = windGust_qcr;
    }

    public int getPrecipitalWater_qca() {
        return precipitalWater_qca;
    }

    public void setPrecipitalWater_qca(int precipitalWater_qca) {
        this.precipitalWater_qca = precipitalWater_qca;
    }

    public int getPrecipitalWater_qcr() {
        return precipitalWater_qcr;
    }

    public void setPrecipitalWater_qcr(int precipitalWater_qcr) {
        this.precipitalWater_qcr = precipitalWater_qcr;
    }

    public int getPressure_qca() {
        return precipitalWater_qca;
    }

    public void setPressure_qca(int pressure_qca) {
        this.pressure_qca = pressure_qca;
    }

    public int getPressure_qcr() {
        return pressure_qcr;
    }

    public void setPressure_qcr(int pressure_qcr) {
        this.pressure_qcr = pressure_qcr;
    }

    /**
     * Gets the value of the timeObs property.
     * 
     * @return possible object is {@link XMLGregorianCalendar }
     * 
     */
    public XMLGregorianCalendar getTimeObs() {
        return timeObs;
    }

    /**
     * Sets the value of the timeObs property.
     * 
     * @param value
     *            allowed object is {@link XMLGregorianCalendar }
     * 
     */
    public void setTimeObs(XMLGregorianCalendar value) {
        this.timeObs = value;
    }

    protected XMLGregorianCalendar getCalendar(Calendar cal) {
        if (cal == null) {
            return null;
        }
        GregorianCalendar gcal = new GregorianCalendar(cal.getTimeZone());
        gcal.setTimeInMillis(cal.getTimeInMillis());
        try {
            return DatatypeFactory.newInstance().newXMLGregorianCalendar(gcal);
        } catch (DatatypeConfigurationException e) {
            statusHandler
                    .error("Unable to convert calendar. " + e.getMessage());
            return null;
        }
    }

    private Integer getInt(int i, int nullVal) {
        if (i == nullVal) {
            return null;
        }
        return i;
    }

    private Float getFloat(float f, float nullVal) {
        if (f == nullVal) {
            return null;
        }
        return f;
    }

    public int getRestriction() {
        return restriction;
    }

    public void setRestriction(int restriction) {
        this.restriction = restriction;
    }

    public int getDataset() {
        return dataset;
    }

    public void setDataset(int dataset) {
        this.dataset = dataset;
    }

    public ObsLocation getObsLocation() {
        return obsLocation;
    }

    public void setObsLocation(ObsLocation obsLocation) {
        this.obsLocation = obsLocation;
    }

    /**
     * .
     * 
     * @param obsLocation
     * @return
     */
    private ObsLocation getObsLocation(SurfaceObsLocation from) {
        return new ObsLocation(from);
    }

    /**
     * transform to SfcObsLocation
     * 
     * @param obsLocation
     * @return
     */
    private SurfaceObsLocation getSfcObsLocation(ObsLocation obsLocation) {
        SurfaceObsLocation sol = new SurfaceObsLocation(
                obsLocation.getStationId());
        sol.setElevation(obsLocation.getElevation());
        List<Double> points = obsLocation.getLocation().getPos().getValue();
        sol.assignLocation(points.get(1), points.get(0));

        return sol;
    }

}
