/**
 * 
 * PafmParameters
 * 
 * This class represents the parameters for the PAFM record.   This contains the 
 * setters and getters for the grandchild table.
 * 
 * <pre>
 *      
 * SOFTWARE HISTORY
 *      
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 08/21/2009   126         F. J. Yen   Initial Creation
 * 12/11/2009	126			F. J. Yen	Migrated from to11d3 to to11d6
 * 01/06/2010   126	 		F. J. Yen	Migrated and refactored from to11dr3 to to11dr11
 *       
 * </pre>
 * 
 * @author Fee Jing Yen, SIB
 * @version 1
 */

package gov.noaa.nws.ncep.common.dataplugin.pafm;

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

import java.io.Serializable;
import java.util.Calendar;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@Entity
@Table(name = "pafm_parameters")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class PafmParameters implements Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    private static final float RMISSD = IDecoderConstantsN.FLOAT_MISSING;

    private static final Integer IMISSD = IDecoderConstantsN.INTEGER_MISSING;

    @Id
    @GeneratedValue
    private Integer recordId = null;

    /** The UTC forecast time **/
    // @DataURI(position=1)
    @XmlElement
    @DynamicSerializeElement
    private Calendar forecastTimeUtc;

    /** The local forecast time **/
    @XmlElement
    @DynamicSerializeElement
    private Integer forecastTimeLocal;

    /** The local forecast time zone **/
    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String forecastTimeZone;

    @XmlElement
    @DynamicSerializeElement
    private Float avgMxTmpf;

    @XmlElement
    @DynamicSerializeElement
    private Float hiMxTmpf;

    @XmlElement
    @DynamicSerializeElement
    private Float loMxTmpf;

    @XmlElement
    @DynamicSerializeElement
    private Float avgMnTmpf;

    @XmlElement
    @DynamicSerializeElement
    private Float hiMnTmpf;

    @XmlElement
    @DynamicSerializeElement
    private Float loMnTmpf;

    @XmlElement
    @DynamicSerializeElement
    private Float tmpf;

    @XmlElement
    @DynamicSerializeElement
    private Float dwpf;

    @XmlElement
    @DynamicSerializeElement
    private Float relh;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String windDir;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String windSmph;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String gust_Mph;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String pwindDir;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String windChar;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String skyCover;

    @XmlElement
    @DynamicSerializeElement
    private Float pop12;

    @XmlElement
    @DynamicSerializeElement
    private Float qpf12Mn;

    @XmlElement
    @DynamicSerializeElement
    private Float qpf12Mx;

    @XmlElement
    @DynamicSerializeElement
    private Float highestMaxQpf;

    @XmlElement
    @DynamicSerializeElement
    private Float lowestMaxQpf;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private Float snow12Mn;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private Float snow12Mx;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String avgSkyCover;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String obvis;

    @XmlElement
    @DynamicSerializeElement
    private Float windChill;

    @XmlElement
    @DynamicSerializeElement
    private Float heatIndex;

    @XmlElement
    @DynamicSerializeElement
    private Float minChill;

    @XmlElement
    @DynamicSerializeElement
    private Float maxHeat;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String rain;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String rainShwrs;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String sprinkles;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String tstms;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String drizzle;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String snow;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String snowShwrs;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String flurries;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String sleet;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String frzgRain;

    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String frzgDrzl;

    @Column(length = 160)
    @XmlElement
    @DynamicSerializeElement
    private String hazards;

    /**
     * No-Arg Constructor.
     */
    public PafmParameters() {
        this.avgMxTmpf = RMISSD;
        this.hiMxTmpf = RMISSD;
        this.loMxTmpf = RMISSD;
        this.avgMnTmpf = RMISSD;
        this.loMnTmpf = RMISSD;
        this.hiMnTmpf = RMISSD;
        this.tmpf = RMISSD;
        this.dwpf = RMISSD;
        this.relh = RMISSD;
        this.windSmph = " ";
        this.gust_Mph = " ";
        this.pop12 = RMISSD;
        this.qpf12Mn = RMISSD;
        this.qpf12Mx = RMISSD;
        this.highestMaxQpf = RMISSD;
        this.lowestMaxQpf = RMISSD;
        this.snow12Mn = RMISSD;
        this.snow12Mx = RMISSD;

        this.windChill = RMISSD;
        this.heatIndex = RMISSD;
        this.minChill = RMISSD;
        this.maxHeat = RMISSD;
        this.forecastTimeUtc = null;
        // this.forecastTimeLocal = null;
        this.forecastTimeLocal = IMISSD;
        this.forecastTimeZone = " ";
        this.windDir = " ";
        this.pwindDir = " ";
        this.windChar = " ";
        this.skyCover = " ";
        this.avgSkyCover = " ";
        this.obvis = " ";

        this.rain = " ";
        this.rainShwrs = " ";
        this.sprinkles = " ";
        this.tstms = " ";
        this.drizzle = " ";
        this.snow = " ";
        this.snowShwrs = " ";
        this.flurries = " ";
        this.sleet = " ";
        this.frzgRain = " ";
        this.frzgDrzl = " ";
        this.hazards = " ";

    }

    /**
     * @return the serialVersionUID
     */
    public static long getSerialVersionUID() {
        return serialVersionUID;
    }

    /**
     * @return The recordId. If not set returns to null.
     */
    public Integer getRecordId() {
        return recordId;
    }

    /**
     * Set the record id.
     * 
     * @param record
     */
    @SuppressWarnings("unused")
    private void setRecordId(Integer recordId) {
        this.recordId = recordId;
    }

    /**
     * @return the forecastTimeUtc
     */
    public Calendar getForecastTimeUtc() {
        return forecastTimeUtc;
    }

    /**
     * @param forecastTimeUtc
     *            the forecastTimeUtc to set
     */
    public void setForecastTimeUtc(Calendar forecastTimeUtc) {
        this.forecastTimeUtc = forecastTimeUtc;
    }

    public Integer getForecastTimeLocal() {
        return forecastTimeLocal;
    }

    /**
     * @param forecastTimeLocal
     *            the local forecast hour
     */
    public void setForecastTimeLocal(Integer forecastTimeLocal) {
        this.forecastTimeLocal = forecastTimeLocal;
    }

    public String getForecastTimeZone() {
        return forecastTimeZone;
    }

    /**
     * @param forecastTimeZone
     *            the local time zone (for forecastTimeLocal)
     */
    public void setForecastTimeZone(String forecastTimeZone) {
        this.forecastTimeZone = forecastTimeZone;
    }

    public float getAvgMxTmpf() {
        return avgMxTmpf;
    }

    public void setAvgMxTmpf(float avgMxTmpf) {
        this.avgMxTmpf = avgMxTmpf;
    }

    public float getHiMxTmpf() {
        return hiMxTmpf;
    }

    public void setHiMxTmpf(float hiMxTmpf) {
        this.hiMxTmpf = hiMxTmpf;
    }

    public float getLoMxTmpf() {
        return loMxTmpf;
    }

    public void setLoMxTmpf(float loMxTmpf) {
        this.loMxTmpf = loMxTmpf;
    }

    public float getAvgMnTmpf() {
        return avgMnTmpf;
    }

    public void setAvgMnTmpf(float avgMnTmpf) {
        this.avgMnTmpf = avgMnTmpf;
    }

    public float getHiMnTmpf() {
        return hiMnTmpf;
    }

    public void setHiMnTmpf(float hiMnTmpf) {
        this.hiMnTmpf = hiMnTmpf;
    }

    public float getLoMnTmpf() {
        return loMnTmpf;
    }

    public void setLoMnTmpf(float loMnTmpf) {
        this.loMnTmpf = loMnTmpf;
    }

    public float getTmpf() {
        return tmpf;
    }

    public void setTmpf(float tmpf) {
        this.tmpf = tmpf;
    }

    public float getDwpf() {
        return dwpf;
    }

    public void setDwpf(float dwpf) {
        this.dwpf = dwpf;
    }

    public float getRelh() {
        return relh;
    }

    public void setRelh(float relh) {
        this.relh = relh;
    }

    public String getWindDir() {
        return windDir;
    }

    public void setWindDir(String windDir) {
        this.windDir = windDir;
    }

    /*
     * Getter and setter for Wind Speed in mph
     */
    public String getWindSmph() {
        return windSmph;
    }

    public void setWindSmph(String windSmph) {
        this.windSmph = windSmph;
    }

    public String getGust_Mph() {
        return gust_Mph;
    }

    public void setGust_Mph(String gust_Mph) {
        this.gust_Mph = gust_Mph;
    }

    public String getWindChar() {
        return windChar;
    }

    public void setWindChar(String windChar) {
        this.windChar = windChar;
    }

    public String getPwindDir() {
        return pwindDir;
    }

    public void setPwindDir(String pwindDir) {
        this.pwindDir = pwindDir;
    }

    public String getSkyCover() {
        return skyCover;
    }

    public void setSkyCover(String skyCover) {
        this.skyCover = skyCover;
    }

    public float getPop12() {
        return pop12;
    }

    public void setPop12(float pop12) {
        this.pop12 = pop12;
    }

    public float getQpf12Mn() {
        return qpf12Mn;
    }

    public void setQpf12Mn(float qpf12Mn) {
        this.qpf12Mn = qpf12Mn;
    }

    public float getQpf12Mx() {
        return qpf12Mx;
    }

    public void setQpf12Mx(float qpf12Mx) {
        this.qpf12Mx = qpf12Mx;
    }

    public float getHighestMaxQpf() {
        return highestMaxQpf;
    }

    public void setHighestMaxQpf(float highestMaxQpf) {
        this.highestMaxQpf = highestMaxQpf;
    }

    public float getLowestMaxQpf() {
        return lowestMaxQpf;
    }

    public void setLowestMaxQpf(float lowestMaxQpf) {
        this.lowestMaxQpf = lowestMaxQpf;
    }

    public Float getSnow12Mn() {
        return snow12Mn;
    }

    public void setSnow12Mn(Float snow12Mn) {
        this.snow12Mn = snow12Mn;
    }

    public Float getSnow12Mx() {
        return snow12Mx;
    }

    public void setSnow12Mx(Float snow12Mx) {
        this.snow12Mx = snow12Mx;
    }

    public String getAvgSkyCover() {
        return avgSkyCover;
    }

    public void setAvgSkyCover(String avgSkyCover) {
        this.avgSkyCover = avgSkyCover;

    }

    public String getObvis() {
        return obvis;
    }

    public void setObvis(String obvis) {
        this.obvis = obvis;
    }

    public float getWindChill() {
        return windChill;
    }

    public void setWindChill(float windChill) {
        this.windChill = windChill;
    }

    public float getHeatIndex() {
        return heatIndex;
    }

    public void setHeatIndex(float heatIndex) {
        this.heatIndex = heatIndex;
    }

    public float getMinChill() {
        return minChill;
    }

    public void setMinChill(float minChill) {
        this.minChill = minChill;
    }

    public float getMaxHeat() {
        return maxHeat;
    }

    public void setMaxHeat(float maxHeat) {
        this.maxHeat = maxHeat;
    }

    public String getRain() {
        return rain;
    }

    public void setRain(String rain) {
        this.rain = rain;
    }

    public String getRainShwrs() {
        return rainShwrs;
    }

    public void setRainShwrs(String rainShwrs) {
        this.rainShwrs = rainShwrs;
    }

    public String getSprinkles() {
        return sprinkles;
    }

    public void setSprinkles(String sprinkles) {
        this.sprinkles = sprinkles;
    }

    public String getTstms() {
        return tstms;
    }

    public void setTstms(String tstms) {
        this.tstms = tstms;
    }

    public String getDrizzle() {
        return drizzle;
    }

    public void setDrizzle(String drizzle) {
        this.drizzle = drizzle;
    }

    public String getSnow() {
        return snow;
    }

    public void setSnow(String snow) {
        this.snow = snow;
    }

    public String getSnowShwrs() {
        return snowShwrs;
    }

    public void setSnowShwrs(String snowShwrs) {
        this.snowShwrs = snowShwrs;
    }

    public String getFlurries() {
        return flurries;
    }

    public void setFlurries(String flurries) {
        this.flurries = flurries;
    }

    public String getSleet() {
        return sleet;
    }

    public void setSleet(String sleet) {
        this.sleet = sleet;
    }

    public String getFrzgRain() {
        return frzgRain;
    }

    public void setFrzgRain(String frzgRain) {
        this.frzgRain = frzgRain;
    }

    public String getFrzgDrzl() {
        return frzgDrzl;
    }

    public void setFrzgDrzl(String frzgDrzl) {
        this.frzgDrzl = frzgDrzl;
    }

    public String getHazards() {
        return hazards;
    }

    public void setHazards(String hazards) {
        this.hazards = hazards;
    }

    /**
     * Set Parameters record.
     */
    /*
     * 8888888888888888888888 public static PafmParameters setParms(String
     * pParameters) {
     * 
     * // New PafmParameters record to hold PafmRecord PafmParameters
     * currentParameters = new PafmParameters();
     * currentParameters.setParameters(pParameters);
     * 
     * return currentParameters;
     * 
     * } 888
     */

}
