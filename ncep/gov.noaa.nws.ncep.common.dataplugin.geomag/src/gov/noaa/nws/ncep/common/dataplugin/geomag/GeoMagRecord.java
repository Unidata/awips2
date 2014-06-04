package gov.noaa.nws.ncep.common.dataplugin.geomag;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IHDFFilePathProvider;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Record implementation for geomag plugin.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer           Description
 * ------------ ---------- ----------------   --------------------------
 * Mar 27, 2013 975        sgurung            Initial creation.
 * May 26, 2013            bhebbard           Added SequenceGenerator
 *                                            annotation.
 * Jun 26, 2013 989        qzhou              Added lots of fields.
 * Jul 22, 2013 1977       rjpeter            Added getDataURI and annotations.
 * Jul 26, 2013 989        qzhou              Added lots of fields.
 * Aug 30, 2013 2298       rjpeter            Make getPluginName abstract
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "geomagseq")
@Table(name = "geomag", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GeoMagRecord extends PersistablePluginDataObject {

    private static final long serialVersionUID = 1L;

    public static final String OBS_TIME = "Obs_Time";// h5

    public static final String Comp_Insert_Time = "Comp_Insert_Time";

    public static final String component1 = "Component1";

    public static final String component2 = "Component2";

    public static final String component3 = "Component3";

    public static final String component4 = "Component4";

    public static final String H_HR_AVG = "Hr_Avg_H";

    public static final String D_HR_AVG = "Hr_Avg_D";

    public static final String K_Index = "P3h_K_Index";

    public static final String K_Real = "P3h_K_Real";

    public static final String K_Gamma = "P3h_Gamma";

    public static final String Kest_Index = "P3h_Kest_Index";

    public static final String Kest_Real = "P3h_Kest_Real";

    public static final String Kest_Gamma = "P3h_Kest_Gamma";

    public static final String KH_Real = "P3h_Hk_Real";

    public static final String KH_Gamma = "P3h_H_Gamma";

    public static final String KD_Real = "P3h_Dk_Real";

    public static final String KD_Gamma = "P3h_D_Gamma";

    public static final String A_Running = "P3h_A_Running";

    public static final String A_Final_Running = "P3h_A_Final_Running";

    public static final String KH_Int = "P3h_Hk_Int";

    public static final String KD_Int = "P3h_Dk_Int";

    public static final String Last_Update = "P3h_Last_Update";

    public static final String Kest_Index_1m = "P1m_Kest_Index";

    public static final String Kest_Real_1m = "P1m_Kest_Real";

    public static final String Kest_Gamma_1m = "P1m_Kest_Gamma";

    public static final String KH_Real_1m = "P1m_Hk_Real";

    public static final String KH_Gamma_1m = "P1m_H_Gamma";

    public static final String KH_Index_1m = "P1m_Hk_Index";

    public static final String KD_Real_1m = "P1m_Dk_Real";

    public static final String KD_Gamma_1m = "P1m_D_Gamma";

    public static final String KD_Index_1m = "P1m_Dk_Index";

    public static final String KH_Count = "P1m_H_Count";

    public static final String KD_Count = "P1m_D_Count";

    public static final String KH_Dev = "P1m_H_Dev";

    public static final String KD_Dev = "P1m_D_Dev";

    public static final String A_est = "P1m_A_est";

    public static final String K_s = "P1m_K_s";

    public static final String Last_Update_1m = "P1m_Last_Update";

    /**
     * station code
     */
    @DataURI(position = 1)
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private String stationCode;

    /**
     * sourceId of data
     */
    @DataURI(position = 2)
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private int sourceId;

    /**
     * report type
     */
    @DataURI(position = 3)
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private String reportType;

    /**
     * flag to indicate bad data point
     */
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private int badDataPoint;

    /**
     * H or X values
     */
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private float component_1;

    /**
     * D or Y values
     */
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private float component_2;

    /**
     * Z values
     */
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private float component_3;

    /**
     * F values
     */
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private float component_4;

    /**
     * F values
     */
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private int distributionId;

    /**
     * F values
     */
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private int instrumentOrientationId;

    /**
     * F values
     */
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private int cadenceId;

    /**
     * Observation Date and Time for the minute values
     */
    @Transient
    private long[] obs_times;

    /**
     * H or X values
     */
    @Transient
    private float[] comp1_data;

    /**
     * D or Y values
     */
    @Transient
    private float[] comp2_data;

    /**
     * Z values
     */
    @Transient
    private float[] comp3_data;

    /**
     * F values
     */
    @Transient
    private float[] comp4_data;

    /**
     * No-arg Constructor
     */
    public GeoMagRecord() {
    }

    /**
     * Constructor for DataURI construction through base class. This is used by
     * the notification service.
     * 
     * @param uri
     *            A dataURI applicable to this class.
     */
    public GeoMagRecord(String uri) {
        super(uri);
    }

    /**
     * @return the instrument
     */
    public String getStationCode() {
        return stationCode;
    }

    /**
     * @param stationCode
     *            the stationCode to set
     */
    public void setStationCode(String stationCode) {
        this.stationCode = stationCode;
    }

    /**
     * @return the sourceId
     */
    public int getSourceId() {
        return sourceId;
    }

    /**
     * @param sourceId
     *            the sourceId to set
     */
    public void setSourceId(int sourceId) {
        this.sourceId = sourceId;
    }

    /**
     * @return the badDataPoint
     */
    public int getBadDataPoint() {
        return badDataPoint;
    }

    /**
     * @param the
     *            badDataPoint to set
     */
    public void setBadDataPoint(int badDataPoint) {
        this.badDataPoint = badDataPoint;
    }

    /**
     * @return the reportType
     */
    public String getReportType() {
        return reportType;
    }

    /**
     * @param reportType
     *            the reportType to set
     */
    public void setReportType(String reportType) {
        this.reportType = reportType;
    }

    /**
     * @return the obs_times array
     */
    public long[] getObsTimes() {
        return obs_times;
    }

    public void setObsTimes(long[] obs_times) {
        this.obs_times = obs_times;
    }

    /**
     * @return the comp1_data array
     */
    public float[] getComp1Data() {
        return comp1_data;
    }

    public void setComp1Data(float[] h_data) {
        this.comp1_data = h_data;
    }

    /**
     * @return the comp2_data array
     */
    public float[] getComp2Data() {
        return comp2_data;
    }

    public void setComp2Data(float[] d_data) {
        this.comp2_data = d_data;
    }

    /**
     * @return the comp3_data
     */
    public float[] getComp3Data() {
        return comp3_data;
    }

    public void setComp3Data(float[] z_data) {
        this.comp3_data = z_data;
    }

    /**
     * @return the comp4_data
     */
    public float[] getComp4Data() {
        return comp4_data;
    }

    public void setComp4Data(float[] f_data) {
        this.comp4_data = f_data;
    }

    /**
     * @return the component_1
     */
    public float getComponent_1() {
        return component_1;
    }

    public void setComponent_1(float component_1) {
        this.component_1 = component_1;
    }

    /**
     * @return the component_2
     */
    public float getComponent_2() {
        return component_2;
    }

    public void setComponent_2(float component_2) {
        this.component_2 = component_2;
    }

    /**
     * @return the component_3
     */
    public float getComponent_3() {
        return component_3;
    }

    public void setComponent_3(float component_3) {
        this.component_3 = component_3;
    }

    /**
     * @return the component_4
     */
    public float getComponent_4() {
        return component_4;
    }

    public void setComponent_4(float component_4) {
        this.component_4 = component_4;
    }

    /**
     * @return the h_HrAvg
     */
    public int getDistributionId() {
        return distributionId;
    }

    /**
     * @param h_HrAvg
     */
    public void setDistributionId(int distributionId) {
        this.distributionId = distributionId;
    }

    /**
     * @return the d_HrAvg
     */
    public int getInstrumentOrientationId() {
        return instrumentOrientationId;
    }

    /**
     * @param d_HrAvg
     */
    public void setInstrumentOrientationId(int instrumentOrientationId) {
        this.instrumentOrientationId = instrumentOrientationId;
    }

    public int getCadenceId() {
        return cadenceId;
    }

    public void setCadenceId(int cadenceId) {
        this.cadenceId = cadenceId;
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    @Override
    public IHDFFilePathProvider getHDFPathProvider() {
        return GeoMagPathProvider.getInstance();
    }

    @Override
    public String getPluginName() {
        return "geomag";
    }
}
