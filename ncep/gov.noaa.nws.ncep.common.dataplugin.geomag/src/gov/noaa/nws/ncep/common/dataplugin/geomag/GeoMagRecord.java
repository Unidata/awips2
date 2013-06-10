package gov.noaa.nws.ncep.common.dataplugin.geomag;

import java.util.Calendar;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
/**
 * Record implementation for geomag plugin. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer           Description
 * ------------ ---------- ----------------   --------------------------
 * 03/27/2013   975        sgurung            Initial creation.
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

@Entity
@Table(name = "geomag", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GeoMagRecord extends PersistablePluginDataObject {

    private static final long serialVersionUID = 1L;

    public static final String OBS_TIME = "OBS_TIME";
    
    public static final String component1 = "component1";
    
    public static final String component2 = "component2";
    
    public static final String component3 = "component3";
    
    public static final String component4 = "component4";

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
    /*@Column
    @DynamicSerializeElement
    @XmlAttribute
    private String badDataPoint;*/
   
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
     * @return the min_data
     */
    public long[] getObsTimes() {
        return obs_times;
    }

    /**
     * @param comp1_data
     *            the comp1_data to set
     */
    public void setObsTimes(long[] obs_times) {
        this.obs_times = obs_times;
    }
    
    /**
     * @return the comp1_data
     */
    public float[] getComp1Data() {
        return comp1_data;
    }

    /**
     * @param comp1_data
     *            the comp1_data to set
     */
    public void setComp1Data(float[] h_data) {
        this.comp1_data = h_data;
    }
    
    /**
     * @return the comp2_data
     */
    public float[] getComp2Data() {
        return comp2_data;
    }

    /**
     * @param comp2_data
     *            the comp2_data to set
     */
    public void setComp2Data(float[] d_data) {
        this.comp2_data = d_data;
    }
    
    /**
     * @return the comp3_data
     */
    public float[] getComp3Data() {
        return comp3_data;
    }

    /**
     * @param comp3_data
     *            the comp3_data to set
     */
    public void setComp3Data(float[] z_data) {
        this.comp3_data = z_data;
    }
    
    /**
     * @return the comp4_data
     */
    public float[] getComp4Data() {
        return comp4_data;
    }

    /**
     * @param comp4_data
     *            the comp4_data to set
     */
    public void setComp4Data(float[] f_data) {
        this.comp4_data = f_data;
    }
    
    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }

    public void retrieveFromDataStore(IDataStore dataStore) {
        
        try {
            IDataRecord[] dataRec = dataStore.retrieve(getDataURI());
            for (int i = 0; i < dataRec.length; i++) {
            	if (dataRec[i].getName().equals(GeoMagRecord.component1)) {
               	     obs_times = (((LongDataRecord) dataRec[i]).getLongData());
                } 
                if (dataRec[i].getName().equals(GeoMagRecord.component1)) {
                	 comp1_data = (((FloatDataRecord) dataRec[i]).getFloatData());
                } 
                if (dataRec[i].getName().equals(GeoMagRecord.component2)) {
               	     comp2_data = (((FloatDataRecord) dataRec[i]).getFloatData());
                } 
                if (dataRec[i].getName().equals(GeoMagRecord.component3)) {
               	     comp3_data = (((FloatDataRecord) dataRec[i]).getFloatData());
                } 
                if (dataRec[i].getName().equals(GeoMagRecord.component4)) {
               	     comp4_data = (((FloatDataRecord) dataRec[i]).getFloatData());
                } 
            }

        } catch (Exception se) {
            se.printStackTrace();
        }
    }
   
}
