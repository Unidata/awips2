package gov.noaa.nws.ncep.common.dataplugin.solarimage;

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
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
/**
 * Record implementation for solarimage plugin. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer           Description
 * ------------ ---------- ----------------   --------------------------
 * 12/05/2012   865        sgurung, qzhou     Initial creation.
 * </pre>
 * 
 * @author sgurung, qzhou
 * @version 1.0
 */

@Entity
@Table(name = "solarimage", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SolarImageRecord extends PersistablePluginDataObject {

    private static final long serialVersionUID = 1L;

    public static final String RAW_DATA = "Raw_Data";

    /**
     * name of satellite/telescope
     */
    @DataURI(position = 1)
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private String satellite;

    /**
     * name of instrument and detector
     */
    @DataURI(position = 2)
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private String instrument;

    /**
     * wavelength
     */
    @DataURI(position = 3)
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private String wavelength;

    /**
     * integration time
     */
    @DataURI(position = 4)
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private float intTime;
    
    /**
     * hdu containing image data
     */
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private int imageHDUNum;
  
    /**
     * report type
     */
    @Column 
    @DynamicSerializeElement
    @XmlAttribute
    private String reportType;
    
    /**
     * raw data
     */
    @Transient
    private byte[] raw_data;

    /**
     * No-arg Constructor
     */
    public SolarImageRecord() {
    }

    /**
     * Constructor for DataURI construction through base class. This is used by
     * the notification service.
     * 
     * @param uri
     *            A dataURI applicable to this class.
     */
    public SolarImageRecord(String uri) {
        super(uri);
    }

    /**
     * @return the instrument
     */
    public String getInstrument() {
        return instrument;
    }

    /**
     * @param instrument
     *            the instrument to set
     */
    public void setInstrument(String instrument) {
        this.instrument = instrument;
    }
   

    /**
     * @return the satellite
     */
    public String getSatellite() {
        return satellite;
    }

    /**
     * @param satellite
     *            the satellite to set
     */
    public void setSatellite(String satellite) {
        this.satellite = satellite;
    }

    /**
     * @return the wavelength
     */
    public String getWavelength() {
        return wavelength;
    }

    /**
     * @param wavelength
     *            the wavelength to set
     */
    public void setWavelength(String wavelength) {
        this.wavelength = wavelength;
    }
  
    /**
     * @return the intTime
     */
    public float getIntTime() {
        return intTime;
    }

    /**
     * @param intTime
     *            the intTime to set
     */
    public void setIntTime(float intTime) {
        this.intTime = intTime;
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
     * @return imageHDUNum
     */
    public int getImageHDUNum() {
        return imageHDUNum;
    }
   
    /**
     * @param imageHDUNum
     *            the hdu containing image data
     */
    public void setImageHDUNum(int hduId) {
        this.imageHDUNum = hduId;
    }  
    
    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }
      
    /**
     * @return the raw_data
     */
    public byte[] getRawData() {
        return raw_data;
    }

    /**
     * @param raw_data
     *            the raw_data to set
     */
    public void setRawData(byte[] raw_data) {
        this.raw_data = raw_data;
    }

    public void retrieveFromDataStore(IDataStore dataStore) {
        
        try {
            IDataRecord[] dataRec = dataStore.retrieve(getDataURI());
            for (int i = 0; i < dataRec.length; i++) {
                if (dataRec[i].getName().equals(SolarImageRecord.RAW_DATA)) {
                	 raw_data = (((ByteDataRecord) dataRec[i]).getByteData());
                } 
            }

        } catch (Exception se) {
            se.printStackTrace();
        }
    }
   
}
