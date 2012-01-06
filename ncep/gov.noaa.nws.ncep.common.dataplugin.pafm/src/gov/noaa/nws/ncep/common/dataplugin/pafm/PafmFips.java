/**
 * PafmFips
 * 
 * This java class represents the county FIPS for a PAFM record.
 * 
 * HISTORY
 *
 * Date     	Ticket # Author		Description
 * ------------	-------- ----------	-----------	--------------------------
 * 08/05/09		   126   F. J. Yen	Initial creation based on AWW Decoder
 * 12/11/09		   126   F. J. Yen	Migrated from to11d3 to to11d6
 * 01/06/10		   126	 F. J. Yen	Migrated and refactored from to11dr3 to to11dr11
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.dataplugin.pafm;

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

import java.io.Serializable;

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
@Table(name = "pafm_fips")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class PafmFips implements Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue
    private Integer recordId = null;

    // The county FIPS
    @Column(length = 16)
    @XmlElement
    @DynamicSerializeElement
    private String fips;

    // The universal geographic code
    /*
     * 888888888888888888
     * 
     * @Column(length=640)
     * 
     * @XmlElement
     * 
     * @DynamicSerializeElement private String ugc; 888
     */

    // The elevation
    @XmlElement
    @DynamicSerializeElement
    private Float elev;

    // The latitude
    @XmlElement
    @DynamicSerializeElement
    private Float lat;

    // The longitude
    @XmlElement
    @DynamicSerializeElement
    private Float lon;

    /**
     * No-Arg Constructor.
     */
    public PafmFips() {
        this.fips = null;
        this.elev = IDecoderConstantsN.FLOAT_MISSING;
        this.lat = IDecoderConstantsN.FLOAT_MISSING;
        this.lon = IDecoderConstantsN.FLOAT_MISSING;
        // this.ugc=null;
    }

    /**
     * @return the serialVersionUID
     */
    public static long getSerialVersionUID() {
        return serialVersionUID;
    }

    /**
     * Get the record id.
     * 
     * @return The recordId. If not set returns null.
     */
    public Integer getRecordId() {
        return recordId;
    }

    /**
     * Set the record id.
     * 
     * @param record
     */
    public void setRecordId(Integer recordId) {
        this.recordId = recordId;
    }

    /**
     * @return the fips
     */
    public String getFips() {
        return fips;
    }

    /**
     * @param fips
     *            to set
     */
    public void setFips(String fips) {
        this.fips = fips;
    }

    /**
     * @return the elev
     */
    public Float getElev() {
        return elev;
    }

    /**
     * @param elev
     *            to set
     */
    public void setElev(Float elev) {
        this.elev = elev;
    }

    /**
     * @return the lat
     */
    public Float getLat() {
        return lat;
    }

    /**
     * @param lat
     *            to set
     */
    public void setLat(Float lat) {
        this.lat = lat;
    }

    /**
     * @return the lon
     */
    public Float getLon() {
        return lon;
    }

    /**
     * @param lon
     *            to set
     */
    public void setLon(Float lon) {
        this.lon = lon;
    }

    /**
     * @return the ugc
     */
    /*
     * 888888888888888 public String getUgc() { return ugc; } 888
     */

    /**
     * @param ugc
     *            to set
     */
    /*
     * 888888888888888888888 public void setUgc(String ugc) { this.ugc = ugc; }
     * 888
     */

    /**
     * Set FIPS record.
     */
    public static PafmFips setFIPS(String countyFips) {
        // New PafmFips record to hold county FIPS
        PafmFips currentFips = new PafmFips();
        currentFips.setFips(countyFips);
        return currentFips;
    }
}
