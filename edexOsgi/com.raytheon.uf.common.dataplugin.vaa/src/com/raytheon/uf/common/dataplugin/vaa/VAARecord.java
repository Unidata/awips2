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
package com.raytheon.uf.common.dataplugin.vaa;

import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 4, 2009            jkorman     Initial creation
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013       1857 bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from

 *                                     PluginDataObject.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */


@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "vaaseq")
@Table(name = "vaa", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "vaa",
		indexes = {
				@Index(name = "vaa_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class VAARecord extends PluginDataObject implements
        ISpatialEnabled {
    
    private static final long serialVersionUID = 1L;
    
    @Embedded
    @DataURI(position = 1, embedded = true)
    @XmlElement
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    /**
     * 
     */
    @Column(length = 8)
    @DataURI(position = 2)
    @XmlElement
    @DynamicSerializeElement
    private String recordType; 

    /**
     * 
     */
    @Column(length = 16)
    @DataURI(position = 3)
    @XmlElement
    @DynamicSerializeElement
    private String advisoryNumber; 
    
    // Correction indicator from wmo header
    @DataURI(position = 4)
    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String corIndicator;

    @Column(length = 32)
    @XmlElement
    @DynamicSerializeElement
    private String centerId;

    /**
     * 
     */
    @Column(length = 2048)
    @XmlElement
    @DynamicSerializeElement
    private String message; 

    /**
     * 
     */
    @Column(length = 512)
    @XmlElement
    @DynamicSerializeElement
    private String anal00Hr; 

    /**
     * 
     */
    @Column(length = 512)
    @XmlElement
    @DynamicSerializeElement
    private String fcst06Hr; 

    /**
     * 
     */
    @Column(length = 512)
    @XmlElement
    @DynamicSerializeElement
    private String fcst12Hr; 

    /**
     * 
     */
    @Column(length = 512)
    @XmlElement
    @DynamicSerializeElement
    private String fcst18Hr; 
    
    // Text of the WMO header
    @Column(length = 64)
    @XmlElement
    @DynamicSerializeElement
    private String wmoHeader = "";

    @DynamicSerializeElement
    @XmlElement
    @OneToMany(cascade = CascadeType.ALL, mappedBy = "parentId", fetch = FetchType.EAGER)
    private Set<VAASubPart> subParts = new HashSet<VAASubPart>();


    /**
     * Empty default constructor
     */
    public VAARecord() {
    }
    
    /**
     * Construct an instance of this class using the supplied datauri.
     * @param dataUri
     */
    public VAARecord(String dataUri) {
        super(dataUri);
    }
    
    /**
     * @return the corIndicator
     */
    public String getCorIndicator() {
        return corIndicator;
    }

    /**
     * @param corIndicator the corIndicator to set
     */
    public void setCorIndicator(String corIndicator) {
        this.corIndicator = corIndicator;
    }
    
    /**
     * @return the centerId
     */
    public String getCenterId() {
        return centerId;
    }

    /**
     * @param centerId the centerId to set
     */
    public void setCenterId(String centerId) {
        this.centerId = centerId;
    }

    /**
     * @return the wmoHeader
     */
    public String getWmoHeader() {
        return wmoHeader;
    }

    /**
     * @param wmoHeader the wmoHeader to set
     */
    public void setWmoHeader(String wmoHeader) {
        this.wmoHeader = wmoHeader;
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }
    
    @Override
    public SurfaceObsLocation getSpatialObject() {
        return location;
    }

    public SurfaceObsLocation getLocation() {
        return location;
    }

    public void setLocation(SurfaceObsLocation location) {
        this.location = location;
    }
    
    /**
     * Get this observation's geometry.
     * 
     * @return The geometry for this observation.
     */
    public Geometry getGeometry() {
        return location.getGeometry();
    }

    /**
     * Get the geometry latitude.
     * 
     * @return The geometry latitude.
     */
    public double getLatitude() {
        return location.getLatitude();
    }

    /**
     * Get the geometry longitude.
     * 
     * @return The geometry longitude.
     */
    public double getLongitude() {
        return location.getLongitude();
    }

    /**
     * Get the station identifier for this observation.
     * 
     * @return the stationId
     */
    public String getStationId() {
        return location.getStationId();
    }

    /**
     * Get the elevation, in meters, of the observing platform or location.
     * 
     * @return The observation elevation, in meters.
     */
    public Integer getElevation() {
        return location.getElevation();
    }

    /**
     * Get whether the location for this observation is defined.
     * 
     * @return Is this location defined.
     */
    public Boolean getLocationDefined() {
        return location.getLocationDefined();
    }
    
    
    
    /**
     * @return the recordType
     */
    public String getRecordType() {
        return recordType;
    }

    /**
     * @param recordType the recordType to set
     */
    public void setRecordType(String recordType) {
        this.recordType = recordType;
    }

    /**
     * @return the advisoryNumber
     */
    public String getAdvisoryNumber() {
        return advisoryNumber;
    }

    /**
     * @param advisoryNumber the advisoryNumber to set
     */
    public void setAdvisoryNumber(String advisoryNumber) {
        this.advisoryNumber = advisoryNumber;
    }

    /**
     * @return the message
     */
    public String getMessage() {
        return message;
    }

    /**
     * @param message the message to set
     */
    public void setMessage(String message) {
        this.message = message;
    }

    /**
     * @return the anal00Hr
     */
    public String getAnal00Hr() {
        return anal00Hr;
    }

    /**
     * @param anal00Hr the anal00Hr to set
     */
    public void setAnal00Hr(String anal00Hr) {
        this.anal00Hr = anal00Hr;
    }

    /**
     * @return the fcst06Hr
     */
    public String getFcst06Hr() {
        return fcst06Hr;
    }

    /**
     * @param fcst06Hr the fcst06Hr to set
     */
    public void setFcst06Hr(String fcst06Hr) {
        this.fcst06Hr = fcst06Hr;
    }

    /**
     * @return the fcst12Hr
     */
    public String getFcst12Hr() {
        return fcst12Hr;
    }

    /**
     * @param fcst12Hr the fcst12Hr to set
     */
    public void setFcst12Hr(String fcst12Hr) {
        this.fcst12Hr = fcst12Hr;
    }

    /**
     * @return the fcst18Hr
     */
    public String getFcst18Hr() {
        return fcst18Hr;
    }

    /**
     * @param fcst18Hr the fcst18Hr to set
     */
    public void setFcst18Hr(String fcst18Hr) {
        this.fcst18Hr = fcst18Hr;
    }

    public void addSubPart(VAASubPart part) {
        if(subParts == null) {
            subParts = new HashSet<VAASubPart>();
        }
        part.setParentId(this);
        subParts.add(part);
    }
    
    
    /**
     * @return the subParts
     */
    public Set<VAASubPart> getSubParts() {
        return subParts;
    }

    /**
     * @param subParts the subParts to set
     */
    public void setSubParts(Set<VAASubPart> subParts) {
        for(VAASubPart p : subParts) {
            p.setParentId(this);
        }
        this.subParts = subParts;
    }
    
    /**
     * 
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("VAA:");
        sb.append("VAAC: ");
        sb.append(centerId);
        sb.append(" PSN:[");
        sb.append(getLatitude());
        sb.append(",");
        sb.append(getLongitude());
        sb.append("]");
        
        
        return sb.toString();
    }
    
//    "\r\r\nVA ADVISORY" +
//    "\r\r\nDTG: 20091104/1708Z" +
//    "\r\r\nVAAC: WASHINGTON" +
//    "\r\r\nVOLCANO: SOUFRIERE HILLS 1600-05" +
//    "\r\r\nPSN: N1642 W06210" +
//    "\r\r\nAREA: W_INDIES" +
//    "\r\r\nSUMMIT ELEV: 3002 FT (915 M)" +
//    "\r\r\nADVISORY NR: 2009/146" +
//    "\r\r\nINFO SOURCE: GOES-12. GFS WINDS." +
//    "\r\r\nERUPTION DETAILS: CONTINUOUS EMISSIONS" +
//    "\r\r\nOBS VA DTG: 04/1645Z" +
//    "\r\r\nOBS VA CLD: SFC/FL100 42NM WID LINE BTN N1638" +
//    "\r\r\nW06611 - N1643 W06214. MOV W 7KT" +
//    "\r\r\nFCST VA CLD +6HR: 04/2300Z SFC/FL100 40NM WID" +
//    "\r\r\nLINE BTN N1640 W06614 - N1644 W06214." +
//    "\r\r\nFCST VA CLD +12HR: 05/0500Z SFC/FL100 40NM WID" +
//    "\r\r\nLINE BTN N1638 W06614 - N1643 W06214. SFC/FL100" +
//    "\r\r\n40NM WID LINE BTN N1641 W06616 - N1643 W06214." +
//    "\r\r\nFCST VA CLD +18HR: 05/1100Z" +
//    "\r\r\nRMK: A SPREADING 42 NMI WIDE ASH PLUME MOVING AT" +
//    "\r\r\nA MEASURED 7 KTS EXTENDS AT LEAST 211 NMI TO THE" +
//    "\r\r\nWEST OF THE VOLCANO, OR TO ABOUT 66W.  NO" +
//    "\r\r\nSIGNIFICANT CHANGE IN DIRECTION OR SPEED IS" +
//    "\r\r\nANTICIPATED DURING THE NEXT 12 HOURS. ...BALDWIN" +
//    "\r\r\nNXT ADVISORY: WILL BE ISSUED BY 20091104/2315Z" +

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }
}
