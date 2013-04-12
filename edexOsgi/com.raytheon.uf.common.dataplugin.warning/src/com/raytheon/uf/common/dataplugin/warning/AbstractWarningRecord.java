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

package com.raytheon.uf.common.dataplugin.warning;

import java.util.Calendar;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.serialization.adapters.GeometryAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;

/**
 * 
 * Warning Record
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/12/2007   1003        bwoodle     initial creation
 * 04/12/2013   1857        bgonzale    Added SequenceGenerator annotation.
 * 
 * </pre>
 * 
 * @author bwoodle
 * @version 1
 */
@Entity
@SequenceGenerator(name = PluginDataObject.ID_GEN)
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class AbstractWarningRecord extends PluginDataObject {

    private static final long serialVersionUID = 1L;

    @DataURI(position = 1)
    @Column(length = 32)
    @XmlAttribute
    @DynamicSerializeElement
    private String wmoid;

    @DataURI(position = 2)
    @Column(length = 4)
    @XmlAttribute
    @DynamicSerializeElement
    private String pil;

    @DataURI(position = 3)
    @Column(length = 4)
    @XmlAttribute
    @DynamicSerializeElement
    private String xxxid;

    @Column(columnDefinition = "text")
    @XmlAttribute
    @DynamicSerializeElement
    private String countyheader;

    @XmlElement
    @DynamicSerializeElement
    @OneToMany(cascade = CascadeType.ALL, mappedBy = "parentWarning", fetch = FetchType.EAGER)
    protected Set<UGCZone> ugczones = new HashSet<UGCZone>();

    @Column(columnDefinition = "text")
    @XmlAttribute
    @DynamicSerializeElement
    private String vtecstr;

    @Column(length = 4)
    @XmlAttribute
    @DynamicSerializeElement
    private String productClass;

    @DataURI(position = 4)
    @Column(length = 4)
    @XmlAttribute
    @DynamicSerializeElement
    private String act;

    @Column(length = 8)
    @XmlAttribute
    @DynamicSerializeElement
    @Index(name = "query_index", columnNames = { "officeid", "phensig" })
    private String officeid;

    @Column(length = 4)
    @XmlAttribute
    @DynamicSerializeElement
    private String phen;

    @Column(length = 4)
    @XmlAttribute
    @DynamicSerializeElement
    private String sig;

    @DataURI(position = 5)
    @Column(length = 4)
    @XmlAttribute
    @DynamicSerializeElement
    private String etn;

    /** vtec start time */
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar startTime;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar endTime;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar issueTime;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar purgeTime;

    @Column(length = 8)
    @XmlAttribute
    @DynamicSerializeElement
    private boolean ufn;

    @Column(name = "geometry", columnDefinition = "geometry")
    @Type(type = "com.raytheon.edex.db.objects.hibernate.GeometryType")
    @XmlJavaTypeAdapter(value = GeometryAdapter.class)
    @DynamicSerializeElement
    private Geometry geometry;

    @Transient
    @XmlAttribute
    @DynamicSerializeElement
    private String forecaster = "";

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer motdir;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer motspd;

    @Column(columnDefinition = "text")
    @XmlAttribute
    @DynamicSerializeElement
    private String loc;

    @Column(columnDefinition = "text")
    @XmlAttribute
    @DynamicSerializeElement
    private String rawmessage;

    @DataURI(position = 6)
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int seg;

    @DataURI(position = 7)
    @Column(length = 4)
    @XmlAttribute
    @DynamicSerializeElement
    private String phensig;

    @Transient
    @XmlAttribute
    @DynamicSerializeElement
    private String region;

    @Column(columnDefinition = "text")
    @XmlAttribute
    @DynamicSerializeElement
    private String overviewText;

    /** segment text */
    @Column(columnDefinition = "text")
    @XmlAttribute
    @DynamicSerializeElement
    private String segText;

    @Column(length = 8)
    @XmlAttribute
    @DynamicSerializeElement
    private String locationID;

    @Column(length = 2)
    @XmlAttribute
    @DynamicSerializeElement
    private String floodSeverity;

    @XmlAttribute
    @DynamicSerializeElement
    private String immediateCause;

    @Column(length = 2)
    @XmlAttribute
    @DynamicSerializeElement
    private String floodRecordStatus;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar floodBegin;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar floodCrest;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar floodEnd;

    /**
     * Constructor.
     */
    public AbstractWarningRecord() {
        super();
    }

    /**
     * Constructor to duplicate record.
     * 
     * @param message
     *            The text of the message
     */
    public AbstractWarningRecord(AbstractWarningRecord old) {
        super((String) old.getMessageData());
        this.setCountyheader(old.getCountyheader());
        this.setDataTime(old.getDataTime());
        this.setForecaster(old.getForecaster());
        this.setGeometry(old.getGeometry());
        this.setGeometry(old.getGeometry());
        this.setIdentifier(old.getIdentifier());
        this.setInsertTime(old.getInsertTime());
        this.setIssueTime(old.getIssueTime());
        this.setLoc(old.getLoc());
        this.setMotdir(old.getMotdir());
        this.setMotspd(old.getMotspd());
        this.setPil(old.getPil());
        this.setPhensig(old.getPhensig());
        this.setPluginName(old.getPluginName());
        this.setRawmessage(old.getRawmessage());
        this.setSeg(old.getSeg());
        this.setTraceId(old.getTraceId());
        this.setStartTime(old.getStartTime());
        this.setEndTime(old.getEndTime());
        this.setVtecstr(old.getVtecstr());
        this.setAct(old.getAct());
        this.setOfficeid(old.getOfficeid());
        this.setProductClass(old.getProductClass());
        this.setPhen(old.getPhen());
        this.setSig(old.getSig());
        this.setEtn(old.getEtn());
        this.setWmoid(old.getWmoid());
        this.setXxxid(old.getXxxid());
    }

    /**
     * Constructs a warning record from a dataURI
     * 
     * @param uri
     *            The dataURI
     * @param tableDef
     *            The table definition associated with this class
     */
    public AbstractWarningRecord(String uri) {
        super(uri);
        identifier = java.util.UUID.randomUUID().toString();
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }

    public String createWarningProduct() {
        return "Not yet implemented.";
    }

    public String getWmoid() {
        return wmoid;
    }

    public void setWmoid(String wmoid) {
        this.wmoid = wmoid;
    }

    public String getPil() {
        return pil;
    }

    public void setPil(String pil) {
        this.pil = pil;
    }

    public String getXxxid() {
        return xxxid;
    }

    public void setXxxid(String xxxid) {
        this.xxxid = xxxid;
    }

    public String getVtecstr() {
        return vtecstr;
    }

    public void setVtecstr(String vtec) {
        this.vtecstr = vtec;
    }

    public String getForecaster() {
        return forecaster;
    }

    public void setForecaster(String forecaster) {
        this.forecaster = forecaster;
    }

    public Integer getMotdir() {
        return motdir;
    }

    public void setMotdir(Integer motdir) {
        this.motdir = motdir;
    }

    public Integer getMotspd() {
        return motspd;
    }

    public void setMotspd(Integer motspd) {
        this.motspd = motspd;
    }

    public String getEtn() {
        return etn;
    }

    public void setEtn(String etn) {
        this.etn = etn;
    }

    public String getRawmessage() {
        return rawmessage;
    }

    public void setRawmessage(String rawmessage) {
        this.rawmessage = rawmessage;
    }

    public String getProductClass() {
        return productClass;
    }

    public void setProductClass(String vteck) {
        this.productClass = vteck;
    }

    public String getAct() {
        return act;
    }

    public void setAct(String action) {
        this.act = action;
    }

    public String getOfficeid() {
        return officeid;
    }

    public void setOfficeid(String oid) {
        this.officeid = oid;
    }

    public String getPhen() {
        return phen;
    }

    public void setPhen(String phen) {
        this.phen = phen;
    }

    public String getSig() {
        return sig;
    }

    public void setSig(String sig) {
        this.sig = sig;
    }

    public String getCountyheader() {
        return countyheader;
    }

    public void setCountyheader(String countyheader) {
        this.countyheader = countyheader;
    }

    public Geometry getGeometry() {
        return geometry;
    }

    public void setGeometry(Geometry geometry) {
        this.geometry = geometry;
    }

    public String getLoc() {
        return loc;
    }

    public void setLoc(String loc) {
        this.loc = loc;
    }

    /**
     * @return the segmentnumber
     */
    public int getSeg() {
        return seg;
    }

    /**
     * @param segmentnumber
     *            the segmentnumber to set
     */
    public void setSeg(int segmentnumber) {
        this.seg = segmentnumber;
    }

    /**
     * @return the phensig
     */
    public String getPhensig() {
        return phensig;
    }

    /**
     * @param phensig
     *            the phensig to set
     */
    public void setPhensig(String phensig) {
        this.phensig = phensig;
    }

    /**
     * @return the vtecstarttime
     */
    public Calendar getStartTime() {
        return startTime;
    }

    /**
     * @param vtecstarttime
     *            the vtecstarttime to set
     */
    public void setStartTime(Calendar vtecstarttime) {
        this.startTime = vtecstarttime;
    }

    /**
     * @return the vtecendtime
     */
    public Calendar getEndTime() {
        return endTime;
    }

    /**
     * @param vtecendtime
     *            the vtecendtime to set
     */
    public void setEndTime(Calendar vtecendtime) {
        this.endTime = vtecendtime;
    }

    /**
     * @return the timeissued
     */
    public Calendar getIssueTime() {
        return issueTime;
    }

    /**
     * @param timeissued
     *            the timeissued to set
     */
    public void setIssueTime(Calendar timeissued) {
        this.issueTime = timeissued;
    }

    /**
     * @return the purgeTime
     */
    public Calendar getPurgeTime() {
        return purgeTime;
    }

    /**
     * @param purgeTime
     *            the purgeTime to set
     */
    public void setPurgeTime(Calendar purgeTime) {
        this.purgeTime = purgeTime;
    }

    /**
     * @return the ugczones
     */
    public Set<UGCZone> getUgczones() {
        return ugczones;
    }

    /**
     * @param ugczones
     *            the ugczones to set
     */
    public void setUgczones(Set<UGCZone> ugczones) {
        this.ugczones = ugczones;
    }

    /**
     * @return the region
     */
    public String getRegion() {
        return region;
    }

    /**
     * @param region
     *            the region to set
     */
    public void setRegion(String region) {
        this.region = region;
    }

    /**
     * @return the locationID
     */
    public String getLocationID() {
        return locationID;
    }

    /**
     * @param locationID
     *            the locationID to set
     */
    public void setLocationID(String locationID) {
        this.locationID = locationID;
    }

    /**
     * @return the floodSeverity
     */
    public String getFloodSeverity() {
        return floodSeverity;
    }

    /**
     * @param floodSeverity
     *            the floodSeverity to set
     */
    public void setFloodSeverity(String floodSeverity) {
        this.floodSeverity = floodSeverity;
    }

    /**
     * @return the immediateCause
     */
    public String getImmediateCause() {
        return immediateCause;
    }

    /**
     * @param immediateCause
     *            the immediateCause to set
     */
    public void setImmediateCause(String immediateCause) {
        this.immediateCause = immediateCause;
    }

    /**
     * @return the floodRecordStatus
     */
    public String getFloodRecordStatus() {
        return floodRecordStatus;
    }

    /**
     * @param floodRecordStatus
     *            the floodRecordStatus to set
     */
    public void setFloodRecordStatus(String floodRecordStatus) {
        this.floodRecordStatus = floodRecordStatus;
    }

    /**
     * @return the floodBegin
     */
    public Calendar getFloodBegin() {
        return floodBegin;
    }

    /**
     * @param floodBegin
     *            the floodBegin to set
     */
    public void setFloodBegin(Calendar floodBegin) {
        this.floodBegin = floodBegin;
    }

    /**
     * @return the floodCrest
     */
    public Calendar getFloodCrest() {
        return floodCrest;
    }

    /**
     * @param floodCrest
     *            the floodCrest to set
     */
    public void setFloodCrest(Calendar floodCrest) {
        this.floodCrest = floodCrest;
    }

    /**
     * @return the floodEnd
     */
    public Calendar getFloodEnd() {
        return floodEnd;
    }

    /**
     * @param floodEnd
     *            the floodEnd to set
     */
    public void setFloodEnd(Calendar floodEnd) {
        this.floodEnd = floodEnd;
    }

    public abstract void setUgcs(List<String> list);

    /**
     * @return the ufn
     */
    public boolean isUfn() {
        return ufn;
    }

    /**
     * @param ufn
     *            the ufn to set
     */
    public void setUfn(boolean ufn) {
        this.ufn = ufn;
    }

    /**
     * @return the overviewText
     */
    public String getOverviewText() {
        return overviewText;
    }

    /**
     * @param overviewText
     *            the overviewText to set
     */
    public void setOverviewText(String overviewText) {
        this.overviewText = overviewText;
    }

    /**
     * @return the segText
     */
    public String getSegText() {
        return segText;
    }

    /**
     * @param segText
     *            the segText to set
     */
    public void setSegText(String segText) {
        this.segText = segText;
    }

    public String[] getUgcsString() {
        String[] s = new String[ugczones.size()];
        UGCZone[] ugcs = ugczones.toArray(new UGCZone[ugczones.size()]);
        for (int i = 0; i < ugcs.length; i++) {
            s[i] = ugcs[i].getZone();
        }
        return s;
    }

}
