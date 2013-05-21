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

import java.util.Arrays;
import java.util.Calendar;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.regex.Pattern;

import javax.persistence.Column;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.MappedSuperclass;
import javax.persistence.SequenceGenerator;
import javax.persistence.Transient;

import org.hibernate.annotations.Type;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Warning Record
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/12/2007   1003        bwoodle     initial creation
 * 04/12/2013   1857        bgonzale    Added SequenceGenerator annotation.
 * 05/02/2013   1949        rjpeter     Moved ugcZones to be a column inside table.
 * </pre>
 * 
 * @author bwoodle
 * @version 1
 */
@MappedSuperclass
@SequenceGenerator(name = PluginDataObject.ID_GEN)
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
@DynamicSerialize
public abstract class AbstractWarningRecord extends PluginDataObject {
    private static final Pattern ugcSplitter = Pattern.compile(", ");

    private static final long serialVersionUID = 1L;

    @DataURI(position = 1)
    @Column(length = 32)
    @DynamicSerializeElement
    private String wmoid;

    @DataURI(position = 2)
    @Column(length = 4)
    @DynamicSerializeElement
    private String pil;

    @DataURI(position = 3)
    @Column(length = 4)
    @DynamicSerializeElement
    private String xxxid;

    @Column(columnDefinition = "text")
    @DynamicSerializeElement
    private String countyheader;

    @Column(name = "ugczones", columnDefinition = "text")
    @DynamicSerializeElement
    protected String ugcZoneList;

    @Transient
    protected Set<String> ugcZones;

    @Column(columnDefinition = "text")
    @DynamicSerializeElement
    private String vtecstr;

    @Column(length = 4)
    @DynamicSerializeElement
    private String productClass;

    @DataURI(position = 4)
    @Column(length = 4)
    @DynamicSerializeElement
    private String act;

    @Column(length = 8)
    @DynamicSerializeElement
    private String officeid;

    @Column(length = 4)
    @DynamicSerializeElement
    private String phen;

    @Column(length = 4)
    @DynamicSerializeElement
    private String sig;

    @DataURI(position = 5)
    @Column(length = 4)
    @DynamicSerializeElement
    private String etn;

    /** vtec start time */
    @DynamicSerializeElement
    @Column
    private Calendar startTime;

    @Column
    @DynamicSerializeElement
    private Calendar endTime;

    @Column
    @DynamicSerializeElement
    private Calendar issueTime;

    @Column
    @DynamicSerializeElement
    private Calendar purgeTime;

    @Column(length = 8)
    @DynamicSerializeElement
    private boolean ufn;

    @Column(name = "geometry", columnDefinition = "geometry")
    @Type(type = "com.raytheon.edex.db.objects.hibernate.GeometryType")
    @DynamicSerializeElement
    private Geometry geometry;

    @Transient
    @DynamicSerializeElement
    private String forecaster = "";

    @Column
    @DynamicSerializeElement
    private Integer motdir;

    @Column
    @DynamicSerializeElement
    private Integer motspd;

    @Column(columnDefinition = "text")
    @DynamicSerializeElement
    private String loc;

    @Column(columnDefinition = "text")
    @DynamicSerializeElement
    private String rawmessage;

    @DataURI(position = 6)
    @Column
    @DynamicSerializeElement
    private int seg;

    @DataURI(position = 7)
    @Column(length = 4)
    @DynamicSerializeElement
    private String phensig;

    @Transient
    @DynamicSerializeElement
    private String region;

    @Column(columnDefinition = "text")
    @DynamicSerializeElement
    private String overviewText;

    /** segment text */
    @Column(columnDefinition = "text")
    @DynamicSerializeElement
    private String segText;

    @Column(length = 8)
    @DynamicSerializeElement
    private String locationID;

    @Column(length = 2)
    @DynamicSerializeElement
    private String floodSeverity;

    @Column
    @DynamicSerializeElement
    private String immediateCause;

    @Column(length = 2)
    @DynamicSerializeElement
    private String floodRecordStatus;

    @Column
    @DynamicSerializeElement
    private Calendar floodBegin;

    @Column
    @DynamicSerializeElement
    private Calendar floodCrest;

    @Column
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

    public void setUgcZones(Set<String> list) {
        ugcZones = new LinkedHashSet<String>(list);
        StringBuilder builder = new StringBuilder(ugcZones.size() * 8);
        boolean addComma = false;
        for (String ugc : list) {
            if (addComma) {
                builder.append(", ");
            } else {
                addComma = true;
            }
            builder.append(ugc);
        }
        ugcZoneList = builder.toString();
    }

    public Set<String> getUgcZones() {
        if (ugcZones == null) {
            ugcZones = new LinkedHashSet<String>();

            if ((ugcZoneList != null) && (ugcZoneList.length() > 0)) {
                String[] zones = ugcSplitter.split(ugcZoneList);
                ugcZones.addAll(Arrays.asList(zones));
            }
        }

        return ugcZones;
    }

    public String getUgcZoneList() {
        return ugcZoneList;
    }

    public void setUgcZoneList(String ugcZoneList) {
        this.ugcZoneList = ugcZoneList;
        this.ugcZones = null;
    }
}
