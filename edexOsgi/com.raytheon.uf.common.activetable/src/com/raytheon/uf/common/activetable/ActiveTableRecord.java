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
package com.raytheon.uf.common.activetable;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.MappedSuperclass;
import javax.persistence.Transient;

import org.hibernate.annotations.Type;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Nearly identical to the WarningRecord, for storing the ActiveTable in a
 * database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 24, 2009            njensen     Initial creation
 * Feb 26, 2013 1447       dgilling    Implement equals().
 * May 10, 2013 1951       rjpeter     Added own id sequence tagging
 * Jul 16, 2013 2181       bsteffen    Convert geometry types to use hibernate-
 *                                     spatial
 * 10/16/2014   3454       bphillip    Upgrading to Hibernate 4
 * 04/28/2015   4027       randerso    Expunged Calendar from ActiveTableRecord
 * 05/22/2015   4522       randerso    Create proper primary key for ActiveTableRecord
 * 08/04/2015   4712       bphillip    Added parameter to PersistableDataObject
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */
@MappedSuperclass
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
@DynamicSerialize
// TODO: do we get anything from extending PersistableDataObject here?
public abstract class ActiveTableRecord extends PersistableDataObject<ActiveTableKey> {
    protected static final long serialVersionUID = 1L;

    @EmbeddedId
    @DynamicSerializeElement
    protected ActiveTableKey key;

    @Column(length = 22)
    @DynamicSerializeElement
    protected String wmoid;

    @Column(length = 4)
    @DynamicSerializeElement
    protected String pil;

    @Column(length = 4)
    @DynamicSerializeElement
    protected String xxxid;

    @Column(columnDefinition = "text")
    @DynamicSerializeElement
    protected String countyheader;

    @Column(length = 48)
    @DynamicSerializeElement
    protected String vtecstr;

    @Column(length = 1)
    @DynamicSerializeElement
    protected String productClass;

    @DataURI(position = 4)
    @Column(length = 3)
    @DynamicSerializeElement
    protected String act;

    /** vtec start time */
    @DynamicSerializeElement
    protected Date startTime;

    @Column
    @DynamicSerializeElement
    protected Date endTime;

    @Column
    @DynamicSerializeElement
    protected Date issueTime;

    @Column
    @DynamicSerializeElement
    protected Date purgeTime;

    @Column
    @DynamicSerializeElement
    protected boolean ufn;

    @Column
    @Type(type = "org.hibernate.spatial.GeometryType")
    @DynamicSerializeElement
    protected Geometry geometry;

    @Transient
    @DynamicSerializeElement
    protected String forecaster = "";

    @Column
    @DynamicSerializeElement
    protected Integer motdir;

    @Column
    @DynamicSerializeElement
    protected Integer motspd;

    // TODO: make this column a Geometry
    @Column(columnDefinition = "text")
    @DynamicSerializeElement
    protected String loc;

    @Column(columnDefinition = "text")
    @DynamicSerializeElement
    protected String rawmessage;

    @DataURI(position = 6)
    @Column
    @DynamicSerializeElement
    protected int seg;

    @DataURI(position = 7)
    @Column(length = 4)
    @DynamicSerializeElement
    protected String phensig;

    @Transient
    @DynamicSerializeElement
    protected String region;

    @Column(columnDefinition = "text")
    @DynamicSerializeElement
    protected String overviewText;

    /** segment text */
    @Column(columnDefinition = "text")
    @DynamicSerializeElement
    protected String segText;

    @Column(length = 5)
    @DynamicSerializeElement
    protected String locationID;

    @Column(length = 1)
    @DynamicSerializeElement
    protected String floodSeverity;

    @Column(length = 2)
    @DynamicSerializeElement
    protected String immediateCause;

    @Column(length = 2)
    @DynamicSerializeElement
    protected String floodRecordStatus;

    @Column
    @DynamicSerializeElement
    protected Date floodBegin;

    @Column
    @DynamicSerializeElement
    protected Date floodCrest;

    @Column
    @DynamicSerializeElement
    protected Date floodEnd;

    public ActiveTableRecord() {
        this.key = new ActiveTableKey();
    }

    @Override
    public abstract Object clone();

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        ActiveTableRecord other = (ActiveTableRecord) obj;
        if (act == null) {
            if (other.act != null) {
                return false;
            }
        } else if (!act.equals(other.act)) {
            return false;
        }
        if (countyheader == null) {
            if (other.countyheader != null) {
                return false;
            }
        } else if (!countyheader.equals(other.countyheader)) {
            return false;
        }
        if (endTime == null) {
            if (other.endTime != null) {
                return false;
            }
        } else if (!endTime.equals(other.endTime)) {
            return false;
        }
        if (floodBegin == null) {
            if (other.floodBegin != null) {
                return false;
            }
        } else if (!floodBegin.equals(other.floodBegin)) {
            return false;
        }
        if (floodCrest == null) {
            if (other.floodCrest != null) {
                return false;
            }
        } else if (!floodCrest.equals(other.floodCrest)) {
            return false;
        }
        if (floodEnd == null) {
            if (other.floodEnd != null) {
                return false;
            }
        } else if (!floodEnd.equals(other.floodEnd)) {
            return false;
        }
        if (floodRecordStatus == null) {
            if (other.floodRecordStatus != null) {
                return false;
            }
        } else if (!floodRecordStatus.equals(other.floodRecordStatus)) {
            return false;
        }
        if (floodSeverity == null) {
            if (other.floodSeverity != null) {
                return false;
            }
        } else if (!floodSeverity.equals(other.floodSeverity)) {
            return false;
        }
        if (forecaster == null) {
            if (other.forecaster != null) {
                return false;
            }
        } else if (!forecaster.equals(other.forecaster)) {
            return false;
        }
        if (geometry == null) {
            if (other.geometry != null) {
                return false;
            }
        } else if (!geometry.equals(other.geometry)) {
            return false;
        }
        if (immediateCause == null) {
            if (other.immediateCause != null) {
                return false;
            }
        } else if (!immediateCause.equals(other.immediateCause)) {
            return false;
        }
        if (issueTime == null) {
            if (other.issueTime != null) {
                return false;
            }
        } else if (!issueTime.equals(other.issueTime)) {
            return false;
        }
        if (key == null) {
            if (other.key != null) {
                return false;
            }
        } else if (!key.equals(other.key)) {
            return false;
        }
        if (loc == null) {
            if (other.loc != null) {
                return false;
            }
        } else if (!loc.equals(other.loc)) {
            return false;
        }
        if (locationID == null) {
            if (other.locationID != null) {
                return false;
            }
        } else if (!locationID.equals(other.locationID)) {
            return false;
        }
        if (motdir == null) {
            if (other.motdir != null) {
                return false;
            }
        } else if (!motdir.equals(other.motdir)) {
            return false;
        }
        if (motspd == null) {
            if (other.motspd != null) {
                return false;
            }
        } else if (!motspd.equals(other.motspd)) {
            return false;
        }
        if (overviewText == null) {
            if (other.overviewText != null) {
                return false;
            }
        } else if (!overviewText.equals(other.overviewText)) {
            return false;
        }
        if (phensig == null) {
            if (other.phensig != null) {
                return false;
            }
        } else if (!phensig.equals(other.phensig)) {
            return false;
        }
        if (pil == null) {
            if (other.pil != null) {
                return false;
            }
        } else if (!pil.equals(other.pil)) {
            return false;
        }
        if (productClass == null) {
            if (other.productClass != null) {
                return false;
            }
        } else if (!productClass.equals(other.productClass)) {
            return false;
        }
        if (purgeTime == null) {
            if (other.purgeTime != null) {
                return false;
            }
        } else if (!purgeTime.equals(other.purgeTime)) {
            return false;
        }
        if (rawmessage == null) {
            if (other.rawmessage != null) {
                return false;
            }
        } else if (!rawmessage.equals(other.rawmessage)) {
            return false;
        }
        if (region == null) {
            if (other.region != null) {
                return false;
            }
        } else if (!region.equals(other.region)) {
            return false;
        }
        if (seg != other.seg) {
            return false;
        }
        if (segText == null) {
            if (other.segText != null) {
                return false;
            }
        } else if (!segText.equals(other.segText)) {
            return false;
        }
        if (startTime == null) {
            if (other.startTime != null) {
                return false;
            }
        } else if (!startTime.equals(other.startTime)) {
            return false;
        }
        if (ufn != other.ufn) {
            return false;
        }
        if (vtecstr == null) {
            if (other.vtecstr != null) {
                return false;
            }
        } else if (!vtecstr.equals(other.vtecstr)) {
            return false;
        }
        if (wmoid == null) {
            if (other.wmoid != null) {
                return false;
            }
        } else if (!wmoid.equals(other.wmoid)) {
            return false;
        }
        if (xxxid == null) {
            if (other.xxxid != null) {
                return false;
            }
        } else if (!xxxid.equals(other.xxxid)) {
            return false;
        }
        return true;
    }

    /**
     * @return the key
     */
    public ActiveTableKey getKey() {
        return key;
    }

    /**
     * @param key
     *            the key to set
     */
    public void setKey(ActiveTableKey key) {
        this.key = key;
    }

    /**
     * @return the wmoid
     */
    public String getWmoid() {
        return wmoid;
    }

    /**
     * @param wmoid
     *            the wmoid to set
     */
    public void setWmoid(String wmoid) {
        this.wmoid = wmoid;
    }

    /**
     * @return the pil
     */
    public String getPil() {
        return pil;
    }

    /**
     * @param pil
     *            the pil to set
     */
    public void setPil(String pil) {
        this.pil = pil;
    }

    /**
     * @return the xxxid
     */
    public String getXxxid() {
        return xxxid;
    }

    /**
     * @param xxxid
     *            the xxxid to set
     */
    public void setXxxid(String xxxid) {
        this.xxxid = xxxid;
    }

    /**
     * @return the countyheader
     */
    public String getCountyheader() {
        return countyheader;
    }

    /**
     * @param countyheader
     *            the countyheader to set
     */
    public void setCountyheader(String countyheader) {
        this.countyheader = countyheader;
    }

    /**
     * @return the ugcZone
     */
    public String getUgcZone() {
        return key.ugcZone;
    }

    /**
     * @param ugcZone
     *            the ugcZone to set
     */
    public void setUgcZone(String ugcZone) {
        this.key.ugcZone = ugcZone;
    }

    /**
     * @return the vtecstr
     */
    public String getVtecstr() {
        return vtecstr;
    }

    /**
     * @param vtecstr
     *            the vtecstr to set
     */
    public void setVtecstr(String vtecstr) {
        this.vtecstr = vtecstr;
    }

    /**
     * @return the productClass
     */
    public String getProductClass() {
        return productClass;
    }

    /**
     * @param productClass
     *            the productClass to set
     */
    public void setProductClass(String productClass) {
        this.productClass = productClass;
    }

    /**
     * @return the act
     */
    public String getAct() {
        return act;
    }

    /**
     * @param act
     *            the act to set
     */
    public void setAct(String act) {
        this.act = act;
    }

    /**
     * @return the officeid
     */
    public String getOfficeid() {
        return key.officeid;
    }

    /**
     * @param officeid
     *            the officeid to set
     */
    public void setOfficeid(String officeid) {
        this.key.officeid = officeid;
    }

    /**
     * @return the phen
     */
    public String getPhen() {
        return key.phen;
    }

    /**
     * @param phen
     *            the phen to set
     */
    public void setPhen(String phen) {
        this.key.phen = phen;
    }

    /**
     * @return the sig
     */
    public String getSig() {
        return key.sig;
    }

    /**
     * @param sig
     *            the sig to set
     */
    public void setSig(String sig) {
        this.key.sig = sig;
    }

    /**
     * @return the etn
     */
    public String getEtn() {
        return key.etn;
    }

    /**
     * @param etn
     *            the etn to set
     */
    public void setEtn(String etn) {
        this.key.etn = etn;
    }

    /**
     * @return the startTime
     */
    public Date getStartTime() {
        return startTime;
    }

    /**
     * @param startTime
     *            the startTime to set
     */
    public void setStartTime(Date startTime) {
        this.startTime = startTime;
    }

    /**
     * @return the endTime
     */
    public Date getEndTime() {
        return endTime;
    }

    /**
     * @param endTime
     *            the endTime to set
     */
    public void setEndTime(Date endTime) {
        this.endTime = endTime;
    }

    /**
     * @return the issueTime
     */
    public Date getIssueTime() {
        return issueTime;
    }

    /**
     * @param issueTime
     *            the issueTime to set
     */
    public void setIssueTime(Date issueTime) {
        this.issueTime = issueTime;
    }

    /**
     * @return the purgeTime
     */
    public Date getPurgeTime() {
        return purgeTime;
    }

    /**
     * @param purgeTime
     *            the purgeTime to set
     */
    public void setPurgeTime(Date purgeTime) {
        this.purgeTime = purgeTime;
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
     * @return the geometry
     */
    public Geometry getGeometry() {
        return geometry;
    }

    /**
     * @param geometry
     *            the geometry to set
     */
    public void setGeometry(Geometry geometry) {
        this.geometry = geometry;
    }

    /**
     * @return the forecaster
     */
    public String getForecaster() {
        return forecaster;
    }

    /**
     * @param forecaster
     *            the forecaster to set
     */
    public void setForecaster(String forecaster) {
        this.forecaster = forecaster;
    }

    /**
     * @return the motdir
     */
    public Integer getMotdir() {
        return motdir;
    }

    /**
     * @param motdir
     *            the motdir to set
     */
    public void setMotdir(Integer motdir) {
        this.motdir = motdir;
    }

    /**
     * @return the motspd
     */
    public Integer getMotspd() {
        return motspd;
    }

    /**
     * @param motspd
     *            the motspd to set
     */
    public void setMotspd(Integer motspd) {
        this.motspd = motspd;
    }

    /**
     * @return the loc
     */
    public String getLoc() {
        return loc;
    }

    /**
     * @param loc
     *            the loc to set
     */
    public void setLoc(String loc) {
        this.loc = loc;
    }

    /**
     * @return the rawmessage
     */
    public String getRawmessage() {
        return rawmessage;
    }

    /**
     * @param rawmessage
     *            the rawmessage to set
     */
    public void setRawmessage(String rawmessage) {
        this.rawmessage = rawmessage;
    }

    /**
     * @return the seg
     */
    public int getSeg() {
        return seg;
    }

    /**
     * @param seg
     *            the seg to set
     */
    public void setSeg(int seg) {
        this.seg = seg;
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
    public Date getFloodBegin() {
        return floodBegin;
    }

    /**
     * @param floodBegin
     *            the floodBegin to set
     */
    public void setFloodBegin(Date floodBegin) {
        this.floodBegin = floodBegin;
    }

    /**
     * @return the floodCrest
     */
    public Date getFloodCrest() {
        return floodCrest;
    }

    /**
     * @param floodCrest
     *            the floodCrest to set
     */
    public void setFloodCrest(Date floodCrest) {
        this.floodCrest = floodCrest;
    }

    /**
     * @return the floodEnd
     */
    public Date getFloodEnd() {
        return floodEnd;
    }

    /**
     * @param floodEnd
     *            the floodEnd to set
     */
    public void setFloodEnd(Date floodEnd) {
        this.floodEnd = floodEnd;
    }

    public Object internalClone(ActiveTableRecord atr) {
        atr.setAct(this.getAct());
        atr.setCountyheader(this.getCountyheader());
        atr.setEndTime(this.getEndTime());
        atr.setEtn(this.getEtn());
        atr.setFloodBegin(this.getFloodBegin());
        atr.setFloodCrest(this.getFloodCrest());
        atr.setFloodEnd(this.getFloodEnd());
        atr.setFloodRecordStatus(this.getFloodRecordStatus());
        atr.setFloodSeverity(this.getFloodSeverity());
        atr.setForecaster(this.getForecaster());
        atr.setGeometry(this.getGeometry());
        atr.setImmediateCause(this.getImmediateCause());
        atr.setIssueTime(this.getIssueTime());
        atr.setLoc(this.getLoc());
        atr.setLocationID(this.getLocationID());
        atr.setMotdir(this.getMotdir());
        atr.setMotspd(this.getMotspd());
        atr.setOfficeid(this.getOfficeid());
        atr.setOverviewText(this.getOverviewText());
        atr.setPhen(this.getPhen());
        atr.setPhensig(this.getPhensig());
        atr.setPil(this.getPil());
        atr.setProductClass(this.getProductClass());
        atr.setPurgeTime(this.getPurgeTime());
        atr.setRawmessage(this.getRawmessage());
        atr.setRegion(this.getRegion());
        atr.setSeg(this.getSeg());
        atr.setSegText(this.getSegText());
        atr.setSig(this.getSig());
        atr.setStartTime(this.getStartTime());
        atr.setUfn(this.isUfn());
        atr.setVtecstr(this.getVtecstr());
        atr.setWmoid(this.getWmoid());
        atr.setXxxid(this.getXxxid());
        atr.setUgcZone(this.getUgcZone());
        return atr;
    }

    public static List<ActiveTableRecord> transformFromWarnings(
            List<AbstractWarningRecord> warnings, ActiveTableMode mode) {
        List<ActiveTableRecord> list = new ArrayList<ActiveTableRecord>();
        for (AbstractWarningRecord wr : warnings) {
            ActiveTableRecord atr = null;
            if (mode.equals(ActiveTableMode.OPERATIONAL)) {
                atr = new OperationalActiveTableRecord();
            } else {
                atr = new PracticeActiveTableRecord();
            }
            atr.setAct(wr.getAct());
            atr.setCountyheader(wr.getCountyheader());
            atr.setEndTime(calendarToDate(wr.getEndTime()));
            atr.setEtn(wr.getEtn());
            atr.setFloodBegin(calendarToDate(wr.getFloodBegin()));
            atr.setFloodCrest(calendarToDate(wr.getFloodCrest()));
            atr.setFloodEnd(calendarToDate(wr.getFloodEnd()));
            atr.setFloodRecordStatus(wr.getFloodRecordStatus());
            atr.setFloodSeverity(wr.getFloodSeverity());
            atr.setForecaster(wr.getForecaster());
            atr.setGeometry(wr.getGeometry());
            atr.setImmediateCause(wr.getImmediateCause());
            atr.setIssueTime(calendarToDate(wr.getIssueTime()));
            atr.setLoc(wr.getLoc());
            atr.setLocationID(wr.getLocationID());
            atr.setMotdir(wr.getMotdir());
            atr.setMotspd(wr.getMotspd());
            atr.setOfficeid(wr.getOfficeid());
            atr.setOverviewText(wr.getOverviewText());
            atr.setPhen(wr.getPhen());
            atr.setPhensig(wr.getPhensig());
            atr.setPil(wr.getPil());
            atr.setProductClass(wr.getProductClass());
            atr.setPurgeTime(calendarToDate(wr.getPurgeTime()));
            atr.setRawmessage(wr.getRawmessage());
            atr.setRegion(wr.getRegion());
            atr.setSeg(wr.getSeg());
            atr.setSegText(wr.getSegText());
            atr.setSig(wr.getSig());
            atr.setStartTime(calendarToDate(wr.getStartTime()));
            atr.setUfn(wr.isUfn());
            atr.setVtecstr(wr.getVtecstr());
            atr.setWmoid(wr.getWmoid());
            atr.setXxxid(wr.getXxxid());

            for (String ugc : wr.getUgcZones()) {
                ActiveTableRecord ugcRecord = (ActiveTableRecord) atr.clone();
                ugcRecord.setUgcZone(ugc);
                list.add(ugcRecord);
            }
        }

        return list;
    }

    private static Date calendarToDate(Calendar calendar) {
        Date date = null;
        if (calendar != null) {
            date = calendar.getTime();
        }
        return date;
    }
}
