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
import java.util.List;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
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
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */
@MappedSuperclass
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
@DynamicSerialize
public abstract class ActiveTableRecord extends PersistableDataObject {

    protected static final long serialVersionUID = 1L;

    protected static final String ID_GEN = "idgen";

    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = ID_GEN)
    @Id
    protected int id;

    @Column(length = 32)
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

    @Column(length = 8)
    @DynamicSerializeElement
    protected String ugcZone;

    @Column(columnDefinition = "text")
    @DynamicSerializeElement
    protected String vtecstr;

    @Column(length = 4)
    @DynamicSerializeElement
    protected String productClass;

    @DataURI(position = 4)
    @Column(length = 4)
    @DynamicSerializeElement
    protected String act;

    @Column(length = 8)
    @DynamicSerializeElement
    protected String officeid;

    @Column(length = 4)
    @DynamicSerializeElement
    protected String phen;

    @Column(length = 4)
    @DynamicSerializeElement
    protected String sig;

    @DataURI(position = 5)
    @Column(length = 4)
    @DynamicSerializeElement
    protected String etn;

    /** vtec start time */
    @DynamicSerializeElement
    protected Calendar startTime;

    @Column
    @DynamicSerializeElement
    protected Calendar endTime;

    @Column
    @DynamicSerializeElement
    protected Calendar issueTime;

    @Column
    @DynamicSerializeElement
    protected Calendar purgeTime;

    @Column(length = 8)
    @DynamicSerializeElement
    protected boolean ufn;

    @Column(name = "geometry", columnDefinition = "geometry")
    @Type(type = "com.raytheon.edex.db.objects.hibernate.GeometryType")
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

    @Column(length = 8)
    @DynamicSerializeElement
    protected String locationID;

    @Column(length = 2)
    @DynamicSerializeElement
    protected String floodSeverity;

    @DynamicSerializeElement
    protected String immediateCause;

    @Column(length = 2)
    @DynamicSerializeElement
    protected String floodRecordStatus;

    @Column
    @DynamicSerializeElement
    protected Calendar floodBegin;

    @Column
    @DynamicSerializeElement
    protected Calendar floodCrest;

    @Column
    @DynamicSerializeElement
    protected Calendar floodEnd;

    @Override
    public abstract Object clone();

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
        if (etn == null) {
            if (other.etn != null) {
                return false;
            }
        } else if (!etn.equals(other.etn)) {
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
        } else if (!geometry.equalsExact(other.geometry)) {
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
        if (officeid == null) {
            if (other.officeid != null) {
                return false;
            }
        } else if (!officeid.equals(other.officeid)) {
            return false;
        }
        if (overviewText == null) {
            if (other.overviewText != null) {
                return false;
            }
        } else if (!overviewText.equals(other.overviewText)) {
            return false;
        }
        if (phen == null) {
            if (other.phen != null) {
                return false;
            }
        } else if (!phen.equals(other.phen)) {
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
        if (sig == null) {
            if (other.sig != null) {
                return false;
            }
        } else if (!sig.equals(other.sig)) {
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
        if (ugcZone == null) {
            if (other.ugcZone != null) {
                return false;
            }
        } else if (!ugcZone.equals(other.ugcZone)) {
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
        return ugcZone;
    }

    /**
     * @param ugcZone
     *            the ugcZone to set
     */
    public void setUgcZone(String ugcZone) {
        this.ugcZone = ugcZone;
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
        return officeid;
    }

    /**
     * @param officeid
     *            the officeid to set
     */
    public void setOfficeid(String officeid) {
        this.officeid = officeid;
    }

    /**
     * @return the phen
     */
    public String getPhen() {
        return phen;
    }

    /**
     * @param phen
     *            the phen to set
     */
    public void setPhen(String phen) {
        this.phen = phen;
    }

    /**
     * @return the sig
     */
    public String getSig() {
        return sig;
    }

    /**
     * @param sig
     *            the sig to set
     */
    public void setSig(String sig) {
        this.sig = sig;
    }

    /**
     * @return the etn
     */
    public String getEtn() {
        return etn;
    }

    /**
     * @param etn
     *            the etn to set
     */
    public void setEtn(String etn) {
        this.etn = etn;
    }

    /**
     * @return the startTime
     */
    public Calendar getStartTime() {
        return startTime;
    }

    /**
     * @param startTime
     *            the startTime to set
     */
    public void setStartTime(Calendar startTime) {
        this.startTime = startTime;
    }

    /**
     * @return the endTime
     */
    public Calendar getEndTime() {
        return endTime;
    }

    /**
     * @param endTime
     *            the endTime to set
     */
    public void setEndTime(Calendar endTime) {
        this.endTime = endTime;
    }

    /**
     * @return the issueTime
     */
    public Calendar getIssueTime() {
        return issueTime;
    }

    /**
     * @param issueTime
     *            the issueTime to set
     */
    public void setIssueTime(Calendar issueTime) {
        this.issueTime = issueTime;
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
            atr.setEndTime(wr.getEndTime());
            atr.setEtn(wr.getEtn());
            atr.setFloodBegin(wr.getFloodBegin());
            atr.setFloodCrest(wr.getFloodCrest());
            atr.setFloodEnd(wr.getFloodEnd());
            atr.setFloodRecordStatus(wr.getFloodRecordStatus());
            atr.setFloodSeverity(wr.getFloodSeverity());
            atr.setForecaster(wr.getForecaster());
            atr.setGeometry(wr.getGeometry());
            atr.setImmediateCause(wr.getImmediateCause());
            atr.setIssueTime(wr.getIssueTime());
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
            atr.setPurgeTime(wr.getPurgeTime());
            atr.setRawmessage(wr.getRawmessage());
            atr.setRegion(wr.getRegion());
            atr.setSeg(wr.getSeg());
            atr.setSegText(wr.getSegText());
            atr.setSig(wr.getSig());
            atr.setStartTime(wr.getStartTime());
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

}
