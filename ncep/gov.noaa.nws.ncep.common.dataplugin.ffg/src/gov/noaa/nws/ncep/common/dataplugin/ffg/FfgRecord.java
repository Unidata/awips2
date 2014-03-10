package gov.noaa.nws.ncep.common.dataplugin.ffg;

import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * FfgRecord
 * 
 * This java class performs the mapping to the database tables for FFG.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/2008      14         T. Lee      Initial coding
 * 12/2008      14         T. Lee      Initialized variable
 * 03/2009      14         T. Lee      Migration to TO10
 * 07/2009      14         T. Lee      Migration to TO11
 * 09/2011                 Chin Chen   changed to improve purge performance
 *                                     and  removed xml serialization as
 *                                     well
 * Apr 04, 2013 1846       bkowal      Added an index on refTime and
 *                                     forecastTime
 * Apr 12, 2013 1857       bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Feb 11, 2014 2784       rferrel     Remove override of setIdentifier.
 * 
 * </pre>
 * 
 * @author T.Lee
 * @version 1.0
 */

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "ffgseq")
@Table(name = "ffg", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "ffg", indexes = { @Index(name = "ffg_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@DynamicSerialize
public class FfgRecord extends PluginDataObject {
    private static final long serialVersionUID = 1L;

    /** Report type */
    @Column(length = 32)
    @DynamicSerializeElement
    @DataURI(position = 2)
    private String reportType;

    /** FFG AWIPS identifier */
    @Column(length = 32)
    @DataURI(position = 1)
    @DynamicSerializeElement
    private String awipsID;

    /** Bulletin insurance time */
    @Column
    @DynamicSerializeElement
    private Calendar issueTime;

    /** Station ID */
    @Column(length = 32)
    @DynamicSerializeElement
    private String issueOffice;

    /** Designator BBB */
    @Column(length = 8)
    @DynamicSerializeElement
    private String designatorBBB;

    /** Bulletin messages */
    @Column(length = 10000)
    @DynamicSerializeElement
    private String bullMessage;

    /** Mass News Disseminator (MND) */
    @Column(length = 72)
    @DynamicSerializeElement
    private String mndTime;

    /** WMO header */
    @Column(length = 32)
    @DynamicSerializeElement
    private String wmoHeader;

    /** FFG precipitation */
    @DynamicSerializeElement
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    @JoinColumn(name = "parentID", nullable = false)
    @Index(name = "ffgP_parentid_idex")
    private Set<FfgPrecip> ffgP = new HashSet<FfgPrecip>();

    /**
     * Default Constructor
     */
    public FfgRecord() {
        awipsID = "";
        issueTime = null;
        issueOffice = "";
        wmoHeader = "";
        mndTime = "";
        bullMessage = "";
    }

    /**
     * Constructs a FFG record from a dataURI
     * 
     * @param uri
     *            : The dataURI
     */
    public FfgRecord(String uri) {
        super(uri);
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * Return the reportType
     */
    public String getReportType() {
        return reportType;
    }

    /**
     * @param reportType
     *            the report type to set
     */
    public void setReportType(String reportType) {
        this.reportType = reportType;
    }

    /**
     * @return the awipsID
     */
    public String getAwipsID() {
        return awipsID;
    }

    /**
     * @param awipsID
     *            the AWIPS identifier to set
     */
    public void setAwipsID(String awipsID) {
        this.awipsID = awipsID;
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
     * @return the issueOffice
     */
    public String getIssueOffice() {
        return issueOffice;
    }

    /**
     * @param issueOffice
     *            the issueOffice to set
     */
    public void setIssueOffice(String issueOffice) {
        this.issueOffice = issueOffice;
    }

    /**
     * @return the wmoHeader
     */
    public String getWmoHeader() {
        return wmoHeader;
    }

    /**
     * @param wmoHeader
     *            the wmoHeader to set
     */
    public void setWmoHeader(String wmoHeader) {
        this.wmoHeader = wmoHeader;
    }

    /**
     * @return designatorBBB
     */
    public String getDesignatorBBB() {
        return designatorBBB;
    }

    /**
     * @param designatorBBB
     *            the designatorBBB to set
     */
    public void setDesignatorBBB(String designatorBBB) {
        this.designatorBBB = designatorBBB;
    }

    /**
     * @return the bullMessage
     */
    public String getBullMessage() {
        return bullMessage;
    }

    /**
     * @param bullMessage
     *            the bullMessage to set
     */
    public void setBullMessage(String bullMessage) {
        this.bullMessage = bullMessage;
    }

    /**
     * @return the MndTime
     */
    public String getMndTime() {
        return mndTime;
    }

    /**
     * @param mndTime
     *            the mndTime to set
     */
    public void setMndTime(String mndTime) {
        this.mndTime = mndTime;
    }

    /**
     * @return the set of precipitation (ffgP)
     */
    public Set<FfgPrecip> getFfgP() {
        return ffgP;
    }

    /**
     * @param ffgP
     *            the set of precipitation to set
     */
    public void setFfgP(Set<FfgPrecip> ffgP) {
        this.ffgP = ffgP;
    }

    /**
     * Add FfgPrecip to set
     */
    public void addPrecip(FfgPrecip precip) {
        ffgP.add(precip);
        // precip.setParentID (this);
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    @Override
    public String getPluginName() {
        return "ffg";
    }
}
