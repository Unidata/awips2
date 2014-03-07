package gov.noaa.nws.ncep.common.dataplugin.wcp;

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
 * WcpRecord is the Data Access component for WCP Watch Corner Point data. This
 * contains getters and setters for the main parent table wcp. This code has
 * been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12Dec2008    37         F. J. Yen   Initial Coding.
 * 17Apr2009    37         F. J. Yen   Refactored for TO10
 * 24Aug2009    37         F. J. Yen   Refactored for TO11
 * 17May2010    37         F. J. Yen   Refactored to dataplugin for migration to
 *                                     to11dr11
 * 09/2011                 Chin Chen   changed to improve purge performance and
 *                                     removed xml serialization as well
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
 * @author F. J. Yen, SIB
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "wcpseq")
@Table(name = "wcp", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "wcp", indexes = { @Index(name = "wcp_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@DynamicSerialize
public class WcpRecord extends PluginDataObject {

    private static final long serialVersionUID = 1L;

    /** Report type */
    @Column(length = 32)
    @DynamicSerializeElement
    @DataURI(position = 2)
    private String reportType;

    @Column
    @DynamicSerializeElement
    private Calendar issueTime;

    @DataURI(position = 1)
    @Column(length = 8)
    @DynamicSerializeElement
    private String designatorBBB;

    @Column(length = 2500)
    @DynamicSerializeElement
    private String bullMessage;

    /** WcpSevrln */
    @DynamicSerializeElement
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    @JoinColumn(name = "parentID", nullable = false)
    @Index(name = "wcpSevrLn_parentid_idex")
    private Set<WcpSevrln> wcpSevrLn = new HashSet<WcpSevrln>();

    /**
     * Default Constructor
     */
    public WcpRecord() {
    }

    /**
     * Constructs a wcp record from a dataURI
     * 
     * @param uri
     *            The dataURI
     */
    public WcpRecord(String uri) {
        super(uri);
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        // TODO Auto-generated method stub
        return null;
    }

    public String getReportType() {
        return reportType;
    }

    public void setReportType(String reportType) {
        this.reportType = reportType;
    }

    public Calendar getIssueTime() {
        return issueTime;
    }

    public void setIssueTime(Calendar issueTime) {
        this.issueTime = issueTime;
    }

    public String getDesignatorBBB() {
        return designatorBBB;
    }

    public void setDesignatorBBB(String designatorBBB) {
        this.designatorBBB = designatorBBB;
    }

    public String getBullMessage() {
        return bullMessage;
    }

    public void setBullMessage(String bullMessage) {
        this.bullMessage = bullMessage;
    }

    /**
     * return the set of wcpSevrLn
     */
    public Set<WcpSevrln> getWcpSevrLn() {
        return wcpSevrLn;
    }

    /**
     * @param wcpSevrln
     *            the wcpSevrln to set
     */
    public void setWcpSevrLn(Set<WcpSevrln> wcpSevrlin) {
        this.wcpSevrLn = wcpSevrlin;
    }

    /*
     * Add wcpSevrln to set
     */
    public void addWcpSevrLn(WcpSevrln psevrln) {
        wcpSevrLn.add(psevrln);

    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    @Override
    public String getPluginName() {
        return "wcp";
    }
}
