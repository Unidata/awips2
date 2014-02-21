package gov.noaa.nws.ncep.common.dataplugin.airmet;

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
 * AirmetRecord
 * 
 * This java class performs the mapping to the database table for AIRMET
 * 
 * <pre>
 * HISTORY
 * 
 * Date         Author      Description
 * ------------ ----------  ----------- --------------------------
 * 05/2009      L. Lin      Initial creation    
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 05/2009      39              L. Lin      Initial coding
 * 07/2009      39              L. Lin      Migration to TO11
 * 09/2011                      Chin Chen   changed to improve purge performance and
 *                                          removed xml serialization as well
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013       1857 bgonzale         Added SequenceGenerator annotation.
 * May 07, 2013 1869            bsteffen    Remove dataURI column from
 *                                          PluginDataObject.
 * Feb 11, 2014 2784            rferrel     Remove override of setIdentifier.
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "airmetseq")
@Table(name = "airmet", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "airmet", indexes = { @Index(name = "airmet_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@DynamicSerialize
public class AirmetRecord extends PluginDataObject {

    /**
	 * 
	 */
    private static final long serialVersionUID = 1L;

    // reportType is AIRMET.
    @Column(length = 32)
    @DataURI(position = 4)
    @DynamicSerializeElement
    private String reportType;

    // reportName will be SIERRA, TANGO, or ZULU
    @Column(length = 32)
    @DataURI(position = 1)
    @DynamicSerializeElement
    private String reportName;

    // WMO header
    @Column(length = 32)
    @DataURI(position = 2)
    @DynamicSerializeElement
    private String wmoHeader;

    // The issue office where the report from
    @Column(length = 32)
    @DynamicSerializeElement
    private String issueOffice;

    // Update number as: 1, 2, 3, ...
    @Column
    @DataURI(position = 3)
    @DynamicSerializeElement
    private Integer updateNumber;

    // Issue time of the report
    @Column
    @DynamicSerializeElement
    private Calendar issueTime;

    // The designator
    @Column(length = 8)
    @DynamicSerializeElement
    private String designatorBBB;

    // CorrectionFlag is a flag: 0 for normal, 1 for COR or CC, 2 for AMD, and 3
    // for TEST
    /*
     * CorrectionFlag is a flag: 0 for normal, 1 for COR or CC, 2 for AMD, 3 for
     * TEST, and 4 for NIL report
     */
    @Column
    @DynamicSerializeElement
    private Integer correctionFlag;

    // The entire report
    @Column(length = 15000)
    @DynamicSerializeElement
    private String bullMessage;

    /**
     * Airmet report
     */
    @DynamicSerializeElement
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    @JoinColumn(name = "parentID", nullable = false)
    @Index(name = "airmetReport_parentid_idex")
    private Set<AirmetReport> airmetReport = new HashSet<AirmetReport>();

    /**
     * Default Convstructor
     */
    public AirmetRecord() {
        this.issueOffice = "";
        this.wmoHeader = "";
        this.bullMessage = "";
        this.designatorBBB = "";
        this.updateNumber = 0;
        this.reportType = "";
        this.reportName = null;
        this.correctionFlag = 0;
    }

    /**
     * Convstructs an airmet record from a dataURI
     * 
     * @param uri
     *            The dataURI
     */
    public AirmetRecord(String uri) {
        super(uri);
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * @return the issueOffice
     */
    public String getIssueOffice() {
        return issueOffice;
    }

    /**
     * @param issueOffice
     *            to set
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
     * @param wnoHeader
     *            to set
     */
    public void setWmoHeader(String wmoHeader) {
        this.wmoHeader = wmoHeader;
    }

    /**
     * @return the issueTime
     */
    public Calendar getIssueTime() {
        return issueTime;
    }

    /**
     * @param issueTime
     *            to set
     */
    public void setIssueTime(Calendar issueTime) {
        this.issueTime = issueTime;
    }

    /**
     * @return the reportType
     */
    public String getReportType() {
        return reportType;
    }

    /**
     * @param reportType
     *            to set
     */
    public void setReportType(String reportType) {
        this.reportType = reportType;
    }

    /**
     * @return the designatorBBB
     */
    public String getDesignatorBBB() {
        return designatorBBB;
    }

    /**
     * @param designatorBBB
     *            to set
     */
    public void setDesignatorBBB(String designatorBBB) {
        this.designatorBBB = designatorBBB;
    }

    /**
     * @return the correctionFlag
     */
    public Integer getCorrectionFlag() {
        return correctionFlag;
    }

    /**
     * @param correctionFlag
     *            to set
     */
    public void setCorrectionFlag(Integer correctionFlag) {
        this.correctionFlag = correctionFlag;
    }

    /**
     * @return the updateNumber
     */
    public Integer getUpdateNumber() {
        return updateNumber;
    }

    /**
     * @param updateNumber
     *            to set
     */
    public void setUpdateNumber(Integer updateNumber) {
        this.updateNumber = updateNumber;
    }

    /**
     * @return the bullMessage
     */
    public String getBullMessage() {
        return bullMessage;
    }

    /**
     * @param bullMessage
     *            to set
     */
    public void setBullMessage(String bullMessage) {
        this.bullMessage = bullMessage;
    }

    /**
     * @return the reportName
     */
    public String getReportName() {
        return reportName;
    }

    /**
     * @param reportName
     *            to set
     */
    public void setReportName(String reportName) {
        this.reportName = reportName;
    }

    /**
     * @return the set of AIRMET report
     */
    public Set<AirmetReport> getAirmetReport() {
        return airmetReport;
    }

    /**
     * @param airmet
     *            the report to set
     */
    public void setAirmetReport(Set<AirmetReport> curReport) {
        this.airmetReport = curReport;
    }

    /**
     * @param add
     *            AirmetReport to set
     */
    public void addAirmetReport(AirmetReport curReport) {
        airmetReport.add(curReport);
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    @Override
    public String getPluginName() {
        return "airmet";
    }
}
