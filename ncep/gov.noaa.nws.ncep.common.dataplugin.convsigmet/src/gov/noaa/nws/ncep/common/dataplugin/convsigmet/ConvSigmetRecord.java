package gov.noaa.nws.ncep.common.dataplugin.convsigmet;

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

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
 * ConvsigmetRecord
 * 
 * This java class performs the mapping to the database table for CONVSIGMET
 * 
 * SOFTWARE HISTORY
 * 
 * <pre>
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 03/2009      87/114          L. Lin      Initial coding
 * 07/2009      87/114          L. Lin      Migration to TO11
 * 09/2011                      Chin Chen   changed to improve purge performance and
 *                                          removed xml serialization as well
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013 1857            bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869            bsteffen    Remove dataURI column from
 *                                          PluginDataObject.
 * Feb 11, 2014 2784            rferrel     Remove override of setIdentifier.
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "convsigmetseq")
@Table(name = "convsigmet", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "convsigmet", indexes = { @Index(name = "convsigmet_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@DynamicSerialize
public class ConvSigmetRecord extends PluginDataObject {

    /**
	 * 
	 */
    private static final long serialVersionUID = 1L;

    // reportType is "convective sigmet".
    @Column(length = 32)
    @DataURI(position = 1)
    @DynamicSerializeElement
    private String reportType;

    // WMO header
    @Column(length = 32)
    @DataURI(position = 2)
    @DynamicSerializeElement
    private String wmoHeader;

    // forecastRegion as: SIGW, SIGC, or SIGE
    @Column(length = 8)
    @DataURI(position = 3)
    @DynamicSerializeElement
    private String forecastRegion;

    // The issue office where the report from
    @Column(length = 32)
    @DynamicSerializeElement
    private String issueOffice;

    // Issue time of the report
    @Column
    @DynamicSerializeElement
    private Calendar issueTime;

    // The designator
    @Column(length = 8)
    @DynamicSerializeElement
    private String designatorBBB;

    // CorrectionFlag is a flag indicating a cancellation (0 or 1)
    @Column
    @DynamicSerializeElement
    private Integer correctionFlag;

    // The entire report
    @Column(length = 15000)
    @DynamicSerializeElement
    private String bullMessage;

    /**
     * Convsigmet section
     */
    @DynamicSerializeElement
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    @JoinColumn(name = "parentID", nullable = false)
    @Index(name = "convSigmetSection_parentid_idex")
    private Set<ConvSigmetSection> convSigmetSection = new HashSet<ConvSigmetSection>();

    /**
     * Default Convstructor
     */
    public ConvSigmetRecord() {
        this.issueOffice = "";
        this.wmoHeader = "";
        this.bullMessage = "";
        this.designatorBBB = "";
        this.forecastRegion = "";
        this.reportType = "";
        this.correctionFlag = IDecoderConstantsN.INTEGER_MISSING;
    }

    /**
     * Convstructs a consigmet record from a dataURI
     * 
     * @param uri
     *            The dataURI
     */
    public ConvSigmetRecord(String uri) {
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
     * @return the forecastRegion
     */
    public String getForecastRegion() {
        return forecastRegion;
    }

    /**
     * @param forecastRegion
     *            to set
     */
    public void setForecastRegion(String forecastRegion) {
        this.forecastRegion = forecastRegion;
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
     * @return the set of convective Sigmet section
     */
    public Set<ConvSigmetSection> getConvSigmetSection() {
        return convSigmetSection;
    }

    /**
     * @param convsigmet
     *            the section to set
     */
    public void setConvSigmetSection(Set<ConvSigmetSection> convSection) {
        this.convSigmetSection = convSection;
    }

    /**
     * @param add
     *            convective Sigmet Section to set
     */
    public void addConvSigmetSection(ConvSigmetSection psection) {
        convSigmetSection.add(psection);

    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    @Override
    public String getPluginName() {
        return "convsigmet";
    }
}
