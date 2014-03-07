package gov.noaa.nws.ncep.common.dataplugin.nonconvsigmet;

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
 * NonConvsigmetRecord
 * 
 * This java class performs the mapping to the database table for NONCONVSIGMET
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket Author      Description
 * ------------ ------ ------  ----------- --------------------------
 * 06/2009      Uma Josyula Initial creation    
 * 09/2011      Chin Chen   changed to improve purge performance and
 *                          removed xml serialization as well
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013       1857 bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 bsteffen    Remove dataURI column from PluginDataObject.
 * Feb 11, 2014 2784   rferrel    Remove override of setIdentifier.
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "nonconvsigmetseq")
@Table(name = "nonconvsigmet", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "nonconvsigmet", indexes = { @Index(name = "nonconvsigmet_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@DynamicSerialize
public class NonConvSigmetRecord extends PluginDataObject {

    /**
	 * 
	 */
    private static final long serialVersionUID = 1L;

    // reportType is "non-convective sigmet".
    @Column(length = 32)
    @DataURI(position = 5)
    @DynamicSerializeElement
    private String reportType;

    // WMO header
    @Column(length = 32)
    @DynamicSerializeElement
    private String wmoHeader;

    // forecastRegion as: SL
    @Column(length = 8)
    @DataURI(position = 2)
    @DynamicSerializeElement
    private String forecastRegion;

    // The issue office where the report from
    @Column(length = 32)
    @DataURI(position = 1)
    @DynamicSerializeElement
    private String issueOffice;

    // Issue time of the report
    @Column
    @DataURI(position = 3)
    @DynamicSerializeElement
    private Calendar issueTime;

    // The designator
    @Column(length = 8)
    @DynamicSerializeElement
    private String designatorBBB;

    // CorrectionFlag is a flag with values (1 or 2 or 3)
    @Column(length = 8)
    @DynamicSerializeElement
    private String correctionRemarks;

    // The awipsId from the report
    @Column(length = 32)
    @DataURI(position = 4)
    @DynamicSerializeElement
    private String awipsId;

    // The state list from the report
    @Column(length = 256)
    @DynamicSerializeElement
    private String stateList;

    // Start time of the report
    @Column
    @DynamicSerializeElement
    private Calendar startTime;

    // End time of the report
    @Column
    @DynamicSerializeElement
    private Calendar endTime;

    // The type of the hazard from the report
    @Column(length = 16)
    @DynamicSerializeElement
    private String hazardType;

    // The intensity of the hazard from the report
    @Column(length = 64)
    @DynamicSerializeElement
    private String hazardIntensity;

    // The cause for the hazard from the report
    @Column(length = 128)
    @DynamicSerializeElement
    private String hazardCause;

    // The conditions stated about the hazard from the report
    @Column(length = 128)
    @DynamicSerializeElement
    private String hazardCondition;

    // The lower flight level from the report
    @Column
    @DynamicSerializeElement
    private int flightLevel1;

    // The upper flight level from the report
    @Column
    @DynamicSerializeElement
    private int flightLevel2;

    // The sigmet Identifier from the report
    @Column(length = 32)
    @DynamicSerializeElement
    private String sigmetId;

    // The entire report
    @Column(length = 3000)
    @DynamicSerializeElement
    private String bullMessage;

    /**
     * Convsigmet location
     */
    @DynamicSerializeElement
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    @JoinColumn(name = "parentID", nullable = false)
    @Index(name = "nonConvSigmetLocation_parentid_idex")
    private Set<NonConvSigmetLocation> nonConvSigmetLocation = new HashSet<NonConvSigmetLocation>();

    /**
     * Default Constructor
     */
    public NonConvSigmetRecord() {
        this.issueOffice = "";
        this.wmoHeader = "";
        this.bullMessage = "";
        this.designatorBBB = "";
        this.forecastRegion = "";
        this.reportType = "";
        this.correctionRemarks = "";
        this.awipsId = "";
        this.flightLevel1 = IDecoderConstantsN.INTEGER_MISSING;
        this.flightLevel2 = IDecoderConstantsN.INTEGER_MISSING;
        this.hazardCause = "";
        this.hazardCondition = "";
        this.hazardIntensity = "";
        this.hazardType = "UNKNOWN";
        this.stateList = "";
        this.sigmetId = "";
    }

    /**
     * Convstructs a non-consigmet record from a dataURI
     * 
     * @param uri
     *            The dataURI
     */
    public NonConvSigmetRecord(String uri) {
        super(uri);
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        // TODO Auto-generated method stub
        return null;
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
     * @return the wmoHeader
     */
    public String getWmoHeader() {
        return wmoHeader;
    }

    /**
     * @param wmoHeader
     *            to set
     */
    public void setWmoHeader(String wmoHeader) {
        this.wmoHeader = wmoHeader;
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
    public String getCorrectionRemarks() {
        return correctionRemarks;
    }

    /**
     * @param correctionFlag
     *            to set
     */
    public void setCorrectionRemarks(String correctionRemarks) {
        this.correctionRemarks = correctionRemarks;
    }

    /**
     * @return the awipsId
     */
    public String getAwipsId() {
        return awipsId;
    }

    /**
     * @param awipsId
     *            to set
     */
    public void setAwipsId(String awipsId) {
        this.awipsId = awipsId;
    }

    /**
     * @return the stateList
     */
    public String getStateList() {
        return stateList;
    }

    /**
     * @param stateList
     *            to set
     */
    public void setStateList(String stateList) {
        this.stateList = stateList;
    }

    /**
     * @return the startTime
     */
    public Calendar getStartTime() {
        return startTime;
    }

    /**
     * @param startTime
     *            to set
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
     *            to set
     */
    public void setEndTime(Calendar endTime) {
        this.endTime = endTime;
    }

    /**
     * @return the hazardType
     */
    public String getHazardType() {
        return hazardType;
    }

    /**
     * @param hazardType
     *            to set
     */
    public void setHazardType(String hazardType) {
        this.hazardType = hazardType;
    }

    /**
     * @return the hazardIntensity
     */
    public String getHazardIntensity() {
        return hazardIntensity;
    }

    /**
     * @param hazardIntensity
     *            to set
     */
    public void setHazardIntensity(String hazardIntensity) {
        this.hazardIntensity = hazardIntensity;
    }

    /**
     * @return the hazardCause
     */
    public String getHazardCause() {
        return hazardCause;
    }

    /**
     * @param hazardCause
     *            to set
     */
    public void setHazardCause(String hazardCause) {
        this.hazardCause = hazardCause;
    }

    /**
     * @return the hazardCondition
     */
    public String getHazardCondition() {
        return hazardCondition;
    }

    /**
     * @param hazardCondition
     *            to set
     */
    public void setHazardCondition(String hazardCondition) {
        this.hazardCondition = hazardCondition;
    }

    /**
     * @return the flightLevel1
     */
    public int getFlightLevel1() {
        return flightLevel1;
    }

    /**
     * @param flightLevel1
     *            to set
     */
    public void setFlightLevel1(int flightLevel1) {
        this.flightLevel1 = flightLevel1;
    }

    /**
     * @return the flightLevel2
     */
    public int getFlightLevel2() {
        return flightLevel2;
    }

    /**
     * @param flightLevel2
     *            to set
     */
    public void setFlightLevel2(int flightLevel2) {
        this.flightLevel2 = flightLevel2;
    }

    /**
     * @return the sigmetId
     */
    public String getSigmetId() {
        return sigmetId;
    }

    /**
     * @param sigmetId
     *            to set
     */
    public void setSigmetId(String sigmetId) {
        this.sigmetId = sigmetId;
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
     * @return the set of nonconvective sigmet location
     */
    public Set<NonConvSigmetLocation> getNonConvSigmetLocation() {
        return nonConvSigmetLocation;
    }

    /**
     * @param nonconvsigmet
     *            the location to set
     */
    public void setNonConvSigmetLocation(
            Set<NonConvSigmetLocation> nonConvLocation) {
        this.nonConvSigmetLocation = nonConvLocation;
    }

    /**
     * @param add
     *            conv Sigmet location to set
     */
    public void addNonConvSigmetLocation(NonConvSigmetLocation pLocation) {
        nonConvSigmetLocation.add(pLocation);

    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    @Override
    public String getPluginName() {
        return "nonconvsigmet";
    }
}