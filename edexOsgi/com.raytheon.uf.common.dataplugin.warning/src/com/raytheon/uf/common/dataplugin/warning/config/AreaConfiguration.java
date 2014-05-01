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
package com.raytheon.uf.common.dataplugin.warning.config;

import java.util.HashMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestableMetadataMarshaller;

/**
 * AreaConfiguration
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Nov 26, 2007             chammack    Initial Creation.
 *    Aug 26, 2008 #1502       bclement    Added JAXB annotations
 *    Mar 29, 2012 #14691      Qinglu Lin  Added feAreaField and its getter and setter.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AreaConfiguration {

    @XmlAttribute
    private String variable;

    @XmlElement
    private String pointField;

    @XmlJavaTypeAdapter(value = RequestableMetadataMarshaller.class)
    private HashMap<String, RequestConstraint> pointFilter;

    @XmlElement
    private String areaNotationField;

    @XmlElement
    private String areaSource;

    @XmlElement
    private String areaField;

    @XmlElement
    private String feAreaField;
    
    @XmlElement
    private String timeZoneField;

    @XmlElement
    private String fipsField;

    @XmlElement
    private String parentAreaField;

    @XmlElement
    private String areaNotationTranslationFile;

    @XmlElementWrapper(name = "sortBy")
    @XmlElement(name = "sort")
    private String[] sortBy;

    @XmlElement
    private double inclusionPercent = 0.00;

    @XmlElement
    private String inclusionAndOr = "AND";

    @XmlElement
    private double inclusionArea = 0.00;

    @XmlElement
    private double includedWatchAreaBuffer;

    /**
     * @return the parentAreaField
     */
    public String getParentAreaField() {
        return parentAreaField;
    }

    /**
     * @param parentAreaField
     *            the parentAreaField to set
     */
    public void setParentAreaField(String parentAreaField) {
        this.parentAreaField = parentAreaField;
    }

    /**
     * @return the areaNotationField
     */
    public String getAreaNotationField() {
        return areaNotationField;
    }

    /**
     * @param areaNotationField
     *            the areaNotationField to set
     */
    public void setAreaNotationField(String areaNotationField) {
        this.areaNotationField = areaNotationField;
    }

    /**
     * @return the areaField
     */
    public String getAreaField() {
        return areaField;
    }

    /**
     * @return the feAreaField
     */
    public String getFeAreaField() {
        return feAreaField;
    }

    /**
     * @param areaField
     *            the areaField to set
     */
    public void setAreaField(String areaField) {
        this.areaField = areaField;
    }

    /**
     * @param feAreaField
     *            the feAreaField to set
     */
    public void setFeAreaField(String feAreaField) {
        this.feAreaField = feAreaField;
    }

    /**
     * @return the areaNotationTranslationFile
     */
    public String getAreaNotationTranslationFile() {
        return areaNotationTranslationFile;
    }

    /**
     * @param areaNotationTranslationFile
     *            the areaNotationTranslationFile to set
     */
    public void setAreaNotationTranslationFile(
            String areaNotationTranslationFile) {
        this.areaNotationTranslationFile = areaNotationTranslationFile;
    }

    /**
     * @return the pointField
     */
    public String getPointField() {
        return pointField;
    }

    /**
     * @param pointField
     *            the pointField to set
     */
    public void setPointField(String pointField) {
        this.pointField = pointField;
    }

    /**
     * @return the pointFilter
     */
    public HashMap<String, RequestConstraint> getPointFilter() {
        return pointFilter;
    }

    /**
     * @param pointFilter
     *            the pointFilter to set
     */
    public void setPointFilter(HashMap<String, RequestConstraint> pointFilter) {
        this.pointFilter = pointFilter;
    }

    public double getInclusionPercent() {
        return inclusionPercent;
    }

    public void setInclusionPercent(double inclusionPercent) {
        this.inclusionPercent = inclusionPercent;
    }

    public double getInclusionArea() {
        return inclusionArea;
    }

    public void setInclusionArea(double inclusionArea) {
        this.inclusionArea = inclusionArea;
    }

    public String getFipsField() {
        return fipsField;
    }

    public void setFipsField(String fipsField) {
        this.fipsField = fipsField;
    }

    public String getInclusionAndOr() {
        return inclusionAndOr;
    }

    public void setInclusionAndOr(String inclusionAndOr) {
        this.inclusionAndOr = inclusionAndOr;
    }

    public double getIncludedWatchAreaBuffer() {
        return includedWatchAreaBuffer;
    }

    public void setIncludedWatchAreaBuffer(double includedWatchAreaBuffer) {
        this.includedWatchAreaBuffer = includedWatchAreaBuffer;
    }

    public String[] getSortBy() {
        return sortBy;
    }

    public void setSortBy(String[] sortBy) {
        this.sortBy = sortBy;
    }

    public String getVariable() {
        return variable;
    }

    public void setVariable(String variable) {
        this.variable = variable;
    }

    public String getAreaSource() {
        return areaSource;
    }

    public void setAreaSource(String areaSource) {
        this.areaSource = areaSource;
    }

	public String getTimeZoneField() {
		return timeZoneField;
	}

	public void setTimeZoneField(String timeZoneField) {
		this.timeZoneField = timeZoneField;
	}

}
