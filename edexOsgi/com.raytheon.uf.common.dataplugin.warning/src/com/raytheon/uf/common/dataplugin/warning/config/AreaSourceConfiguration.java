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
 * AreaSourceConfiguration
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Mar 29, 2012 #14691      Qinglu Lin  Added feAreaField and its getter and setter, etc.
 *    Apr 24, 2014  1943       jsanchez    Removed unused areaType.
 * 
 * </pre>
 * 
 * @author
 * @version 1
 */

@XmlAccessorType(XmlAccessType.NONE)
public class AreaSourceConfiguration {
    @XmlAccessorType(XmlAccessType.NONE)
    public static enum AreaType {
        HATCHING, INTERSECT;
    }

    @XmlElement
    private AreaType type = AreaType.HATCHING;

    @XmlAttribute
    private String variable;

    @XmlElement
    private String areaSource;

    @XmlElement
    private String areaField;

    @XmlElement
    private String feAreaField;

    @XmlElement
    private String fipsField;

    @XmlElement
    private String areaNotationField;

    @XmlElement
    private String areaNotationTranslationFile;

    @XmlElement
    private String timeZoneField;

    @XmlElementWrapper(name = "sortBy")
    @XmlElement(name = "sort")
    private String[] sortBy;

    @XmlElement
    private String pointField;

    @XmlJavaTypeAdapter(value = RequestableMetadataMarshaller.class)
    private HashMap<String, RequestConstraint> pointFilter;

    @XmlElement
    private String parentAreaField;

    @XmlElement
    private double inclusionPercent = 0.00;

    @XmlElement
    private String inclusionAndOr = "AND";

    @XmlElement
    private double inclusionArea = 0.00;

    @XmlElement
    private double includedWatchAreaBuffer;

    public AreaSourceConfiguration() {

    }

    public AreaSourceConfiguration(AreaConfiguration areaConfig) {
        setVariable("areas");
        setAreaField(areaConfig.getAreaField());
        setAreaNotationField(areaConfig.getAreaNotationField());
        setFeAreaField(areaConfig.getFeAreaField());
        setTimeZoneField(areaConfig.getTimeZoneField());
        setAreaNotationTranslationFile(areaConfig
                .getAreaNotationTranslationFile());
        setFipsField(areaConfig.getFipsField());
        setIncludedWatchAreaBuffer(areaConfig.getIncludedWatchAreaBuffer());
        setInclusionAndOr(areaConfig.getInclusionAndOr());
        setInclusionArea(areaConfig.getInclusionArea());
        setInclusionPercent(areaConfig.getInclusionPercent());
        setParentAreaField(areaConfig.getParentAreaField());
        setPointField(areaConfig.getPointField());
        setPointFilter(areaConfig.getPointFilter());
        setSortBy(areaConfig.getSortBy());
    }

    public AreaConfiguration getAreaConfig() {
        AreaConfiguration areaConfig = new AreaConfiguration();
        areaConfig.setAreaField(areaField);
        areaConfig.setFeAreaField(feAreaField);
        areaConfig.setAreaNotationField(areaNotationField);
        areaConfig.setAreaNotationTranslationFile(areaNotationTranslationFile);
        areaConfig.setFipsField(fipsField);
        areaConfig.setIncludedWatchAreaBuffer(includedWatchAreaBuffer);
        areaConfig.setInclusionAndOr(inclusionAndOr);
        areaConfig.setInclusionArea(inclusionArea);
        areaConfig.setInclusionPercent(inclusionPercent);
        areaConfig.setParentAreaField(parentAreaField);
        areaConfig.setPointField(pointField);
        areaConfig.setPointFilter(pointFilter);
        areaConfig.setSortBy(sortBy);
        areaConfig.setVariable(variable);
        areaConfig.setAreaSource(areaSource);
        areaConfig.setTimeZoneField(timeZoneField);

        return areaConfig;
    }

    public String getAreaSource() {
        return areaSource;
    }

    public void setAreaSource(String areaSource) {
        this.areaSource = areaSource;
    }

    public String getAreaField() {
        return areaField;
    }

    public String getFeAreaField() {
        return feAreaField;
    }

    public void setAreaField(String areaField) {
        this.areaField = areaField;
    }

    public void setFeAreaField(String feAreaField) {
        this.feAreaField = feAreaField;
    }

    public String getFipsField() {
        return fipsField;
    }

    public void setFipsField(String fipsField) {
        this.fipsField = fipsField;
    }

    public String getAreaNotationField() {
        return areaNotationField;
    }

    public void setAreaNotationField(String areaNotationField) {
        this.areaNotationField = areaNotationField;
    }

    public String getAreaNotationTranslationFile() {
        return areaNotationTranslationFile;
    }

    public void setAreaNotationTranslationFile(
            String areaNotationTranslationFile) {
        this.areaNotationTranslationFile = areaNotationTranslationFile;
    }

    public String[] getSortBy() {
        return sortBy;
    }

    public void setSortBy(String[] sortBy) {
        this.sortBy = sortBy;
    }

    public String getPointField() {
        return pointField;
    }

    public void setPointField(String pointField) {
        this.pointField = pointField;
    }

    public HashMap<String, RequestConstraint> getPointFilter() {
        return pointFilter;
    }

    public void setPointFilter(HashMap<String, RequestConstraint> pointFilter) {
        this.pointFilter = pointFilter;
    }

    public String getParentAreaField() {
        return parentAreaField;
    }

    public void setParentAreaField(String parentAreaField) {
        this.parentAreaField = parentAreaField;
    }

    public double getInclusionPercent() {
        return inclusionPercent;
    }

    public void setInclusionPercent(double inclusionPercent) {
        this.inclusionPercent = inclusionPercent;
    }

    public String getInclusionAndOr() {
        return inclusionAndOr;
    }

    public void setInclusionAndOr(String inclusionAndOr) {
        this.inclusionAndOr = inclusionAndOr;
    }

    public double getInclusionArea() {
        return inclusionArea;
    }

    public void setInclusionArea(double inclusionArea) {
        this.inclusionArea = inclusionArea;
    }

    public double getIncludedWatchAreaBuffer() {
        return includedWatchAreaBuffer;
    }

    public void setIncludedWatchAreaBuffer(double includedWatchAreaBuffer) {
        this.includedWatchAreaBuffer = includedWatchAreaBuffer;
    }

    public String getVariable() {
        return variable;
    }

    public void setVariable(String variable) {
        this.variable = variable;
    }

    public String getTimeZoneField() {
        return timeZoneField;
    }

    public void setTimeZoneField(String timeZoneField) {
        this.timeZoneField = timeZoneField;
    }

    public AreaType getType() {
        return type;
    }

    public void setType(AreaType type) {
        this.type = type;
    }

}
