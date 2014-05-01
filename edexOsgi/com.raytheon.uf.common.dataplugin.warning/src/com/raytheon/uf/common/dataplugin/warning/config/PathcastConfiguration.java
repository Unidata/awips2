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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;

import org.apache.commons.lang.Validate;

/**
 * Pathcast
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Nov 21, 2007             chammack    Initial Creation.
 *    Aug 26, 2008 #1502       bclement    Added JAXB annotations
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
public class PathcastConfiguration extends PointSourceConfiguration {

    @XmlElement
    private int maxGroup = 8;

    @XmlElement
    private int interval = 5;

    @XmlElement
    private int delta = 5;

    @XmlElement
    private String areaNotationField;

    @XmlElement
    private String areaField;

    @XmlElement
    private String parentAreaField;

    @XmlElement
    private String areaNotationTranslationFile;

    @XmlElementWrapper(name = "groupBy")
    @XmlElement(name = "group")
    private String[] groupBy;

    public PathcastConfiguration() {
        setVariable("pathCast");
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
     * @param areaField
     *            the areaField to set
     */
    public void setAreaField(String areaField) {
        this.areaField = areaField;
    }

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

    public String[] getGroupBy() {
        return groupBy;
    }

    public void setGroupBy(String[] groupBy) {
        this.groupBy = groupBy;
    }

    public int getInterval() {
        return interval;
    }

    public void setInterval(int interval) {
        this.interval = interval;
    }

    public int getMaxGroup() {
        return maxGroup;
    }

    public void setMaxGroup(int maxGroup) {
        this.maxGroup = maxGroup;
    }

    public int getDelta() {
        return delta;
    }

    public void setDelta(int delta) {
        this.delta = delta;
    }

    public void validate() {
        Validate.isTrue(
                getMaxResults() > 0,
                "Max count must be greater than zero. Check .xml if maxCount is set in pathcastConfig.\n");
        Validate.isTrue(
                maxGroup > 0,
                "Max group must be greater than zero. Check .xml if maxGroup is set in pathcastConfig.\n");
        Validate.notNull(
                areaField,
                "An area field must be provided. Check .xml if areaField is set in pathcastConfig.\n");
        Validate.notNull(
                getPointField(),
                "A point field must be provided. Check .xml if pointField is set in pathcastConfig.\n");
        Validate.isTrue(
                (!(getAreaNotationTranslationFile() != null && areaNotationField == null)),
                "Area notation field must be provided if translation is specified. Check .xml if areaNotationField and areaNotationTranslationFile are set in pathcastConfig.\n");
        Validate.notNull(
                getPointSource(),
                "Point source must be provided for pointcast to operate. Check .xml if pointSource is set in geospatialConfig.\n");
    }

}
