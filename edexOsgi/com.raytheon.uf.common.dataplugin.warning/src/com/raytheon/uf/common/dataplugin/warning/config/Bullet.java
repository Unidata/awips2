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
import javax.xml.bind.annotation.XmlAttribute;

/**
 * Bullet
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
public class Bullet {

    /** The bullet name as referenced in the template (a keyword) */
    @XmlAttribute
    private String bulletName;

    /** The bullet's text as shown in WarnGen */
    @XmlAttribute
    private String bulletText;

    /** The bullet's group as shown in WarnGen */
    @XmlAttribute
    private String bulletGroup;

    /** The bullet's type as shown in WarnGen */
    @XmlAttribute
    private String bulletType;

    /** The default as shown in WarnGen */
    @XmlAttribute
    private String bulletDefault;

    /** Map to load with bullet selection */
    @XmlAttribute
    private String loadMap;

    /** Map to load with bullet selection */
    @XmlAttribute
    private String floodSeverity;

    /**
     * The text WarnGen looks for in the warning to pre-select this bullet on
     * follow-ups
     */
    @XmlAttribute
    private String parseString;

    /**
     * The text WarnGen looks for in the warning to add a bullet to a bullet
     * list on follow-ups
     */
    @XmlAttribute
    private String showString;

    public String getParseString() {
        return parseString;
    }

    public void setParseString(String parseString) {
        this.parseString = parseString;
    }

    public String getShowString() {
        return showString;
    }

    public void setShowString(String showString) {
        this.showString = showString;
    }

    public String getBulletDefault() {
        return bulletDefault;
    }

    public void setBulletDefault(String bulletDefault) {
        this.bulletDefault = bulletDefault;
    }

    /**
     * @return the bulletName
     */
    public String getBulletName() {
        return bulletName;
    }

    /**
     * @param bulletName
     *            the bulletName to set
     */
    public void setBulletName(String bulletName) {
        this.bulletName = bulletName;
    }

    /**
     * @return the bulletText
     */
    public String getBulletText() {
        return bulletText;
    }

    /**
     * @param bulletText
     *            the bulletText to set
     */
    public void setBulletText(String bulletText) {
        this.bulletText = bulletText;
    }

    /**
     * @return the group
     */
    public String getBulletGroup() {
        return bulletGroup;
    }

    /**
     * @param group
     *            the group to set
     */
    public void setBulletGroup(String bulletGroup) {
        this.bulletGroup = bulletGroup;
    }

    /**
     * @return the type
     */
    public String getBulletType() {
        return bulletType;
    }

    /**
     * @param group
     *            the group to set
     */
    public void setBulletType(String bulletType) {
        this.bulletType = bulletType;
    }

    public String getLoadMap() {
        return loadMap;
    }

    public void setLoadMap(String loadMap) {
        this.loadMap = loadMap;
    }

    public String getFloodSeverity() {
        return floodSeverity;
    }

    public void setFloodSeverity(String floodSeverity) {
        this.floodSeverity = floodSeverity;
    }

}
