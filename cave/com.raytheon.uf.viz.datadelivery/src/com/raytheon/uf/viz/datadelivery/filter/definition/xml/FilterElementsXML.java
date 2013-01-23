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
package com.raytheon.uf.viz.datadelivery.filter.definition.xml;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 21, 2012            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class FilterElementsXML {
    @XmlElement(name = "class", type = String.class)
    protected String clazz = null;

    @XmlElement(name = "displayName", type = String.class)
    protected String displayName = null;

    @XmlElement(name = "filterId", type = String.class)
    protected String id = null;

    @XmlElements({ @XmlElement(name = "settings", type = SettingsXML.class) })
    protected ArrayList<SettingsXML> settingsList = new ArrayList<SettingsXML>();
    
    /**
     * @return the clazz
     */
    public String getClazz() {
        return clazz;
    }

    /**
     * @param clazz
     *            the clazz to set
     */
    public void setClazz(String clazz) {
        this.clazz = clazz;
    }

    /**
     * @return the displayName
     */
    public String getDisplayName() {
        return displayName;
    }

    /**
     * @param displayName
     *            the displayName to set
     */
    public void setDisplayName(String displayName) {
        this.displayName = displayName;
    }

    /**
     * @return the id
     */
    public String getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * @return the settingsList
     */
    public ArrayList<SettingsXML> getSettingsList() {
        return settingsList;
    }

    /**
     * @param settingsList
     *            the settingsList to set
     */
    public void setSettingsList(ArrayList<SettingsXML> settingsList) {
        this.settingsList = settingsList;
    }
}
