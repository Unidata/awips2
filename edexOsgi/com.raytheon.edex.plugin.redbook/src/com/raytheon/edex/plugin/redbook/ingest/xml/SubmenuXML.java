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
package com.raytheon.edex.plugin.redbook.ingest.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

/**
 * Redbook hazard submenu object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 07, 2014    2858    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class SubmenuXML {
    /**
     * Submenu text
     */
    @XmlAttribute(name = "name")
    protected String name;

    /**
     * Submenu unique id
     */
    @XmlAttribute(name = "id")
    protected String id;

    /**
     * List of menu items (buttons) for the submenu
     */
    @XmlElements({ @XmlElement(name = "button", type = ButtonXML.class) })
    protected List<ButtonXML> buttonList = new ArrayList<ButtonXML>();

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the buttonList
     */
    public List<ButtonXML> getButtonList() {
        return buttonList;
    }

    /**
     * @param buttonList
     *            the buttonList to set
     */
    public void setButtonList(List<ButtonXML> buttonList) {
        this.buttonList = buttonList;
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

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        final String nl = System.getProperty("line.separator");
        StringBuilder sb = new StringBuilder();
        sb.append("Submenu Name: ").append(this.name).append(nl);
        for (ButtonXML xml : this.buttonList) {
            sb.append(xml.toString());
        }

        return sb.toString();
    }
}
