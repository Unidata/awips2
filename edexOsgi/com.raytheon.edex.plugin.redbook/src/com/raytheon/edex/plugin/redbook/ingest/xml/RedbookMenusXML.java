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
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * NDM Redbook hazard menu xml object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 07, 2014    2858    mpduff      Initial creation.
 * Mar 17, 2014    2855    mpduff      Renamed to RedbookMenusXML.java.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

@XmlRootElement(name = "redbookMenu")
@XmlAccessorType(XmlAccessType.NONE)
public class RedbookMenusXML {

    /**
     * List of MenuEntry items
     */
    @XmlElements({ @XmlElement(name = "menuEntry", type = MenuEntry.class) })
    protected List<MenuEntry> menuEntryList = new ArrayList<MenuEntry>();

    /**
     * Default constructor
     */
    public RedbookMenusXML() {

    }

    /**
     * @return the menuEntryList
     */
    public List<MenuEntry> getMenuEntryList() {
        return menuEntryList;
    }

    /**
     * @param menuEntryList
     *            the menuEntryList to set
     */
    public void setMenuEntryList(List<MenuEntry> menuEntryList) {
        this.menuEntryList = menuEntryList;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        final String nl = System.getProperty("line.separator");
        StringBuilder sb = new StringBuilder("RedbookMenusXML").append(nl);
        for (MenuEntry sub : menuEntryList) {
            sb.append(sub.toString());
        }
        return sb.toString();
    }
}
