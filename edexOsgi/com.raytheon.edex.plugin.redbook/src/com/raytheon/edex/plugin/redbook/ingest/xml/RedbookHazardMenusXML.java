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
 * Mar 07, 2014    2858    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

@XmlRootElement(name = "redbookHazardMenu")
@XmlAccessorType(XmlAccessType.NONE)
public class RedbookHazardMenusXML {

    /**
     * Submenus
     */
    @XmlElements({ @XmlElement(name = "subMenu", type = SubmenuXML.class) })
    protected List<SubmenuXML> subMenus = new ArrayList<SubmenuXML>();

    /**
     * Default constructor
     */
    public RedbookHazardMenusXML() {

    }

    /**
     * @return the subMenus
     */
    public List<SubmenuXML> getSubMenus() {
        return subMenus;
    }

    /**
     * @param subMenus
     *            the subMenus to set
     */
    public void setSubMenus(List<SubmenuXML> subMenus) {
        this.subMenus = subMenus;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (SubmenuXML sub : subMenus) {
            sb.append(sub.toString());
        }
        return sb.toString();
    }
}
