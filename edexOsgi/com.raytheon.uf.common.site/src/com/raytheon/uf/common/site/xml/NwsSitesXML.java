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
package com.raytheon.uf.common.site.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * XML object for NWS Site IDs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 29, 2013    1040    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlRootElement(name = "nwsSites")
@XmlAccessorType(XmlAccessType.NONE)
public class NwsSitesXML {
    @XmlElements({ @XmlElement(name = "site", type = SiteIdXML.class) })
    private List<SiteIdXML> siteIds = new ArrayList<SiteIdXML>();

    /**
     * @return the siteIds
     */
    public List<SiteIdXML> getSiteIds() {
        return siteIds;
    }

    /**
     * @param siteIds
     *            the siteIds to set
     */
    public void setSiteIds(ArrayList<SiteIdXML> siteIds) {
        this.siteIds = siteIds;
    }

    /**
     * Add a site id to the list.
     * 
     * @param idXml
     *            The object to add.
     */
    public void addSiteId(SiteIdXML idXml) {
        if (idXml != null) {
            this.siteIds.add(idXml);
        }
    }
}
