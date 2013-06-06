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
package com.raytheon.uf.viz.datadelivery.help;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * JaxB xml object for Data Delivery Help.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 4, 2013    2030     mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

@XmlRootElement(name = "Help")
@XmlAccessorType(XmlAccessType.NONE)
public class DataDeliveryHelpXML {

    /**
     * The help title.
     */
    @XmlElement
    private String title;

    /**
     * The help topic entries.
     */
    @XmlElements({ @XmlElement(name = "helpEntry", type = HelpEntryXML.class) })
    protected List<HelpEntryXML> entryList = new ArrayList<HelpEntryXML>();

    /**
     * @return the title
     */
    public String getTitle() {
        return title;
    }

    /**
     * @param title
     *            the title to set
     */
    public void setTitle(String title) {
        this.title = title;
    }

    /**
     * @return the entryList
     */
    public List<HelpEntryXML> getEntryList() {
        return entryList;
    }

    /**
     * @param entryList
     *            the entryList to set
     */
    public void setEntryist(List<HelpEntryXML> entryList) {
        this.entryList = entryList;
    }
}
