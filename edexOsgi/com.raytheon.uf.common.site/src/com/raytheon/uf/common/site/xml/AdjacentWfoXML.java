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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * XML format for a list of {@link CwaXML}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Dec 22, 2009           mpduff      Initial creation
 * Oct 24, 2013  2491     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

@XmlRootElement(name = "adjacentWFO")
@XmlAccessorType(XmlAccessType.NONE)
public class AdjacentWfoXML {
    @XmlElements( { @XmlElement(name = "cwa", type = CwaXML.class) })
    private ArrayList<CwaXML> areaIds;

    public AdjacentWfoXML() {
        
    }

    /**
     * @return the areaIds
     */
    public ArrayList<CwaXML> getAreaIds() {
        return areaIds;
    }

    /**
     * @param areaIds the areaIds to set
     */
    public void setAreaIds(ArrayList<CwaXML> areaIds) {
        this.areaIds = areaIds;
    }
}
