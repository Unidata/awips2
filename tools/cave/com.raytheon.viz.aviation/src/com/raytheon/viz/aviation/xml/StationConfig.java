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
package com.raytheon.viz.aviation.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;

/**
 * StationConfig class.
 * 
 * <pre>
 *   
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    2/5/2008     817         grichard    Initial Creation.
 *   
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class StationConfig {

    /**
     * The Weather Forecast Office.
     */
    @XmlElement
    private String wfoSite;

    /**
     * The International Civil Aviation Organizations of interest.
     */
    @XmlElement(name = "icaoOfInterest")
    @XmlElementWrapper(name = "icaosOfInterest")
    private String[] icaosOfInterest;

    /**
     * Getters and Setters
     */

    /**
     * Method that gets the WFO site.
     * 
     * @return the wfoSite
     */
    public String getWfoSite() {
        return wfoSite;
    }

    /**
     * Method that sets the WFO site.
     * 
     * @param wfoSite
     *            the wfoSite to set
     */
    public void setWfoSite(String wfoSite) {
        this.wfoSite = wfoSite;
    }

    /**
     * Method that gets the ICAOs of interest.
     * 
     * @return the icaosOfInterest
     */
    public String[] getIcaosOfInterest() {
        return icaosOfInterest;
    }

    /**
     * Method that sets the ICAOs of interest.
     * 
     * @param icaosOfInterest
     *            the icaosOfInterest to set
     */
    public void setIcaosOfInterest(String[] icaosOfInterest) {
        this.icaosOfInterest = icaosOfInterest;
    }

}
