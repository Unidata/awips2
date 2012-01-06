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

/**
 * Forecaster Configuration class
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    2/6/2008     817         grichard    Initial Creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ForecasterConfig {
    /**
     * The forecaster's id number
     */
    @XmlElement(name = "id")
    private String forecasterId = "0";

    /**
     * The forecaster's name
     */
    @XmlElement(name = "name")
    private String forecasterName;

    /**
     * The forecaster's transmit privilege
     */
    @XmlElement(name = "xmit")
    private String xmitPrivilege;

    /**
     * Getters and setters
     */

    public String getName() {
        return forecasterName;
    }

    public boolean getXmitPrivilege() {
        System.out.println("xmitPrivilege = " + xmitPrivilege);
        return Boolean.parseBoolean(xmitPrivilege);
    }

    public int getId() {
        return Integer.parseInt(forecasterId);
    }

    public void setName(String name) {
        this.forecasterName = name;
    }

    public void setXmit(boolean privilege) {
        this.xmitPrivilege = Boolean.toString(privilege);
    }

    public void setId(int id) {
        this.forecasterId = String.format("%1$03d", id);
    }
}
