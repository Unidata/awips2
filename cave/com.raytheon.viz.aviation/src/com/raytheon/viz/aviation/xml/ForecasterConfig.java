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

import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * Forecaster Configuration class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2/6/2008     817        grichard    Initial creation
 * 5/20/2015    4510       rferrel     Added {@link #getFormattedId()}.
 * 06/09/2015   4515       rferrel     Remove no longer needed xmit field.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */
@XmlAccessorType
public class ForecasterConfig {
    /**
     * The forecaster's id number
     */
    @XmlElement(name = "id")
    private int forecasterId = 0;

    /**
     * The forecaster's name
     */
    @XmlElement(name = "name")
    private String forecasterName;

    /**
     * Getters and setters
     */

    public String getName() {
        return forecasterName;
    }

    public int getId() {
        return forecasterId;
    }

    /**
     * This formats the id to a 3 character string needed for generating the VFT
     * products.
     * 
     * @return formattedId
     */
    public String getFormattedId() {
        return String.format("%1$03d", forecasterId);
    }

    public void setName(String name) {
        this.forecasterName = name;
    }

    public void setId(int id) {
        this.forecasterId = id;
    }
}
