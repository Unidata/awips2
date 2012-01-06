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

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Aviation Forecaster Configuration
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    2/7/2008     817         grichard    Initial Creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */
@XmlRootElement(name = "aviationForecasterConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class AviationForecasterConfig implements ISerializableObject {

    /**
     * The aviation forecaster configuration.
     */
    @XmlElement(name = "forecaster")
    private ArrayList<ForecasterConfig> forecasterConfig;

    /**
     * Method that gets the aviation forecaster configuration
     * 
     * @return forecasterConfig
     */
    public ArrayList<ForecasterConfig> getForecasterConfig() {
        return forecasterConfig;
    }

    /**
     * Method that sets the aviation forecaster configuration
     * 
     * @param forecasterConfig
     *            -- the aviation forecaster configuration
     */
    public void setForecasterConfig(ArrayList<ForecasterConfig> forecasterConfig) {
        this.forecasterConfig = forecasterConfig;
    }
}
