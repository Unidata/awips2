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
 * Aviation Monitoring Rules
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    3/21/2008    934         grichard    Initial Creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */
@XmlRootElement(name = "aviationMonitoringRules")
@XmlAccessorType(XmlAccessType.NONE)
public class AviationMonitoringRules implements ISerializableObject {

    /**
     * The aviation monitoring rules
     */
    @XmlElement(name = "rule")
    private ArrayList<MonitoringRule> monitoringRules;

    /**
     * Method that gets the monitoring rules
     * 
     * @return monitoringRules
     */
    public ArrayList<MonitoringRule> getRule() {
        return monitoringRules;
    }

    /**
     * Method that sets the monitoring rules
     * 
     * @param monitoringRules
     *            -- the monitoring rules
     */
    public void setRule(ArrayList<MonitoringRule> monitoringRules) {
        this.monitoringRules = monitoringRules;
    }

}
