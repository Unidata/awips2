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
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Contains static information used by the Climate Menu Dialogs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 1, 2011  8896       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
@XmlRootElement(name = "ClimateTimeouts")
@XmlAccessorType(XmlAccessType.NONE)
public class ClimateTimeouts implements ISerializableObject {
    @XmlElement(name = "ClimateMetarTimeout")
    private int climateMetarTimeout;

    @XmlElement(name = "WindRoseTimeout")
    private int windRoseTimeout;

    @XmlElement(name = "CigVisDistTimeout")
    private int cigVisDistTimeout;

    @XmlElement(name = "CigVisTrendTimeout")
    private int cigVisTrendTimeout;

    public ClimateTimeouts() {

    }

    public int getClimateMetarTimeout() {
        return climateMetarTimeout;
    }

    public int getWindRoseTimeout() {
        return windRoseTimeout;
    }

    public int getCigVisDistTimeout() {
        return cigVisDistTimeout;
    }

    public int getCigVisTrendTimeout() {
        return cigVisTrendTimeout;
    }

    public void setClimateMetarTimeout(int climateMetarTimeout) {
        this.climateMetarTimeout = climateMetarTimeout;
    }

    public void setWindRoseTimeout(int windRoseTimeout) {
        this.windRoseTimeout = windRoseTimeout;
    }

    public void setCigVisDistTimeout(int cigVisDistTimeout) {
        this.cigVisDistTimeout = cigVisDistTimeout;
    }

    public void setCigVisTrendTimeout(int cigVisTrendTimeout) {
        this.cigVisTrendTimeout = cigVisTrendTimeout;
    }

}
