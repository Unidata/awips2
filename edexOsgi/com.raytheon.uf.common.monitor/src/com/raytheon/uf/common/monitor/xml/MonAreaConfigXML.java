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
package com.raytheon.uf.common.monitor.xml;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Monitoring area configuration xml.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 21, 2009            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
@XmlRootElement(name = "MonitorAreaConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class MonAreaConfigXML  implements ISerializableObject {
    
    @XmlElements( { @XmlElement(name = "AreaID", type = AreaIdXML.class) })
    private ArrayList<AreaIdXML> areaIds = new ArrayList<AreaIdXML>();
    
    @XmlElements( {@XmlElement(name = "timeWindow", type = Integer.class) })
    private int timeWindow;

    @XmlElements( {@XmlElement(name = "shipDistance", type = Integer.class) })
    private int shipDistance;
    
    @XmlElements( {@XmlElement(name = "useAlgorithms", type = Boolean.class) })
    private boolean useAlgorithms;

    public MonAreaConfigXML() {
    }

    public ArrayList<AreaIdXML> getAreaIds() {
        return areaIds;
    }

    public void setAreaIds(ArrayList<AreaIdXML> areaIds) {
        this.areaIds = areaIds;
    }

    /**
     * @return the timeWindow
     */
    public int getTimeWindow() {
        return timeWindow;
    }

    /**
     * @param timeWindow the timeWindow to set
     */
    public void setTimeWindow(int timeWindow) {
        this.timeWindow = timeWindow;
    }

    /**
     * @return the shipDistance
     */
    public int getShipDistance() {
        return shipDistance;
    }

    /**
     * @param shipDistance the shipDistance to set
     */
    public void setShipDistance(int shipDistance) {
        this.shipDistance = shipDistance;
    }

    /**
     * @return the useAlgorithms
     */
    public boolean isUseAlgorithms() {
        return useAlgorithms;
    }

    /**
     * @param useAlgorithms the useAlgorithms to set
     */
    public void setUseAlgorithms(boolean useAlgorithms) {
        this.useAlgorithms = useAlgorithms;
    }
    
    public void addAreaId(AreaIdXML areaXml) {
        areaIds.add(areaXml);
    }
}
