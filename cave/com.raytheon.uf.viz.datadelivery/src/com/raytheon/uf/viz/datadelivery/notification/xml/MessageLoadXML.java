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
package com.raytheon.uf.viz.datadelivery.notification.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Message Load at Startup XML Config class.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 8, 2012            mpduff     Initial creation.
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */
@XmlAccessorType(XmlAccessType.NONE)
public class MessageLoadXML implements ISerializableObject {
    @XmlElement(name = "loadAllMessages", type = Boolean.class)
    protected boolean loadAllMessages = false;
    
    @XmlElement(name = "loadLast", type = Integer.class)
    protected int loadLast = 48;
    
    @XmlElement(name = "numMessages", type = Boolean.class)
    protected boolean numMessages = false;
    
    @XmlElement(name = "numHours", type = Boolean.class)
    protected boolean numHours = true;

    /**
     * Load All Messages flag.
     * 
     * @return
     *      true if Load All Messages is checked
     */
    public boolean isLoadAllMessages() {
        return loadAllMessages;
    }

    /**
     * Set the Load All Messages flag.
     *  
     * @param loadAllMessages
     *              true for Load All Messages checked
     */
    public void setLoadAllMessages(boolean loadAllMessages) {
        this.loadAllMessages = loadAllMessages;
    }

    /**
     * Get the value for the number of hours to load.
     * 
     * @return
     *     number of hours
     * 
     */
    public int getLoadLast() {
        return loadLast;
    }

    /**
     * Set the Load Last hour value.
     * 
     * @param loadLast
     */
    public void setLoadLast(int loadLast) {
        this.loadLast = loadLast;
    }

    /**
     * Is number of Messages radio button selected flag.
     * 
     * @return
     *      true if number of Messages radio button is selected
     */
    public boolean isNumMessages() {
        return numMessages;
    }

    /**
     * Set the messages flag.
     * 
     * @param numMessages
     *             true if number of Messages radio button is selected
     */
    public void setNumMessages(boolean numMessages) {
        this.numMessages = numMessages;
    }

    /**
     * Is the Hours of Messages radio button selected flag.
     * 
     * @return
     *      true if number of Hours of Messages radio button is selected
     */
    public boolean isNumHours() {
        return numHours;
    }

    /**
     * Set the number of hours flag.
     * 
     * @param numHours
     *             true if Messages radio button is selected
     */
    public void setNumHours(boolean numHours) {
        this.numHours = numHours;
    }
}
