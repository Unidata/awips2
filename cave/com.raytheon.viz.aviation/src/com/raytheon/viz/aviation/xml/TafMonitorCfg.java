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

import java.io.File;
import java.util.ArrayList;

import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * 
 * The class contains the TAF monitor configuration data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 07, 2009  2537      lvenable    Initial creation
 * Oct 11, 2011 11239      rferrel     Added unmarshal method.
 * Sep 15, 2015 4880       njensen     Removed ISerializableObject
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
@XmlRootElement(name = "TafMonitorConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class TafMonitorCfg {
    /**
     * Array of monitor configuration data.
     */
    @XmlElements({ @XmlElement(name = "Monitor", type = MonitorCfg.class) })
    private ArrayList<MonitorCfg> monitorCfgs;

    /**
     * Method to unmarshal xml file that can be used in Java and python code.
     * 
     * @param path
     *            xml file
     * @return tafMonCfg
     */
    public static TafMonitorCfg unmarshal(String path) {
        return JAXB.unmarshal(new File(path), TafMonitorCfg.class);
    }

    /**
     * Constructor.
     */
    public TafMonitorCfg() {
    }

    /**
     * Get the array of monitor configuration data.
     * 
     * @return The monitor configuration data.
     */
    public ArrayList<MonitorCfg> getMonitorCfgs() {
        return monitorCfgs;
    }

    /**
     * Set the array of monitor configuration data.
     * 
     * @param monitorCfgs
     *            The monitor configuration data.
     */
    public void setMonitorCfgs(ArrayList<MonitorCfg> monitorCfgs) {
        this.monitorCfgs = monitorCfgs;
    }
}
