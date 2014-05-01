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

/**
 * VGB's
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * July 14, 2010     3734       dhladky    Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlRootElement(name = "FFMPRunConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class FFMPRunConfigXML {

    @XmlElements({ @XmlElement(name = "runner", type = FFMPRunXML.class) })
    private ArrayList<FFMPRunXML> runner;

    public ArrayList<FFMPRunXML> getFFMPRun() {
        return runner;
    }

    public void setFFMPRun(ArrayList<FFMPRunXML> runner) {
        this.runner = runner;
    }

    public void add(FFMPRunXML run) {
        if (runner == null) {
            runner = new ArrayList<FFMPRunXML>();
        }
        runner.add(run);
    }

}
