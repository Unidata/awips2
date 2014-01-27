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
package com.raytheon.uf.viz.monitor.scan.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Scan Run Configuration accessor.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 20, 2011           mpduff      Initial creation
 * Oct 23, 2013  2491     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

@XmlRootElement(name = "scanRunConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class ScanRunConfigXML {
    @XmlElement(name="vcpList")
    private ScanVcpListXML vcpList;

    public ScanRunConfigXML() {
        
    }
    
    /**
     * @param vcpList the vcpList to set
     */
    public void setVcpList(ScanVcpListXML vcpList) {
        this.vcpList = vcpList;
    }

    /**
     * @return the vcpList
     */
    public ScanVcpListXML getVcpList() {
        return vcpList;
    }
}
