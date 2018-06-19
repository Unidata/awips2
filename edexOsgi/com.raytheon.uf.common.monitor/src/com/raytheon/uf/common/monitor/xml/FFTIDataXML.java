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

/**
 * FFTI data XML.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 01, 2010            wkwock     Initial creation
 * 
 * </pre>
 * 
 * @author wkwock
 * @version 1.0
 */

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name = "FFTIData")
@XmlAccessorType(XmlAccessType.NONE)
public class FFTIDataXML {

    @XmlElement(name = "CWAs", type = FFTICWAsXML.class)
    private FFTICWAsXML cwa;

    @XmlElements({ @XmlElement(name = "Setting", type = FFTISettingXML.class) })
    private ArrayList<FFTISettingXML> settingList;

    public FFTIDataXML() {
        cwa = new FFTICWAsXML();
        settingList = new ArrayList<FFTISettingXML>();
    }

    /**
     * @return the cwaList
     */
    public FFTICWAsXML getCwa() {
        return cwa;
    }

    /**
     * @param cwaList
     *            the cwaList to set
     */
    public void setCwa(FFTICWAsXML cwa) {
        this.cwa = cwa;
    }

    /**
     * @return the settingList
     */
    public ArrayList<FFTISettingXML> getSettingList() {
        return settingList;
    }

    /**
     * @param settingList
     *            the settingList to set
     */
    public void setSettingList(ArrayList<FFTISettingXML> settingList) {
        this.settingList = settingList;
    }

    public void addSetting(FFTISettingXML setting) {
        if (settingList == null) {
            settingList = new ArrayList<FFTISettingXML>();
        }
        this.settingList.add(setting);
    }
}
