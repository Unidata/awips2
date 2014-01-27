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
package com.raytheon.uf.common.monitor.scan.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Scan Alarm XML class.
 * 
 * SOFTWARE HISTORY
 * 
 * Date Ticket# Engineer Description ------------ ---------- -----------
 * -------------------------- Nov 21, 2010 lvenable Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

@XmlRootElement(name = "ScanAlarms")
@XmlAccessorType(XmlAccessType.NONE)
public class ScanAlarmXML {

    @XmlElement(name = "CELL")
    private int cellAlarmTime;

    @XmlElement(name = "DMD")
    private int dmdAlarmTime;

    @XmlElement(name = "MESO")
    private int mesoAlarmTime;

    @XmlElement(name = "TVS")
    private int tvsAlarmTime;

    public ScanAlarmXML() {
    }

    public int getCellAlarmTime() {
        return cellAlarmTime;
    }

    public void setCellAlarmTime(int cellArlarmTime) {
        this.cellAlarmTime = cellArlarmTime;
    }

    public int getDmdAlarmTime() {
        return dmdAlarmTime;
    }

    public void setDmdAlarmTime(int dmdArlarmTime) {
        this.dmdAlarmTime = dmdArlarmTime;
    }

    public int getMesoAlarmTime() {
        return mesoAlarmTime;
    }

    public void setMesoAlarmTime(int mesoArlarmTime) {
        this.mesoAlarmTime = mesoArlarmTime;
    }

    public int getTvsAlarmTime() {
        return tvsAlarmTime;
    }

    public void setTvsAlarmTime(int tvsArlarmTime) {
        this.tvsAlarmTime = tvsArlarmTime;
    }
}
