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

@XmlRootElement(name = "ScanMonitorConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class SCANMonitorConfigXML {
    @XmlElement(name = "CellTilt")
    private double cellTilt;

    @XmlElement(name = "DmdTilt")
    private double dmdTilt;

    @XmlElement(name = "Interval")
    private int interval;

    @XmlElement(name = "Plugins")
    private String plugins;

    public SCANMonitorConfigXML() {
    }

    public double getCellTilt() {
        return cellTilt;
    }

    public void setCellTilt(double cellTilt) {
        this.cellTilt = cellTilt;
    }

    public double getDmdTilt() {
        return dmdTilt;
    }

    public void setDmdTilt(double dmdTilt) {
        this.dmdTilt = dmdTilt;
    }

    public int getInterval() {
        return interval;
    }

    public void setInterval(int interval) {
        this.interval = interval;
    }

    public String getPlugins() {
        return plugins;
    }

    public void setPlugins(String plugins) {
        this.plugins = plugins;
    }
}
