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

import java.util.ArrayList;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElements;
import jakarta.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name = "TrendSets")
@XmlAccessorType(XmlAccessType.NONE)
public class SCANTrendSetsXML {

    @XmlElements({ @XmlElement(name = "TrendSet", type = SCANTrendSetXML.class) })
    private ArrayList<SCANTrendSetXML> trendSets;

    public SCANTrendSetsXML() {
    }

    public ArrayList<SCANTrendSetXML> getTrendSets() {
        return trendSets;
    }

    public void setTrendSets(ArrayList<SCANTrendSetXML> trendSets) {
        this.trendSets = trendSets;
    }
}
