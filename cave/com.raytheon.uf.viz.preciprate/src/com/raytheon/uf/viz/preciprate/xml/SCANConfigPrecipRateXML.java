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
package com.raytheon.uf.viz.preciprate.xml;

import java.util.ArrayList;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElements;
import jakarta.xml.bind.annotation.XmlRootElement;

/**
 * Precip Rate Configuration analogous to legacy dhrParams.txt
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 18, 2011  6779     grichard    Initial creation
 * Oct 24, 2013  2491     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

@XmlRootElement(name = "ScanConfigPrecipRate")
@XmlAccessorType(XmlAccessType.NONE)
public class SCANConfigPrecipRateXML {

    @XmlElements( { @XmlElement(name = "PrecipRate", type = PrecipRateXML.class) })
    private ArrayList<PrecipRateXML> precipRates;
  
    public SCANConfigPrecipRateXML() {
        
    }

    public ArrayList<PrecipRateXML> getPrecipRates() {
        return precipRates;
    }

    public void setPrecipRates(ArrayList<PrecipRateXML> precipRates) {
        this.precipRates = precipRates;
    }
}
