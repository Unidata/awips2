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

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * SCAN Sample Configuration XML.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 5, 2011            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

@XmlRootElement(name = "ScanSampleConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class ScanSampleConfigXML implements ISerializableObject {
    
    @XmlElement(name="cellSampleConfig", type=CellXML.class)
    private CellXML cellSampleConfig;
    
    @XmlElement(name="dmdSampleConfig", type=DmdXML.class)
    private DmdXML dmdSampleConfig;

    /**
     * @return the cellSampleConfig
     */
    public CellXML getCellSampleConfig() {
        return cellSampleConfig;
    }

    /**
     * @param cellSampleConfig the cellSampleConfig to set
     */
    public void setCellSampleConfig(CellXML cellSampleConfig) {
        this.cellSampleConfig = cellSampleConfig;
    }

    /**
     * @return the dmdSampleConfig
     */
    public DmdXML getDmdSampleConfig() {
        return dmdSampleConfig;
    }

    /**
     * @param dmdSampleConfig the dmdSampleConfig to set
     */
    public void setDmdSampleConfig(DmdXML dmdSampleConfig) {
        this.dmdSampleConfig = dmdSampleConfig;
    }
}