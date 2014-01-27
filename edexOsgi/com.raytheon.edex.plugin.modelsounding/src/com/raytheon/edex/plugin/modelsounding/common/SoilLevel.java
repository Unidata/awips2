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
package com.raytheon.edex.plugin.modelsounding.common;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * SoilLevel contains the data for a single soil level forecast.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 03, 2008  1026     jkorman     Initial implementation.
 * Dec 02, 2013  2537     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class SoilLevel implements Serializable {

    private static final long serialVersionUID = 1L;

    @DynamicSerializeElement
    @XmlAttribute
    private Double lyrSoilMoist;

    @DynamicSerializeElement
    @XmlAttribute
    private Double lyrSoilTemp;

    /**
     * Construct an empty instance.
     */
    public SoilLevel() {
    }

    /**
     * @return the lyrSoilMoist
     */
    public Double getLyrSoilMoist() {
        return lyrSoilMoist;
    }

    /**
     * @param lyrSoilMoist
     *            the lyrSoilMoist to set
     */
    public void setLyrSoilMoist(Double lyrSoilMoist) {
        this.lyrSoilMoist = lyrSoilMoist;
    }

    /**
     * @return the lyrSoilTemp
     */
    public Double getLyrSoilTemp() {
        return lyrSoilTemp;
    }

    /**
     * @param lyrSoilTemp
     *            the lyrSoilTemp to set
     */
    public void setLyrSoilTemp(Double lyrSoilTemp) {
        this.lyrSoilTemp = lyrSoilTemp;
    }

}
