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
package com.raytheon.uf.common.dataplugin.sfcobs;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 25, 2007  391      jkorman     Initial Coding.
 * Dec 03, 2013  2537     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class AncPrecip extends AncBase implements Serializable {

    private static final long serialVersionUID = 1L;

    public static final Integer PRECIP_TOTAL = 0;
    
    // Precipitation amount in millimeters.
    @DynamicSerializeElement
    @XmlAttribute
    private Double precipAmount;

    public AncPrecip() {
    }

    /**
     * @return the precipAmount
     */
    public Double getPrecipAmount() {
        return precipAmount;
    }

    /**
     * @param precipAmount
     *            the precipAmount to set
     */
    public void setPrecipAmount(Double precipAmount) {
        this.precipAmount = precipAmount;
    }

}
