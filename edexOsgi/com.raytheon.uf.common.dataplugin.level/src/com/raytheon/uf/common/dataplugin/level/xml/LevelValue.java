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
package com.raytheon.uf.common.dataplugin.level.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Defines specific level values within a level.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 03, 2009            rjpeter     Initial creation.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@DynamicSerialize
@XmlRootElement(namespace = "group")
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(namespace = "group")
public class LevelValue implements ISerializableObject {
    @DynamicSerializeElement
    @XmlAttribute
    private Double valueOne;

    @DynamicSerializeElement
    @XmlAttribute
    private Double valueTwo;

    public Double getValueOne() {
        return valueOne;
    }

    public void setValueOne(Double valueOne) {
        this.valueOne = valueOne;
    }

    public Double getValueTwo() {
        return valueTwo;
    }

    public void setValueTwo(Double valueTwo) {
        this.valueTwo = valueTwo;
    }

}
