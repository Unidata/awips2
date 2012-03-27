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
package com.raytheon.uf.viz.collaboration.comm.provider.session;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IRenderable;

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class TestJAXBObject implements IRenderable, ISerializableObject {

    @XmlElement
    protected String item_1 = null;

    @XmlElement
    private Integer value = -1;

    public TestJAXBObject() {
    }

    /**
     * @return the item_1
     */
    public String getItem_1() {
        return item_1;
    }

    /**
     * @param item_1 the item_1 to set
     */
    public void setItem_1(String item_1) {
        this.item_1 = item_1;
    }

    /**
     * @return the value
     */
    public Integer getValue() {
        return value;
    }

    /**
     * @param value the value to set
     */
    public void setValue(Integer value) {
        this.value = value;
    }
}
