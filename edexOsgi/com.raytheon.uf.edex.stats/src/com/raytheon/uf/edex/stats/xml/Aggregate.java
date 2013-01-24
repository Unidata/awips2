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
package com.raytheon.uf.edex.stats.xml;

import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 *
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2012            jsanchez     Made serializable.
 *
 * </pre>
 *
 * @author jsanchez
 *
 */
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class Aggregate {
    /** the field to perform the function on. */
    @XmlAttribute
    @DynamicSerializeElement
    private String field;

    @XmlAttribute
    @XmlJavaTypeAdapter(value = UnitAdapter.class)
    @DynamicSerializeElement
    private Unit<?> unit = Unit.ONE;

    @XmlAttribute
    @DynamicSerializeElement
    private String displayName;

    /** the name of the statistic function */
    @XmlElement(name = "function")
    @DynamicSerializeElement
    private Item[] functions;

    public String getField() {
        return field;
    }

    public void setField(String field) {
        this.field = field;
    }

    /**
     * @return the unit
     */
    public Unit<?> getUnit() {
        return unit;
    }

    /**
     * @param unit
     *            the unit to set
     */
    public void setUnit(Unit<?> unit) {
        this.unit = unit;
    }

    public Item[] getFunctions() {
        return functions;
    }

    public void setFunctions(Item[] functions) {
        this.functions = functions;
    }

    /**
     * @param displayName
     *            the displayName to set
     */
    public void setDisplayName(String displayName) {
        this.displayName = displayName;
    }

    /**
     * @return the displayName
     */
    public String getDisplayName() {
        return displayName;
    }

}
