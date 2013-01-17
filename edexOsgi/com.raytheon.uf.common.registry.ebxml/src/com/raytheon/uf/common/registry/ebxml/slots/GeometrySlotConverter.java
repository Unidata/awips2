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
package com.raytheon.uf.common.registry.ebxml.slots;

import java.util.ArrayList;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import com.vividsolutions.jts.geom.Geometry;

/**
 * Turns {@link Geometry} into Strings for storage in the registry
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class GeometrySlotConverter implements SlotConverter {

    public static final GeometrySlotConverter INSTANCE = new GeometrySlotConverter();

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.registry.ebxml.slots.SlotConverter#getSlots(java
     * .lang.String, java.lang.Object)
     */
    @Override
    public List<SlotType> getSlots(String slotName, Object slotValue)
            throws IllegalArgumentException {
        List<SlotType> slots = new ArrayList<SlotType>();
        if (slotValue instanceof Geometry) {
            Geometry geom = (Geometry) slotValue;
            SlotType slot = new SlotType();
            StringValueType type = new StringValueType();
            slot.setName(slotName);
            type.setStringValue(geom.toText());
            slot.setSlotValue(type);
            slots.add(slot);
        } else {
            throw new IllegalArgumentException("Object of type "
                    + slotValue.getClass().getName()
                    + " cannot be converted by "
                    + GeometrySlotConverter.class.getName());
        }
        return slots;
    }

}
