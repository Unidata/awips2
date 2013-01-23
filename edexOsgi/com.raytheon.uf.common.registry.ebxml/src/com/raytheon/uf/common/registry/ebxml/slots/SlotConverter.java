package com.raytheon.uf.common.registry.ebxml.slots;

import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

/**
 * A interface for converting an Object reference into RegistryObject slot
 * types.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 15, 2012 355        jspinks     Initial creation
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
public interface SlotConverter {

    /**
     * Extract a List of SlotType Objects from the given slotValue Object.
     * 
     * @param slotName
     *            The name to use for each SlotType created.
     * 
     * @param slotValue
     *            The Object that is to be interrogated for slot values.
     * 
     * @return A List of SlotType Objects extracted from the slotValue Object
     *         provided.
     * 
     * @throws IllegalArgumentException
     *             If the slotValue Object provided cannot be converted to
     *             SlotValue Objects.
     * 
     */
    List<SlotType> getSlots(String slotName, Object slotValue)
            throws IllegalArgumentException;
}
