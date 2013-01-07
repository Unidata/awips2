package com.raytheon.uf.common.registry.ebxml.slots;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.IntegerValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

public class IntegerSlotConverter implements SlotConverter {

    public static final SlotConverter INSTANCE = new IntegerSlotConverter();

    @Override
    public List<SlotType> getSlots(String slotName, Object slotValue) throws IllegalArgumentException {
        List<SlotType> slots = new ArrayList<SlotType>();
        
        // Integer slot will accommodate integer, long and BigInteger types...
        if (slotValue instanceof Integer || 
            slotValue instanceof BigInteger || 
            slotValue instanceof Long) {

            SlotType slot = new SlotType();
            IntegerValueType sv = new IntegerValueType();
            slot.setName(slotName);
            sv.setValue(new BigInteger(slotValue.toString().trim()));
            slot.setSlotValue(sv);
            slots.add(slot);
        }
        else {
           throw new IllegalArgumentException("Object of type " + slotValue.getClass().getName() +
                   " cannot be converted by " + IntegerSlotConverter.class.getName());
        }
        return slots;
    }
    

}
