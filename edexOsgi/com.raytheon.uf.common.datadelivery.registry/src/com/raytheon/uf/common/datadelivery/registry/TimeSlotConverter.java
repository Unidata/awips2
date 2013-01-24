package com.raytheon.uf.common.datadelivery.registry;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.CollectionValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ValueType;

import com.raytheon.uf.common.datadelivery.registry.Time.STEP_UNIT;
import com.raytheon.uf.common.registry.ebxml.CalendarAttribute;
import com.raytheon.uf.common.registry.ebxml.slots.SlotConverter;

/**
 * A <code>SlotConverter</code> implementation for converting a
 * Time Object into a RegistryObject slot type. A collection
 * slot type will be created and each step in the time increment
 * will be added as an element. The collection type for the slot
 * will be set to List.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 15, 2012 455        jspinks     Initial creation
 *
 * </pre>
 *
 * @author jspinks
 * @version 1.0
 */
public class TimeSlotConverter implements SlotConverter {

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
     *             If the slotValue Object provided is not an instance of Time.
     * 
     * @see java.util.Map
     */
    @Override
    public List<SlotType> getSlots(String slotName, Object slotValue) throws IllegalArgumentException {
        List<SlotType> slots = new ArrayList<SlotType>();
        
        
        SlotType slot = new SlotType();
        slot.setName(slotName);
        List<ValueType> collectionValues = new ArrayList<ValueType>();
        CollectionValueType cvt = new CollectionValueType();
        
        
        if (slotValue instanceof Time) {
            
            Time t = (Time)slotValue;
            SimpleDateFormat df = new SimpleDateFormat(CalendarAttribute.DATE_TIME_FORMAT);
            Calendar current = Calendar.getInstance();
            Calendar end = Calendar.getInstance();
            try {
                current.setTime(t.getStartDate());
                end.setTime(t.getEndDate());
            } catch (ParseException e) {
                throw new IllegalArgumentException("Could not format slot [" + slotName + "] with value [" +
                        slotValue.toString() + "]", e);
            }
            
            STEP_UNIT stepu = STEP_UNIT.valueOf(t.getStepUnit().toUpperCase());
            Double step = t.getStep();
            
            // Add One millisecond to handle the case when the step of the Time Object
            // is exactly equal to the end time. 
            end.add(Calendar.MILLISECOND, 1);
            
            while (end.after(current)) {

                collectionValues.add(newEntrySlot(df.format(current.getTime())));

                switch (stepu)
                {
                case SECOND:
                    current.add(Calendar.SECOND, step.intValue());
                    break;
                case MINUTE:
                    current.add(Calendar.MINUTE, step.intValue());
                    break;
                case HOUR:
                    current.add(Calendar.HOUR_OF_DAY, step.intValue());
                    break;
                case WEEK:
                    current.add(Calendar.WEEK_OF_YEAR, step.intValue());
                    break;
                case DAY:
                    current.add(Calendar.DAY_OF_WEEK, step.intValue());
                    break;
                case MONTH:
                    current.add(Calendar.MONTH, step.intValue());
                    break;
                }
            }
            
        }
        else {
           throw new IllegalArgumentException("Object of type " + slotValue.getClass().getName() +
                   " cannot be converted by " + TimeSlotConverter.class.getName());
        }
        
        cvt.setCollectionValue(collectionValues);
        cvt.setCollectionType("urn:oasis:names:tc:ebxml-regrep:CollectionType:List");
        slot.setSlotValue(cvt);
        slots.add(slot);
        return slots;
    }
    
    /**
     * Create a new value for inclusion in the collection slot values.
     * 
     * @param value
     *        The Object to extract the slot value from. 
     *        
     * @return A ValueType to add to the collection slot.
     */
    private ValueType newEntrySlot(String value) {
        
        StringValueType entry = new StringValueType();
        entry.setStringValue(value);
        return entry;
    }
}
