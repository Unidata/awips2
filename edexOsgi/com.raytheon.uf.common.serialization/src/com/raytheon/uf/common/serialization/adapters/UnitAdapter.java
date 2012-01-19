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
package com.raytheon.uf.common.serialization.adapters;

import java.text.ParseException;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.xml.bind.annotation.adapters.XmlAdapter;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * Serialization adapter for Unit
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Aug 12, 2008				njensen	Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class UnitAdapter extends XmlAdapter<String, Unit<?>> implements
        ISerializationTypeAdapter<Unit<?>> {

    @Override
    public String marshal(Unit<?> v) throws Exception {
        if (v == null) {
            return "";
        } else {
            return v.toString();
        }
    }

    @Override
    public Unit<?> unmarshal(String unit) throws Exception {
        Unit<?> retVal = Unit.ONE;

        if (unit != null) {
            if (!unit.equals("")) {
                retVal = (Unit<?>) UnitFormat.getUCUMInstance().parseObject(
                        unit);
            }
        }
        return retVal;
    }

    @Override
    public Unit<?> deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        Unit<?> retVal = Unit.ONE;
        String str = deserializer.readString();
        if (str != null) {
            if (!str.equals("")) {
                try {
                    retVal = (Unit<?>) UnitFormat.getUCUMInstance()
                            .parseObject(str);
                } catch (ParseException e) {
                    throw new SerializationException(
                            "Error parsing unit from string " + str, e);
                }
            }
        }

        return retVal;
    }

    @Override
    public void serialize(ISerializationContext serializer, Unit<?> object)
            throws SerializationException {
        serializer.writeString(object.toString());
    }

}
