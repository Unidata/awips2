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
package com.raytheon.uf.viz.datadelivery.subscription.xml;

import java.util.Collections;
import java.util.Map;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import com.google.common.collect.Maps;
import com.raytheon.uf.viz.datadelivery.system.Operator;
import com.raytheon.uf.viz.datadelivery.system.OperatorTypes;
import com.raytheon.uf.viz.datadelivery.utils.NameOperationItems;
import com.raytheon.uf.viz.datadelivery.utils.TypeOperationItems;

/**
 * Operator adapter class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 07, 2013   1420     mpduff      Initial creation.
 * Jan 14, 2013   1286     djohnson    Add static versions of the conversion methods.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class OperatorAdapter extends XmlAdapter<String, Operator<?>> {

    private static final Map<String, Operator<?>> OPERATOR_MAP;
    static {
        Map<String, Operator<?>> map = Maps.newHashMap();
        for (Operator<?> operator : NameOperationItems.values()) {
            map.put(toString(operator), operator);
        }
        for (Operator<?> operator : OperatorTypes.values()) {
            map.put(toString(operator), operator);
        }
        for (Operator<?> operator : TypeOperationItems.values()) {
            map.put(toString(operator), operator);
        }
        OPERATOR_MAP = Collections.unmodifiableMap(map);
    }

    /**
     * 
     * {@inheritDoc}
     */
    @Override
    public Operator<?> unmarshal(String v) throws Exception {
        return fromString(v);
    }

    /**
     * 
     * {@inheritDoc}
     */
    @Override
    public String marshal(Operator<?> v) throws Exception {
        return toString(v);
    }

    /**
     * Retrieve an {@link Operator} from its {@link String} representation.
     * 
     * @param asString
     *            the string representation
     * @return
     */
    public static Operator<?> fromString(String asString) {
        return OPERATOR_MAP.get(asString);
    }

    /**
     * Retrieve the {@link String} representation of an {@link Operator}
     * instance.
     * 
     * @param operator
     *            the operator
     * @return the {@link String} representation
     */
    public static String toString(Operator<?> operator) {
        return operator.toString();
    }

}
