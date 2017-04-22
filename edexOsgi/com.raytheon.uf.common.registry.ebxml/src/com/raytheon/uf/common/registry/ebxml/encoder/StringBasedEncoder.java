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
package com.raytheon.uf.common.registry.ebxml.encoder;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import com.raytheon.uf.common.registry.ebxml.encoder.RegistryEncoders.Type;

/**
 * A string-based encoding strategy. Package-private because we want the
 * encoding strategies to remain implementation details that are separate from
 * the public API.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 07, 2012 1102       djohnson     Initial creation
 * Jun 03, 2013 2038       djohnson     Add equals/hashcode.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
abstract class StringBasedEncoder extends
        ContentSlotBasedEncoder<StringValueType, String> {

    /**
     * Constructor. Intentionally package-private.
     * 
     * @param type
     *            the type
     */
    StringBasedEncoder(Type type) {
        super(type);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    StringValueType getSlotValueWithValueSet(String encoded) {
        StringValueType sv = new StringValueType();
        sv.setStringValue(encoded);

        return sv;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    String getContent(StringValueType slot) {
        return slot.getStringValue();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    Class<StringValueType> getSlotValueTypeClass() {
        return StringValueType.class;
    }
}
