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

import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * Consolidates the logic for working with a "content" slot. In this encoder
 * there is a specific slot named "content" which contains a serialized form of
 * the object. This class is to remain package-private as it is an
 * implementation detail that should not be available in the public API.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 07, 2012 1102       djohnson     Initial creation
 * Jun 03, 2013 2038       djohnson     Add equals/hashcode.
 * 12/2/2013    1829       bphillip    Changed slot field in ExtensibleObjectType to be List instead of Set
 * Dec 04, 2013 2584       dhladky      Versioning for registry objects
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
abstract class ContentSlotBasedEncoder<SLOT_VALUE_TYPE extends ValueType, CONTENT_TYPE>
        implements IRegistryEncoder {

    /**
     * The slot attribute name designated for storing the "serialized" form of
     * the object.
     */
    private static final String CONTENT_SLOT = "content";

    /**
     * The type of encoder it is.
     */
    private final RegistryEncoders.Type type;
    
    /**
     * Constructor. Intentionally package-private.
     */
    ContentSlotBasedEncoder(RegistryEncoders.Type type) {
        this.type = type;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final Object decodeObject(RegistryObjectType registryObjectType)
            throws SerializationException {
        Object object = null;
        List<SlotType> returnedSlots = registryObjectType.getSlot();
        // Figure out which version we have and it's class
        VersionInfoType vit = registryObjectType.getVersionInfo();
        String className = registryObjectType.getSlotValue(RegistryUtil.registryObjectClassName);

        // Walk the returned slots looking for the "content" slot
        for (SlotType s : returnedSlots) {
            if (CONTENT_SLOT.equals(s.getName())) {
                SLOT_VALUE_TYPE sv = getSlotValueTypeClass().cast(
                        s.getSlotValue());
                CONTENT_TYPE content = getContent(sv);
                object = decodeContent(content, className, vit.getUserVersionName());
                break;
            }
        }

        return object;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final SlotType encodeObject(Object objectToEncode)
            throws SerializationException {
        SlotType slot = new SlotType();
        CONTENT_TYPE encodedVersion = encodeContent(objectToEncode);
        SLOT_VALUE_TYPE sv = getSlotValueWithValueSet(encodedVersion);
        slot.setName(CONTENT_SLOT);
        slot.setSlotValue(sv);

        return slot;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof ContentSlotBasedEncoder) {
            ContentSlotBasedEncoder<?, ?> other = (ContentSlotBasedEncoder<?, ?>) obj;

            EqualsBuilder builder = new EqualsBuilder();
            builder.append(this.type, other.type);
            return builder.isEquals();
        }
        return super.equals(obj);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(type);

        return builder.toHashCode();
    }

    /**
     * Create a new instance of the slot value type and place the encoded object
     * as the value.
     * 
     * @param encoded
     *            the encoded content
     * @return the slot
     */
    abstract SLOT_VALUE_TYPE getSlotValueWithValueSet(CONTENT_TYPE encoded);

    /**
     * Retrieve the content from the slot.
     * 
     * @param slot
     *            the slot
     * @return the content
     */
    abstract CONTENT_TYPE getContent(SLOT_VALUE_TYPE slot);

    /**
     * Retrieve the {@link Class} instance for the slot value type.
     * 
     * @return the {@link Class} instance
     */
    abstract Class<SLOT_VALUE_TYPE> getSlotValueTypeClass();

    /**
     * Decodes the string into an object.
     * 
     * @param content
     *            the content
     * @param the className
     * @param the version for serialization
     * @return the decoded object
     * @throws SerializationException
     *             on error decoding the string into an object
     */
    abstract Object decodeContent(CONTENT_TYPE content, String className, String version)
            throws SerializationException;

    /**
     * Encodes the object into the type specified by the implementation.
     * 
     * @param objectToEncode
     *            the object
     * @return the encoded type
     * @throws SerializationException
     *             on error encoding the object into a string
     */
    abstract CONTENT_TYPE encodeContent(Object objectToEncode)
            throws SerializationException;
}
