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

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

import com.raytheon.uf.common.serialization.SerializationException;

/**
 * Defines the encoding/decoding strategy to store an object in the registry.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 7, 2012  1102       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public interface IRegistryEncoder {
    /**
     * Decodes an object from its encoded form.
     * 
     * @param registryObjectType
     *            the registry object
     * @return the decoded object
     * @throws SerializationException
     */
    Object decodeObject(RegistryObjectType registryObjectType)
            throws SerializationException;

    /**
     * Encodes an object.
     * 
     * @param objectToEncode
     *            the object to encode
     * @return the slot
     * @throws SerializationException
     */
    SlotType encodeObject(Object objectToEncode) throws SerializationException;
}
