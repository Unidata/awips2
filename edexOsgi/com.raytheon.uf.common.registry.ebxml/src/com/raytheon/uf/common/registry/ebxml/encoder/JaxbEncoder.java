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

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * A {@link StringBasedEncoder} implementation that uses JAXB. Package-private
 * as it is not directly accessibly in the public API.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 7, 2012  1102      djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

class JaxbEncoder extends StringBasedEncoder {

    /**
     * {@inheritDoc}
     */
    @Override
    Object decodeContent(String content) throws SerializationException {
        try {
            return SerializationUtil.unmarshalFromXml(content);
        } catch (JAXBException e) {
            throw new SerializationException("Unable to decode the object!", e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    String encodeContent(Object objectToEncode) throws SerializationException {
        try {
            return new String(SerializationUtil.marshalToXml(objectToEncode));
        } catch (JAXBException e) {
            throw new SerializationException("Unable to encode the object!", e);
        }
    }
}
