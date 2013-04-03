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

import org.apache.commons.codec.binary.Base64;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * A {@link StringBasedEncoder} implementation that uses
 * {@link DynamicSerialize} annotations. Package-private as it is not directly
 * accessibly in the public API.
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
class DynamicSerializeEncoder extends StringBasedEncoder {

    /**
     * {@inheritDoc}
     */
    @Override
    Object decodeContent(String content) throws SerializationException {
        return SerializationUtil.transformFromThrift(Base64
                .decodeBase64(content));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    String encodeContent(Object objectToEncode) throws SerializationException {
        return new String(Base64.encodeBase64(SerializationUtil
                .transformToThrift(objectToEncode)));
    }
}
