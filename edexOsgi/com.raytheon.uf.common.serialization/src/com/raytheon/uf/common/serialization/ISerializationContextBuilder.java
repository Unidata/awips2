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

package com.raytheon.uf.common.serialization;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * Defines an interface for constructing serialization contexts
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Aug 12, 2008				chammack	Initial creation
 * Aug 06, 2013    2228     njensen     Added buildDeserializationContext(byte[], dsm)
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public interface ISerializationContextBuilder {

    /**
     * Build a serialization context
     * 
     * @param data
     *            output stream
     * @param manager
     *            the serialization manager
     * @return a serialization context
     */
    public ISerializationContext buildSerializationContext(OutputStream data,
            DynamicSerializationManager manager);

    /**
     * Build a deserialization context
     * 
     * @param data
     *            input stream
     * @param manager
     *            the serialization manager
     * @return a deserialization context
     */
    public IDeserializationContext buildDeserializationContext(
            InputStream data, DynamicSerializationManager manager);

    /**
     * Build a deserialization context
     * 
     * @param data
     *            the bytes of the data
     * @param manager
     *            the serialization manager
     * @return a deserialization context
     */
    public IDeserializationContext buildDeserializationContext(byte[] data,
            DynamicSerializationManager manager);

}
