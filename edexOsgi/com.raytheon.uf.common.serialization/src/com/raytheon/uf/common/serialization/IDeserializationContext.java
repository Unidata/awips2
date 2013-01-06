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

/**
 * Defines the interface for deserialization capability
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date	        Ticket#     Engineer    Description
 * ------------	----------	-----------	--------------------------
 * Aug 12, 2008             chammack    Initial creation
 * Sep 14, 2012 1169        djohnson    Added readObject().
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public interface IDeserializationContext {
    /**
     * Read a boolean
     * 
     * @return
     * @throws SerializationException
     */
    boolean readBool() throws SerializationException;

    /**
     * Read a byte
     * 
     * @return
     * @throws SerializationException
     */
    byte readByte() throws SerializationException;

    /**
     * Read a short
     * 
     * @return
     * @throws SerializationException
     */
    short readI16() throws SerializationException;

    /**
     * Read an int
     * 
     * @return
     * @throws SerializationException
     */
    int readI32() throws SerializationException;

    /**
     * Read a long
     * 
     * @return
     * @throws SerializationException
     */
    long readI64() throws SerializationException;

    /**
     * Read a double
     * 
     * @return
     * @throws SerializationException
     */
    double readDouble() throws SerializationException;

    /**
     * Read a double array
     * 
     * @return
     * @throws SerializationException
     */
    double[] readDoubleArray() throws SerializationException;

    /**
     * Read a float
     * 
     * @return
     * @throws SerializationException
     */
    float readFloat() throws SerializationException;

    /**
     * Read a string
     * 
     * @return
     * @throws SerializationException
     */
    String readString() throws SerializationException;

    /**
     * Read a binary blob
     * 
     * @return
     * @throws SerializationException
     */
    byte[] readBinary() throws SerializationException;

    /**
     * Read a float array
     * 
     * @return
     * @throws SerializationException
     */
    float[] readFloatArray() throws SerializationException;

    /**
     * Read a message header
     * 
     * @return
     * @throws SerializationException
     */
    String readMessageStart() throws SerializationException;

    /**
     * Read a message footer
     * 
     * @throws SerializationException
     */
    void readMessageEnd() throws SerializationException;

    /**
     * Read an object.
     * 
     * @return the read object
     * 
     * @throws SerializationException
     */
    Object readObject() throws SerializationException;
}
