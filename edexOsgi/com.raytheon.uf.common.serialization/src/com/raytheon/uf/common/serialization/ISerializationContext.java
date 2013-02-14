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
 * Defines the interface for serialization capability
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Aug 07, 2008             chammack    Initial creation
 * Sep 14, 2012 1169        djohnson    Added writeObject(OBject).
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public interface ISerializationContext {

    /**
     * Write a boolean
     * 
     * @param b
     * @throws SerializationException
     */
    void writeBool(boolean b) throws SerializationException;

    /**
     * Write a byte
     * 
     * @param b
     * @throws SerializationException
     */
    void writeByte(byte b) throws SerializationException;

    /**
     * Write a short
     * 
     * @param i16
     * @throws SerializationException
     */
    void writeI16(short i16) throws SerializationException;

    /**
     * Write an int
     * 
     * @param i32
     * @throws SerializationException
     */
    void writeI32(int i32) throws SerializationException;

    /**
     * Write a long
     * 
     * @param i64
     * @throws SerializationException
     */
    void writeI64(long i64) throws SerializationException;

    /**
     * Write a double
     * 
     * @param dub
     * @throws SerializationException
     */
    void writeDouble(double dub) throws SerializationException;

    /**
     * Write a double array
     * 
     * @param dub
     * @throws SerializationException
     */
    void writeDoubleArray(double[] dubs)
            throws SerializationException;

    /**
     * Write a float
     * 
     * @param flt
     * @throws SerializationException
     */
    void writeFloat(float flt) throws SerializationException;

    /**
     * Write a string
     * 
     * @param str
     * @throws SerializationException
     */
    void writeString(String str) throws SerializationException;

    /**
     * Write a binary blob
     * 
     * @param bin
     * @throws SerializationException
     */
    void writeBinary(byte[] bin) throws SerializationException;

    /**
     * Write a float array
     * 
     * @param floats
     * @throws SerializationException
     */
    void writeFloatArray(float[] floats)
            throws SerializationException;

    /**
     * Write a message header
     * 
     * @param messageName
     * @throws SerializationException
     */
    void writeMessageStart(String messageName)
            throws SerializationException;

    /**
     * Write a message footer
     * 
     * @throws SerializationException
     */
    void writeMessageEnd() throws SerializationException;

    /**
     * Write another object.
     * 
     * @param obj
     *            the object
     * @throws SerializationException
     */
    void writeObject(Object obj) throws SerializationException;
}
