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
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Aug 7, 2008				chammack	Initial creation
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
    public abstract void writeBool(boolean b) throws SerializationException;

    /**
     * Write a byte
     * 
     * @param b
     * @throws SerializationException
     */
    public abstract void writeByte(byte b) throws SerializationException;

    /**
     * Write a short
     * 
     * @param i16
     * @throws SerializationException
     */
    public abstract void writeI16(short i16) throws SerializationException;

    /**
     * Write an int
     * 
     * @param i32
     * @throws SerializationException
     */
    public abstract void writeI32(int i32) throws SerializationException;

    /**
     * Write a long
     * 
     * @param i64
     * @throws SerializationException
     */
    public abstract void writeI64(long i64) throws SerializationException;

    /**
     * Write a double
     * 
     * @param dub
     * @throws SerializationException
     */
    public abstract void writeDouble(double dub) throws SerializationException;

    /**
     * Write a double array
     * 
     * @param dub
     * @throws SerializationException
     */
    public abstract void writeDoubleArray(double[] dubs)
            throws SerializationException;

    /**
     * Write a float
     * 
     * @param flt
     * @throws SerializationException
     */
    public abstract void writeFloat(float flt) throws SerializationException;

    /**
     * Write a string
     * 
     * @param str
     * @throws SerializationException
     */
    public abstract void writeString(String str) throws SerializationException;

    /**
     * Write a binary blob
     * 
     * @param bin
     * @throws SerializationException
     */
    public abstract void writeBinary(byte[] bin) throws SerializationException;

    /**
     * Write a float array
     * 
     * @param floats
     * @throws SerializationException
     */
    public abstract void writeFloatArray(float[] floats)
            throws SerializationException;

    /**
     * Write a message header
     * 
     * @param messageName
     * @throws SerializationException
     */
    public abstract void writeMessageStart(String messageName)
            throws SerializationException;

    /**
     * Write a message footer
     * 
     * @throws SerializationException
     */
    public abstract void writeMessageEnd() throws SerializationException;

}
