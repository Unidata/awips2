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
package com.raytheon.uf.edex.decodertools.core;

/**
 * Implementations of this interface are adapters between some data source and
 * the using client code. Note that the idea of big/small endian data is not
 * addressed in the interface. This matter is specific to the data and is an
 * implementation detail.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 27 July 2007        411 jkorman     Initial Development
 * 20070912            379 jkorman     Code review cleanup.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1
 */
public interface IBinDataSource {

    /**
     * Are there at least a specified number of bytes left in the data source?
     * 
     * @param count
     *            Number of bytes to check for.
     * @return
     */
    public boolean available(int count);

    /**
     * Get a signed 8 bit value from the data source.
     * 
     * @return A signed 8 bit value.
     */
    public int getS8();

    /**
     * Get an unsigned 8 bit value from the data source.
     * 
     * @return An unsigned 8 bit value.
     */
    public int getU8();

    /**
     * Get an unsigned 16 bit value from the data source.
     * 
     * @return An unsigned 16 bit value.
     */
    public int getU16();

    /**
     * Get an unsigned 32 bit value from the data source.
     * 
     * @return An unsigned 32 bit value.
     */
    public long getU32();

}