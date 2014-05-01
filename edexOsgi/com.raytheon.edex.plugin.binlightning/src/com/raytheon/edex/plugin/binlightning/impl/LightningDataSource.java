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
package com.raytheon.edex.plugin.binlightning.impl;

import java.io.ByteArrayInputStream;

import com.raytheon.uf.edex.decodertools.core.IBinDataSource;

/**
 * Wraps a ByteArrayInputStream with access methods specific to binary
 * lightning data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070810            379 jkorman     Initial Coding from prototype.
 * 
 * </pre>
 *
 * @author jkorman
 * @version 1.0
 */
public class LightningDataSource implements IBinDataSource
{
    private ByteArrayInputStream msgData = null;

    /**
     * Create this data source from supplied byte array data.
     * 
     * @param data
     */
    public LightningDataSource(byte[] data)
    {
        msgData = new ByteArrayInputStream(data);
    }

    /**
     * Are there at least a given number of bytes available in the data source.
     * @param A number of bytes to check for.
     * @return Are the bytes available?
     */
    public boolean available(int count)
    {
        return (msgData.available() >= count);
    }

    /**
     * Get a signed 8 bit value from the data source. NOTE : This conversion is
     * not twos-complement!
     * @return A signed 8 bit value as an int.
     */
    public int getS8()
    {
        int s8 = msgData.read();
        int sign = (s8 & 0x80);
        s8 = s8 & 0x7F;
        
        if(sign > 0)
        {
            s8 = 0 - s8;
        }
        return s8;
    }

    /**
     * Get a unsigned 8 bit value from the data source.
     * @return An unsigned 8 bit value as an int.
     */
    public int getU8()
    {
        int u8 = msgData.read();

        return u8;
    }

    /**
     * Get a unsigned 16 bit value from the data source.
     * @return An unsigned 16 bit value as an int.
     */
    public int getU16()
    {
        int u16 = msgData.read();
        u16 |= (msgData.read() << 8);

        return u16 & 0xFFFF;
    }

    /**
     * Get a unsigned 32 bit value from the data source.
     * @return An unsigned 32 bit value as a long.
     */
    public long getU32()
    {
        long u32 = msgData.read();
        u32 |= (msgData.read() << 8);
        u32 |= (msgData.read() << 16);
        u32 |= (msgData.read() << 24);

        return u32 & 0xFFFFFFFFL;
    }

}
