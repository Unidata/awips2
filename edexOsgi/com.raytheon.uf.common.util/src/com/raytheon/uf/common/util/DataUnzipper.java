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
package com.raytheon.uf.common.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.GZIPInputStream;
import java.util.zip.ZipInputStream;

/**
 * Bean that will attempt to decompress a byte[]. Will throw IOException if data
 * can not be unzipped
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 17, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DataUnzipper {

    /**
     * Uses ZLIB decompression to unzip the data
     * 
     * @param data
     * @return
     * @throws IOException
     */
    public byte[] unzip(byte[] data) throws IOException {
        return unzip(new ZipInputStream(new ByteArrayInputStream(data)),
                data.length * 2);
    }

    /**
     * Uses GZIP decompression to unzip the data
     * 
     * @param data
     * @return
     * @throws IOException
     */
    public byte[] gunzip(byte[] data) throws IOException {
        return unzip(new GZIPInputStream(new ByteArrayInputStream(data)),
                data.length * 2);
    }

    /**
     * Unzips the {@link InputStream} into a byte[] this function will close the
     * in stream after decompression
     * 
     * @param in
     * @param size
     * @return
     * @throws IOException
     */
    private byte[] unzip(InputStream in, int size) throws IOException {
        ByteArrayOutputStream out = new ByteArrayOutputStream(size);
        try {
            byte[] buf = new byte[4096];
            int len;
            while ((len = in.read(buf)) > 0)
                out.write(buf, 0, len);
        } finally {
            in.close();
        }
        return out.toByteArray();
    }
}
