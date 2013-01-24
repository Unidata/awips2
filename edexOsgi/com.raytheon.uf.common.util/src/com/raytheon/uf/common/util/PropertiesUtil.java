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

import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Properties;

/**
 * Utility class to work with {@link Properties}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 28, 2012 819        djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public final class PropertiesUtil {

    /**
     * Prevent instantiation.
     */
    private PropertiesUtil() {
    }

    /**
     * Read an input stream to load a {@link Properties} instance.
     * 
     * @param is
     *            the input stream
     * @return the Properties
     * @throws IOException
     *             on error reading the {@link InputStream}
     */
    public static Properties read(InputStream is) throws IOException {
        Reader reader = null;
        try {
            reader = new InputStreamReader(is);

            Properties properties = new Properties();
            properties.load(reader);

            return properties;
        } finally {
            close(reader);
        }
    }

    /**
     * Read a {@link File} to load a {@link Properties} instance.
     * 
     * @param file
     *            the file to read
     * @return the Properties
     * @throws IOException
     *             on error reading the {@link File}
     */
    public static Properties read(File file) throws IOException {
        InputStream is = null;
        try {
            is = new FileInputStream(file);
            return read(is);
        } finally {
            close(is);
        }
    }

    public static void close(Closeable closeable) {
        if (closeable != null) {
            try {
                closeable.close();
            } catch (IOException e) {
                // Nothing to do, closing...
            }
        }
    }
}
