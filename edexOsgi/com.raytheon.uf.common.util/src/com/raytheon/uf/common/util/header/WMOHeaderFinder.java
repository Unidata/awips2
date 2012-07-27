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
package com.raytheon.uf.common.util.header;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Utility class for finding a WMO header
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class WMOHeaderFinder {

    private static Pattern WMOPATTERN = Pattern
            .compile("([A-Z]{3}[A-Z0-9](\\d{0,2}|[A-Z]{0,2}) [A-Z0-9 ]{4} "
                    + "\\d{6}[^\\r\\n]*)[\\r\\n]+");

    /**
     * Finds and returns the WMO header on the {@link File}
     * 
     * @param file
     * @return
     * @throws FileNotFoundException
     * @throws IOException
     */
    public static String find(File file) throws FileNotFoundException,
            IOException {
        return find(new FileInputStream(file));
    }

    /**
     * Finds and returns the WMO header on the data
     * 
     * @param data
     * @return
     * @throws IOException
     */
    public static String find(byte[] data) throws IOException {
        return find(new ByteArrayInputStream(data));
    }

    /**
     * Finds and returns the WMO header from the {@link InputStream}. Didn't
     * expose this function because I wanted to ensure that it was responsible
     * for closing the input stream
     * 
     * @param in
     * @return
     * @throws IOException
     */
    private static String find(InputStream in) throws IOException {
        try {
            byte[] header = new byte[100];
            in.read(header);
            String sHeader = new String(header, "ISO-8859-1");
            Matcher wmoSearch = WMOPATTERN.matcher(sHeader);
            String messageHeader = null;
            if (wmoSearch.find()) {
                messageHeader = wmoSearch.group();
            }
            return messageHeader;
        } finally {
            in.close();
        }
    }
}
