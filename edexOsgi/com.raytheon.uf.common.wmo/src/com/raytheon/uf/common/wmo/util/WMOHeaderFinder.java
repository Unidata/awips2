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
package com.raytheon.uf.common.wmo.util;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.wmo.WMOHeader;

/**
 * Utility class for finding a WMO header
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2012            mschenke    Initial creation
 * Sep 09, 2013 2327       rjpeter     Updated to allow pattern to be used again.  Added capture for DTG
 * Nov 30, 2016 5970       njensen     Moved from common.util plugin
 *                                      Use pattern from WMOHeader and added WMO_TIME_PATTERN
 * </pre>
 * 
 * @author mschenke
 */

public class WMOHeaderFinder {

    private static final Pattern WMO_TIME_PATTERN = Pattern.compile("\\d{6}");

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
        try (FileInputStream fis = new FileInputStream(file)) {
            return find(fis);
        }
    }

    /**
     * Finds and returns the WMO header on the data
     * 
     * @param data
     * @return
     * @throws IOException
     */
    public static String find(byte[] data) throws IOException {
        try (ByteArrayInputStream bais = new ByteArrayInputStream(data)) {
            return find(bais);
        }
    }

    /**
     * Finds and returns the WMO header from the {@link InputStream}.
     * 
     * @param in
     * @return
     * @throws IOException
     */
    private static String find(InputStream in) throws IOException {
        byte[] header = new byte[100];
        in.read(header);
        String sHeader = new String(header, "ISO-8859-1");
        Matcher wmoSearch = WMOHeader.WMO_HEADER_PATTERN.matcher(sHeader);
        String messageHeader = null;
        if (wmoSearch.find()) {
            messageHeader = wmoSearch.group();
        }
        return messageHeader;
    }

    /**
     * Returns the Date Time Group associated with a WMO Header
     * 
     * @param header
     * @return
     */
    public static String findDtg(String header) {
        String dtg = null;
        Matcher matcher = WMO_TIME_PATTERN.matcher(header);

        if (matcher.find()) {
            dtg = matcher.group();
        }

        return dtg;
    }
}
