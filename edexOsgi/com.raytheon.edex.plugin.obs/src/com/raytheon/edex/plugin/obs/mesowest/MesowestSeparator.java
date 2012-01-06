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

package com.raytheon.edex.plugin.obs.mesowest;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.edex.util.Util;

/**
 * 
 * Separator implementation for mesowest plugin.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 15Nov2006	58			Rockwood	Initial creation
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1
 */
public class MesowestSeparator extends AbstractRecordSeparator {

    private final Log theLogger = LogFactory.getLog(getClass());

    /** Regex used to search for METAR messages */
    private static final String WMOICAO = "^[KCP]...,";

    /** PARM header to identifiy individual Mesowest records */
    private static final String PARM = "PARM,";

    /** Pattern object for regex search */
    Pattern pattern;

    /** Regex matcher */
    private Matcher matcher;

    /** List of records contained in file */
    private List<String> records;

    private Iterator<String> iterator = null;

    private List<String> stations;

    /**
     * Constructor.
     * 
     */
    public MesowestSeparator() {
        records = new ArrayList<String>();
        stations = new ArrayList<String>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.AbstractRecordSeparator#setData(byte[])
     */
    public void setData(byte[] data, Headers headers) {
        this.separate(new String(data));
        iterator = records.iterator();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.AbstractRecordSeparator#hasNext()
     */
    public boolean hasNext() {
        if (iterator == null) {
            return false;
        } else {
            return iterator.hasNext();
        }
    }

    public byte[] next() {
        try {
            String temp = iterator.next();
            if (Util.isEmptyString(temp)) {
                return (byte[]) null;
            } else {
                return temp.getBytes();
            }
        } catch (NoSuchElementException e) {
            return (byte[]) null;
        }
    }

    /**
     * 
     * @param message
     */
    private void separate(String message) {

        String[] mesoParts = message.split("\\x0A");
        pattern = Pattern.compile(WMOICAO);

        try {
            // Assigns records
            for (int i = 3; i < mesoParts.length; i++) {
                // Search for a WMO ICAO and, if found, discard
                matcher = pattern.matcher(mesoParts[i]);
                if (!matcher.find()) {
                    // Make sure there are not duplicate entries
                    String[] mesoStation = mesoParts[i].split(",");
                    if (!stations.contains(mesoStation[0])) {
                        records.add(PARM + mesoParts[i]);
                        stations.add(mesoStation[0]);
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            theLogger.warn("No valid METAR records found.");
        }
        return;
    }

    /**
     * Test function
     * 
     * @param args
     */
    public static void main(String[] args) {
        String fileName = "/awips/opt/data/sbn/tst/ascii/mesowest.dat";
        String fileContents = "";
        MesowestSeparator ben = new MesowestSeparator();
        System.out.println(fileName);

        File mesowestData = new File(fileName);
        int fileSize = (int) mesowestData.length();

        try {
            char[] mesoWestContents = new char[fileSize];
            BufferedReader in = new BufferedReader(new FileReader(mesowestData));

            in.read(mesoWestContents);
            in.close();
            fileContents = new String(mesoWestContents);
        } catch (IOException e) {
            e.printStackTrace();
        }
        ben.setData(fileContents.getBytes(), null);
        int i = 0;
        while (ben.hasNext()) {
            System.out.println("Record #" + (++i) + ": \n" + ben.next());
        }
        System.out.println("Stations processed: " + i);
    }

}
