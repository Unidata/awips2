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
package com.raytheon.rcm.coll;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Calendar;

import com.raytheon.rcm.config.Configuration;
import com.raytheon.rcm.message.GraphicProduct;
import com.raytheon.rcm.message.Message;
import com.raytheon.rcm.server.Log;

/** Implements WAN distribution of radar products.
 * 
 * @author dfriedma
 *
 */
public class Distribution {

    private static final String lineSep = "\r\r\n";

    private static final String MHS_DIST_DIR_PROP_NAME = 
        "com.raytheon.rcm" + ".mhsTmpDir"; 
    
    private static boolean haveMhsDistributionDir;
    private static File mhsDistributionDir;
    
    public static File getMhsDistributionDir() {
        if (! haveMhsDistributionDir) {
            synchronized(Distribution.class) {
                if (! haveMhsDistributionDir) {
                    haveMhsDistributionDir = true;
                    
                    String dir = System.getProperty(MHS_DIST_DIR_PROP_NAME);                
                    String error = null;
                    
                    if (dir != null) {
                        mhsDistributionDir = new File(dir);
                        if (! mhsDistributionDir.exists() || ! mhsDistributionDir.isDirectory())
                            error = String.format("'%s' does not exist or is not a directory", 
                                    mhsDistributionDir);
                        else if (! mhsDistributionDir.canWrite())
                            error = String.format("Directory '%s' is not writable", 
                                    mhsDistributionDir);
                    }
                    
                    if (error != null) {
                        Log.errorf("%s.  Will use the system temporary directory", error);
                        mhsDistributionDir = null;
                    }
                }
            }
        }
        return mhsDistributionDir;
    }

    /**
     * Sends radar data on the WAN interpreting the data as needed.
     * <p>
     * Creates a message with a WMO heading and AWIPS header and sends it on
     * the WAN. The final <i>i</i> part of the WMO heading is taken from
     * config.getRegionCode().
     * <p>
     * NOTE: If the radar product is a Free Text Message (code 74), the text
     * will be extracted and sent rather than the original binary data.
     * 
     * @param data
     *            the radar product data
     * @param productTime
     *            the time to use in the message header
     * @param ttaai
     *            the TTAAi part of the WMO heading
     * @param nnn
     *            the NNN part of the AWIPS header
     * @param radarID
     *            the radar ID
     * @param reason
     *            the reason for sending the product (for logging)
     * @param config
     *            the configuration
     */
    public static void sendProductMessage(byte[] data,
            String ttaai, String nnn, String radarID, String reason,
            Configuration config) {
        
        if (! config.isCollectionEnabled())
            return;
        
        if (ttaai.length() != 5)
            throw new IllegalArgumentException("ttaai value must be 5 characters");
        if (nnn.length() != 3)
            throw new IllegalArgumentException("nnn value must be 3 characters");
        
        int messageCode = Message.messageCodeOf(data);        
        byte[] msgData = data;

        if (messageCode == Message.FREE_TEXT_MESSAGE) {
            msgData = TextRoutines.formatTextProduct(msgData);
            if (msgData == null)
                return;
        }
        
        Calendar productTime;
        if (messageCode >= 16)
            productTime = GraphicProduct.pdbOfMessage(data).volumeScanTime;
        else
            productTime = Message.decodeHeader(data).time;

        String wmoSiteID = config.getWmoSiteID();
        if (wmoSiteID == null || wmoSiteID.length() != 4) {
            String errorMsg = wmoSiteID != null ?
                    String.format("Invalid WMO site ID '%s'", wmoSiteID) :
                        "WMO site ID not set";
            Log.errorf("Cannot send message: %s", errorMsg);
            return;
        }

        int regionCode = config.getRegionCode();
        if (regionCode < 0 || regionCode > 9) {
            // TODO: zero may be invalid too
            Log.errorf("Cannot send message: Invalid region code '%d'", 
                    regionCode);
            return;
        }
        
        String xxx = radarID.substring(1, 4).toUpperCase();

        /* if (commissioned) {
         *   get WMO site ID, region code
         *   ....   
         * } else {
         *   // Holdover from AWIPS 1 for non-commissioned. Probably not needed
         *   headerBuffer.append("SDUS97");
         * } 
         */
            
        StringBuilder headerBuffer = new StringBuilder();
        headerBuffer.append(ttaai);
        headerBuffer.append(regionCode);
        headerBuffer.append(' ');
        headerBuffer.append(wmoSiteID.toUpperCase());
        headerBuffer.append(' ');
        headerBuffer.append(String.format("%1$td%1$tH%1$tM", productTime));
        String wmoHeading = headerBuffer.toString();
        
        headerBuffer.append(lineSep);
        headerBuffer.append(String.format("%s%s%s", nnn, xxx, lineSep));
        String header = headerBuffer.toString();
        
        Log.eventf("Sending %s code=%d header=%s / %s%s (%s)",
                radarID, messageCode, wmoHeading, nnn, xxx, reason);

        File f = null;
        
        try {
            f = File.createTempFile("rcm", null, getMhsDistributionDir());
            FileOutputStream fo = new FileOutputStream(f);
            try {
                fo.write(header.getBytes());
                fo.write(msgData);
            } finally {
                fo.close();
            }
        } catch (IOException e) {
            Log.errorf("Unable to create message file: %s", e);
            if (f != null) {
                try {
                    f.delete();
                } catch (Exception e2) {
                    // ignore
                }
            }
            return;
        }

        // From MhsRadarProd.C
        String[] args = { "-c", "0", "-p", "1", "-e", f.toString(), "-a",
                "DEFAULTNCF" };
        MsgSendRunner msr = new MsgSendRunner(args, f);
        msr.runAsync();     
    }

}
