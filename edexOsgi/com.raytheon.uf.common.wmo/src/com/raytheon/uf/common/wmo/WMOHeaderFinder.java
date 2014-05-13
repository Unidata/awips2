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
package com.raytheon.uf.common.wmo;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 12, 2009            jkorman     Initial creation
 * May 14, 2014 2536       bclement    moved WMO Header to common
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class WMOHeaderFinder {
    public static final int NOT_FOUND = -1;
    
    private static final Pattern WMO_HEADER_PATTERN = Pattern
    .compile(WMOHeader.WMO_HEADER);

    private String msgStr = null;
    
    private int nextFind = 0;
    
    private Matcher matcher;
    
    public WMOHeaderFinder(byte [] messageData) {
        char [] message = new char[messageData.length];
        for(int i = 0;i < messageData.length;i++) {
            message[i] = (char) (messageData[i] & 0xFF);
        }
        msgStr = new String(message);
        if(msgStr != null) {
            matcher = WMO_HEADER_PATTERN.matcher(msgStr);  
        }
    }

    public int getNext() {
        int retValue = NOT_FOUND;
        if(matcher != null) {
            if(matcher.find(nextFind)) {
                retValue = matcher.start();
                nextFind = matcher.end();
            }
        }
        return retValue;
    }

    /**
     * 
     */
    public void dispose() {
        matcher = null;
    }
    
    public static final void main(String [] args) {
        
        File f = new File("/common/jkorman/awips/edex/data/sbn/JUSA41_KWBC_111600.bufr");
        if(f.exists()) {
            
            InputStream is = null;
            try {
                is = new FileInputStream(f);
                
                byte [] data = new byte [(int) f.length()];
                is.read(data);
                
                WMOHeaderFinder finder = new WMOHeaderFinder(data);
                int n = 0;
                while((n = finder.getNext()) > NOT_FOUND) {
                    System.out.println(n);
                }
                System.out.println(f.length());
                finder.dispose();
            } catch(IOException ioe) {
                
            } finally {
                if(is != null) {
                    try {
                        is.close();
                    } catch(IOException ioe) {
                        
                    }
                }
            }
        }
    }
}
