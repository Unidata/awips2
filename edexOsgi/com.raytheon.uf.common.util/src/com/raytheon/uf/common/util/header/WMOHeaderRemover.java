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

import java.io.IOException;

/**
 * Bean that will attempt to extract the WMO header found in the {@link Headers}
 * argument from the begining of the byte[]
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

public class WMOHeaderRemover {

    public byte[] remove(byte[] data) throws IOException {
        String header = WMOHeaderFinder.find(data);
        if (header == null) {
            // No header found, nothing to do
            return data;
        }

        byte[] headerBytes = header.getBytes();

        boolean equal = false;
        // Do < since there should be something after the header
        if (headerBytes.length < data.length) {
            equal = true;
            for (int i = 0; i < headerBytes.length; ++i) {
                if (headerBytes[i] != data[i]) {
                    equal = false;
                    break;
                }
            }
        }
        if (equal) {
            byte[] newData = new byte[data.length - headerBytes.length];
            System.arraycopy(data, headerBytes.length, newData, 0,
                    newData.length);
            data = newData;
        }
        return data;
    }
}
