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

package com.raytheon.edex.plugin.obs;

import java.io.File;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.edex.plugin.obs.mesowest.MesowestSeparator;
import com.raytheon.edex.plugin.obs.metar.MetarSeparator;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Separator implementation for observation data types. This class provides a
 * wrapper in order to select the correct separator based on the data type
 * 
 * <pre>
 * 
 * OFTWARE HISTORY
 *                     
 * ate          Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * 4/27/07      199         bphillip    Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class ObsSeparator extends AbstractRecordSeparator {

    private AbstractRecordSeparator separator;

    public ObsSeparator() {

    }

    public static ObsSeparator separate(String fileName, Headers headers)
            throws Exception {
        byte[] data = FileUtil.file2bytes(new File(fileName));
        ObsSeparator ms = new ObsSeparator();
        ms.setData(data, headers);
        return ms;
    }

    public byte[] next() {
        return (byte[]) separator.next();
    }

    public boolean hasNext() {
        return separator.hasNext();
    }

    public void setData(byte[] data, Headers headers) {
        String message = new String(data).trim();

        /*
         * Determine the correct separator to load
         */

        // Load metar separator
        if (message.contains("METAR") || message.contains("SPECI")) {
            separator = new MetarSeparator();
        }

        // Load mesowest separator
        else if (message.contains("PARM")) {
            separator = new MesowestSeparator();
        }
        separator.setData(data, headers);
    }

}
