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
package com.raytheon.uf.edex.python.decoder;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.PluginDataObject;

/**
 * A PythonDecoder, modified to allow a time offset string (as passed in -z
 * command line options) to be applied during the decoding. This is intended to
 * allow time-offset products to be ingested into the practice active table, and
 * is therefore written around the current UI of WarningDecoder.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 27, 2011           wldougher   Initial creation
 * Oct 03, 2013  2402     bsteffen    Make PythonDecoder more extendable.
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

public class TimeOffsetDecoder extends PythonDecoder {

    /**
     * Constructor.
     */
    public TimeOffsetDecoder() {
        super();
    }

    /**
     * Decode a file with an offset time.
     * 
     * @param file
     *            The file to decode
     * @param drtString
     *            The time offset string as used by
     *            TimeUtil.py::determineDrtOffset()
     * @return An array of decoded records, which may be zero-length, but is not
     *         null.
     * @throws Exception
     *             if anything goes wrong, typically in Python or converting
     *             Python structures to Java
     */
    public PluginDataObject[] decode(File file, Headers headers)
            throws Exception {

        StringBuilder sb = new StringBuilder("cmd -f ");
        sb.append(file.getPath());
        Boolean notifyGFE = (Boolean) headers.get("notifygfe");
        if (Boolean.TRUE.equals(notifyGFE)) {
            sb.append(" -g");
        }
        String drtString = (String) headers.get("drtstring");
        if (drtString != null && !"".equals(drtString)) {
            sb.append(" -z ").append(drtString);
        }

        // create an argument map to run the decoder
        Map<String, Object> decoderArgs = new HashMap<String, Object>(4);
        decoderArgs.put("filePath", null);
        decoderArgs.put("command", sb.toString());
        return decode(decoderArgs);

    }

}
