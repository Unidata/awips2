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
package com.raytheon.uf.edex.activetable.decoder;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.activetable.SendPracticeProductRequest;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.edex.python.decoder.PythonDecoder;

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
 * Nov 14, 2014  4953     randerso    Renamed to PracticeVtecDecoder since it is now
 *                                    used for all practice VTEC products
 *                                    Changed to take in the SendPracticeProductRequest
 *                                    to simplify spring wiring
 *                                    Changed to set the filepath when calling Python decoder
 *                                    so _checkForVTEC will work
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

public class PracticeVtecDecoder extends PythonDecoder {

    /**
     * Constructor.
     */
    public PracticeVtecDecoder() {
        super();
    }

    /**
     * Dump product text to temp file
     * 
     * @param productText
     *            product text
     * @return the temp file
     */
    public static File dumpProductToTempFile(String productText) {
        File file = Util.createTempFile(productText.getBytes(), "vtec");
        file.deleteOnExit();
        return file;
    }

    /**
     * Decode a practice VTEC product with a time offset.
     * 
     * @param req
     * @return An array of decoded records, which may be zero-length, but is not
     *         null.
     * @throws Exception
     *             if anything goes wrong, typically in Python or converting
     *             Python structures to Java
     */
    public PluginDataObject[] decode(SendPracticeProductRequest req)
            throws Exception {

        File file = dumpProductToTempFile(req.getProductText());

        StringBuilder sb = new StringBuilder("cmd -f ");
        sb.append(file.getPath());
        Boolean notifyGFE = req.isNotifyGFE();
        if (Boolean.TRUE.equals(notifyGFE)) {
            sb.append(" -g");
        }
        String drtString = req.getDrtString();
        if ((drtString != null) && !drtString.isEmpty()) {
            sb.append(" -z ").append(drtString);
        }

        // create an argument map to run the decoder
        Map<String, Object> decoderArgs = new HashMap<String, Object>(4);
        decoderArgs.put("filePath", file.getAbsolutePath());
        decoderArgs.put("command", sb.toString());
        return decode(decoderArgs);

    }

}
