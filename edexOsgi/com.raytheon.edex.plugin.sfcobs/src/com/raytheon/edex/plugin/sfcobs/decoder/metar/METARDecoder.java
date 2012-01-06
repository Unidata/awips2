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
package com.raytheon.edex.plugin.sfcobs.decoder.metar;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;

/**
 * This is a place holder class. The only implementation is to identify that
 * METAR/SPECI data was received at this decoder and to emit an info message.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070928            391 jkorman     Initial Coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class METARDecoder extends AbstractSfcObsDecoder {

    /** The logger */
    private Log logger = LogFactory.getLog(getClass());

    /**
     * Construct the decoder.
     */
    public METARDecoder() {
        reportPrefix = "METAR";
    }

    /**
     * Check if the observation data represents a NIL observation.
     * 
     * @return Is this observation a possible NIL?
     */
    public boolean isNILObs() {
        boolean isNIL = false;

        if (reportData != null) {
            boolean check = reportData.startsWith("METAR");
            check |= reportData.startsWith("SPECI");

            isNIL = check & reportData.endsWith("NIL");
        }
        return isNIL;
    }

    /**
     * METAR/SPECI data discovered in sfcobs data. For now just return a null
     * reference and log the wmo message header.
     */
    @Override
    public PluginDataObject decode() throws DecoderException {

        logger.info("METAR data received from message "
                + getHeader().getWmoHeader());
        return null;
    }

}
