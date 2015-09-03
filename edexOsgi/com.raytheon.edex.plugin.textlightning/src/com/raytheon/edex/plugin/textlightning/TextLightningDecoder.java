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
package com.raytheon.edex.plugin.textlightning;

import java.util.ArrayList;
import java.util.Calendar;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.IBinaryDecoder;
import com.raytheon.edex.plugin.textlightning.impl.TextLightningParser;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LightningStrikePoint;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Decoder for text lightning data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2010            jsanchez    Initial creation
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Feb 12, 2014 2655       njensen     Set source
 * Jun 05, 2014 3226       bclement    LightningStikePoint refactor
 * Jun 10, 2014 3226       bclement    fixed source
 * Jun 22, 2015            mjames@ucar Muted setSource
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class TextLightningDecoder extends AbstractDecoder implements
        IBinaryDecoder {

    private String traceId = null;

    /*
     * inferred from Wufeng Zhou comment in BinLightningDecoderUtil, stands for
     * World Wide Lightning Location Network
     */
    private static final String SOURCE = "WWLLN";

    /**
     * Construct a TextLightning decoder. Calling hasNext() after construction
     * will return false, decode() will return a null.
     */
    public TextLightningDecoder() {

    }

    /**
     * Get the next decoded data record.
     * 
     * @return One record of decoded data.
     * @throws DecoderException
     *             Thrown if no data is available.
     */
    @Override
    public PluginDataObject[] decode(byte[] data) throws DecoderException {
        ArrayList<LightningStrikePoint> strikes = new ArrayList<LightningStrikePoint>();
        TextLightningParser parser = new TextLightningParser(data);

        LightningStrikePoint token;
        while (parser.hasNext()) {
            token = parser.next();
            if (token != null) {
                strikes.add(token);
            }
        }
        BinLightningRecord report = null;

        if (strikes.size() > 0) {
            report = new BinLightningRecord(strikes);
        } else {
            return new PluginDataObject[0];
        }

        Calendar c = TimeUtil.newGmtCalendar();
        report.setInsertTime(c);

        report.setTraceId(traceId);

        //report.setSource(SOURCE);

        return new PluginDataObject[] { report };
    }

    /**
     * Set a trace identifier for the source data.
     * 
     * @param traceId
     *            A unique identifier associated with the input data.
     */
    public void setTraceId(String traceId) {
        this.traceId = traceId;
    }

}
