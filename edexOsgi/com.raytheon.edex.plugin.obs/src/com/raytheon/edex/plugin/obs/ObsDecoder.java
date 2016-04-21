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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.obs.metar.MetarDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.wmo.WMOHeader;

/**
 * Decoder implementation for observation data types. This class provides a
 * wrapper in order to select the correct decoder based on the data type
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * Apr 27, 2007 199         bphillip    Initial creation
 * Jul 31, 2007 411         jkorman     Added addition logging
 * Aug 10, 2007 379         jkorman     Added disposal behavior.
 * Dec 17, 2007 453         jkorman     Added code to check for duplicate obs.
 * Mar 14, 2008 995         jkorman     Changed setDecoderStrategy to check for
 *                                      empty data.
 * Apr 08, 2008 1039        jkorman     Added traceId for tracing data.  
 * Mar 19, 2013 1785        bgonzale    Added performance status handler and added
 *                                      status to decode.
 * Aug 30, 2013 2298        rjpeter     Make getPluginName abstract
 * Oct 23, 2013 2361        njensen     Removed dead mesowest code
 * May 14, 2014 2536        bclement    moved WMO Header to common
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class ObsDecoder extends AbstractDecoder {
    /** The logger */
    private final Log logger = LogFactory.getLog(getClass());

    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("Obs:");

    private String traceId = null;

    /**
     * 
     * @return A decoded data record.
     * @throws DecoderException
     *             if no data is available or an error occured in the decode
     *             method.
     */
    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {

        traceId = getTraceId(headers);

        MetarDecoder decoder = getDecoderStrategy(data, headers);
        PluginDataObject[] reports = null;
        try {

            if (decoder != null) {
                ITimer timer = TimeUtil.getTimer();
                timer.start();
                reports = decoder.decode(data, headers);

                if (reports != null) {
                    for (PluginDataObject report : reports) {
                        report.setTraceId(traceId);
                    }
                }
                timer.stop();
                perfLog.logDuration("Time to Decode", timer.getElapsedTime());
            }
        } catch (Exception e) {
            logger.error(traceId + "- Error in ObsDecoder", e);
        } finally {
            if (reports == null) {
                reports = new PluginDataObject[0];
            }
        }

        return reports;
    }

    /**
     * Examine the wmo header and determine what type of data this is. If found
     * create an appropriate decoder strategy for the data.
     * 
     * @param messageData
     *            The data to be decoded.
     * @return the decoder to use
     * @throws DecoderException
     *             An error occurred while attempting to create a decoder or
     *             setting the data.
     */
    private MetarDecoder getDecoderStrategy(byte[] messageData, Headers headers)
            throws DecoderException {
        // We can never be sure when this method is called so make sure
        // to null out the decoder strategy.
        MetarDecoder decoder = null;
        String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
        WMOHeader header = new WMOHeader(messageData, fileName);
        if (header.isValid()) {
            if ('S' == header.getT1()) {
                switch (header.getT2()) {
                case 'A': {
                    decoder = new MetarDecoder();
                    decoder.setTraceId(traceId);
                    break;
                }
                case 'P': {
                    decoder = new MetarDecoder();
                    decoder.setTraceId(traceId);
                    break;
                }
                }
            } else {
                logger.error(traceId + "- Attempt to create non-METAR Decoder");
            }
        } else {
            logger.error(traceId
                    + " - Unable to find appropriate obs decoder:Data Length="
                    + messageData.length);
        }

        return decoder;
    }

    /**
     * 
     * @param hdrMap
     * @return
     */
    String getTraceId(Headers headers) {
        String traceId = null;
        if (headers != null) {
            Object o = headers.get("CamelFileName");
            if (o != null) {
                traceId = (String) o;
            }
        }
        return traceId;
    }

}
