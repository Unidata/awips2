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
package com.raytheon.edex.plugin.airep;

import java.util.Calendar;
import java.util.Map;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.airep.decoder.AIREPWeather;
import com.raytheon.edex.plugin.airep.decoder.AirepParser;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.airep.AirepRecord;
import com.raytheon.uf.common.pointdata.spatial.AircraftObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * Decoder strategy for Aicraft Report (AIREP) observation data. Most common
 * usage is as follows. <code>
 *   AIREPDecoder dec = new AIREPDecoder();
 *   dec.setMessage(messageData);
 *   while(dec.hasNext())
 *   {
 *      PluginDataObject r = dec.decode();
 *      // do something with record.
 *   }
 * </code>
 * 
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080103            384 jkorman     Initial Coding.
 * 20080408           1039 jkorman     Added traceId for tracing data.
 * 11/11/08           1684 chammack    Camel Refactor
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class AirepDecoder extends AbstractDecoder {
    // Name of the plugin controlling this decoder.
    private final String PLUGIN_NAME;

    public static class AirepDecoderInput {
        public WMOHeader wmoHeader;

        public String report;
    }

    /**
     * @param pluginName
     *            Name that identifies this decoder.
     * @throws DecoderException
     */
    public AirepDecoder(String pluginName) throws DecoderException {
        PLUGIN_NAME = pluginName;
    }

    /**
     * Get the next decoded data record.
     * 
     * @param input
     *            the decoder input
     * @return One record of decoded data.
     * @throws DecoderException
     *             Thrown if no data is available.
     */
    public PluginDataObject[] decode(AirepDecoderInput input, Headers header)
            throws DecoderException {

        PluginDataObject[] reports = null;

        AirepRecord report = null;
        String traceId = null;

        try {
            // traceId = getTraceId(hdrMap);
            logger.debug(traceId + "- AirepDecoder.decode()");
            WMOHeader wmoHeader = input.wmoHeader;
            if(wmoHeader != null) {
                Calendar refTime = TimeTools.findDataTime(
                        wmoHeader.getYYGGgg(), header);
                if(refTime != null) {
                    report = populateRecord(new AirepParser(input.report, refTime));
                }
            }
            if (report != null) {
                report.setTraceId(traceId);
                report.setPluginName(PLUGIN_NAME);
                try {
                    report.constructDataURI();
                } catch (PluginException e) {
                    throw new DecoderException("Error constructing dataURI", e);
                }
                reports = new PluginDataObject[] { report };
            }

        } catch (Exception e) {
            logger.error(traceId + "- Error in AirepDecoder", e);
        } finally {
            if (reports == null) {
                reports = new PluginDataObject[0];
            }
        }
        return reports;

    }

    /**
     * @param parser
     *            The reccon parser that contains the decoded data.
     * @return The populated record.
     */
    private AirepRecord populateRecord(AirepParser parser) {

        AirepRecord record = null;
        AircraftObsLocation location = null;
        if (parser != null) {
            // If there is no obstime, don't bother going further.
            Calendar oTime = parser.getObservationTime();
            if (oTime != null) {
                record = new AirepRecord();
                location = new AircraftObsLocation();

                record.setTimeObs(oTime);
                record.setRefHour(TimeTools.copyToNearestHour(oTime));

                DataTime dataTime = new DataTime(oTime);
                record.setDataTime(dataTime);

                record.setReportData(parser.getReportData());
                location.setStationId(parser.getAircraftId());
                record.setReportType(parser.getReportType());
                location.setLatitude(parser.getLatitude());
                location.setLongitude(parser.getLongitude());
                location.setFlightLevel(parser.getFlightLevel());
                location.setLocation(parser.getLatitude(),
                        parser.getLongitude());

                record.setTemp(parser.getTemperature());

                record.setWindDirection(parser.getWindDirection());
                record.setWindSpeed(parser.getWindSpeed());

                record.setLocation(location);

                AIREPWeather wx = parser.getWeatherGroup();
                if (wx != null) {
                    record.setFlightConditions(wx.getFlightConditions());
                    record.setFlightHazard(wx.getHazard());
                    record.setFlightWeather(wx.getWeather());
                }
            }
        }
        return record;
    }

    /**
     * 
     * @param hdrMap
     * @return
     */
    String getTraceId(Map<String, Object> hdrMap) {
        String traceId = null;
        if (hdrMap != null) {
            Object o = hdrMap.get("CamelFileName");
            if (o != null) {
                traceId = (String) o;
            }
        }
        return traceId;
    }

}
