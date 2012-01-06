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
package com.raytheon.edex.plugin.pirep;

import java.util.Calendar;
import java.util.List;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.pirep.decoder.PirepParser;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.pirep.PirepLayerData;
import com.raytheon.uf.common.dataplugin.pirep.PirepRecord;
import com.raytheon.uf.common.pointdata.spatial.AircraftObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.aircraft.AircraftCloudLayer;
import com.raytheon.uf.edex.decodertools.aircraft.AircraftFlightCondition;
import com.raytheon.uf.edex.decodertools.core.BasePoint;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.decodertools.core.IDecoderInput;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * Decoder strategy for text PIREP observation data. Most common usage is as
 * follows. <code>
 *   PirepDecoder dec = new PirepDecoder();
 *   dec.setMessage(msgData);
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
 * 20080128            861 jkorman     Add pirep layer data.
 * 20080408           1039 jkorman     Added traceId for tracing data. 
 * 11/13/08           1684 chammack    Camel Refactor
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class PirepDecoder extends AbstractDecoder {
    // Name of the plugin controlling this decoder.
    private final String PLUGIN_NAME;

    /**
     * Construct this decoder using supplied plugin name.
     * 
     * @throws DecoderException
     */
    public PirepDecoder(String pluginName) throws DecoderException {
        PLUGIN_NAME = pluginName;
    }

    /**
     * Get the next decoded data record.
     * 
     * @param input
     *            the input
     * @return One record of decoded data.
     * @throws DecoderException
     *             Thrown if no data is available.
     */
    public PluginDataObject[] decode(IDecoderInput input, Headers headers)
            throws DecoderException {

        PluginDataObject[] reports = null;

        String traceId = "";
        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }
        try {
            logger.debug(traceId + "- PirepDecoder.decode()");

            PirepRecord report = populateRecord(
                    new PirepParser(input.getReport(), traceId, headers),
                    input.getWmoHeader());

            if (report != null) {
                report.setTraceId(traceId);
                report.setPluginName(PLUGIN_NAME);
                try {
                    report.constructDataURI();
                } catch (PluginException e) {
                    fixTrace(e);
                    logger.error(traceId + "- Unable to construct dataURI", e);
                }
                reports = new PluginDataObject[] { report };
            }

        } catch (Exception e) {
            fixTrace(e);
            logger.error(traceId + "- Error in PirepDecoder", e);
        } finally {
            if (reports == null) {
                reports = new PluginDataObject[0];
            }
        }
        return reports;
    }

    /**
     * Populate a PirepRecord with data that was decoded from a single PIREP
     * report.
     * 
     * @param parser
     *            The PIREP parser that contains the decoded data.
     * @param wmoHeader
     *            the wmo header
     * @return The populated record. This method returns a null reference if
     *         either the observation time or location data is unavailable.
     */
    private PirepRecord populateRecord(PirepParser parser, WMOHeader wmoHeader) {

        PirepRecord record = null;
        AircraftObsLocation location = null;
        if (parser != null) {
            // If there is no observation time or location,
            // don't bother going further.
            Calendar oTime = parser.getObservationTime();
            BasePoint p = parser.getLocation();

            if ((oTime != null) && (p != null)) {
                record = new PirepRecord();
                location = new AircraftObsLocation();

                record.setTimeObs(oTime);
                record.setRefHour(TimeTools.copyToNearestHour(oTime));
                DataTime dataTime = new DataTime(oTime);
                record.setDataTime(dataTime);

                record.setWmoHeader(wmoHeader.getWmoHeader());
                record.setObsText(DecoderTools.normalizeObs(
                        parser.getReportData(), wmoHeader.getWmoHeader()));

                location.setLatitude(p.getLatitude());
                location.setLongitude(p.getLongitude());
                location.setLocation(p.getLatitude(), p.getLongitude());

                record.setReportData(parser.getReportData());
                record.setReportType(parser.getReportType());
                location.setFlightLevel(parser.getFlightLevel());
                record.setAircraftType(parser.getAircraftType());

                record.setTemp(parser.getTemperature());
                record.setWindDirection(parser.getWindDirection());
                record.setWindSpeed(parser.getWindSpeed());

                record.setLocation(location);

                record.setHorzVisibility(parser.getHorxVisibility());

                // Collect the decoded icing flight conditions data
                List<AircraftFlightCondition> icing = parser
                        .getIcingLayers();
                if (icing != null) {
                    PirepLayerData iceLayer = null;
                    for (AircraftFlightCondition layer : icing) {
                        if ((iceLayer = PirepLayerData.getLayerData(layer)) != null) {
                            iceLayer.setLayerType(PirepLayerData.LAYER_TYP_ICING);
                            record.addLayer(iceLayer);
                        }
                    } // for
                }
                // Collect the decoded turbulence flight conditions data
                List<AircraftFlightCondition> turbc = parser
                        .getTurbulenceLayers();
                if (turbc != null) {
                    PirepLayerData turbLayer = null;
                    for (AircraftFlightCondition layer : turbc) {
                        if ((turbLayer = PirepLayerData.getLayerData(layer)) != null) {
                            turbLayer
                                    .setLayerType(PirepLayerData.LAYER_TYP_TURBC);
                            record.addLayer(turbLayer);
                        }
                    } // for
                }

                List<AircraftCloudLayer> clouds = parser.getCloudLayers();
                if (clouds != null) {
                    PirepLayerData cloudLayer = null;
                    for (AircraftCloudLayer layer : clouds) {
                        if ((cloudLayer = PirepLayerData
                                .getCloudLayerData(layer)) != null) {
                            record.addLayer(cloudLayer);
                        }
                    } // for
                }

                String[] weatherCodes = parser.getWeatherCodes();
                if (weatherCodes != null) {
                    StringBuilder sb = new StringBuilder();
                    for (String code : weatherCodes) {
                        sb.append(code);
                        sb.append(" ");
                    }
                    record.setWeatherGroup(sb.toString().trim());
                }
            }
        }
        return record;
    }

    /**
     * 
     * @param e
     * @return
     */
    private Throwable fixTrace(Throwable e) {
        StackTraceElement[] trace = e.getStackTrace();

        StackTraceElement[] newTrace = null;

        int traceIdx = trace.length - 1;
        for (; traceIdx >= 0; traceIdx--) {
            StackTraceElement element = trace[traceIdx];
            if (element.getClassName().indexOf("com.raytheon") >= 0) {
                break;
            }
        }
        if (traceIdx >= 0) {
            newTrace = new StackTraceElement[traceIdx + 1];
            for (int j = 0; j < traceIdx + 1; j++) {
                newTrace[j] = trace[j];
            }
            e.setStackTrace(newTrace);
        }
        return e;
    }

}
