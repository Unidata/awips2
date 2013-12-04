/**
 * This software was modified from Raytheon's airep plugin by
 * NOAA/NWS/NCEP/NCO to order to output point data in HDF5.
 **/
package gov.noaa.nws.ncep.edex.plugin.pirep;

import gov.noaa.nws.ncep.common.dataplugin.pirep.PirepLayerData;
import gov.noaa.nws.ncep.common.dataplugin.pirep.PirepRecord;
import gov.noaa.nws.ncep.edex.plugin.pirep.decoder.PirepParser;

import java.util.Calendar;
import java.util.List;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
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
 * Apr 28, 2011            F.J.Yen     Initial creation from pirep.
 * Aug 30, 2011 286        qzhou       Fixed report time
 * Sep 26, 2011 286        qzhou       Changed reportType from int to string
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Sep 05, 2013 2316       bsteffen    Unify pirep and ncpirep.
 * </pre>
 * 
 * @author F. J. Yen
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
            System.out.println("****" + input.getReport());

            PirepRecord report = populateRecord(
                    new PirepParser(input.getReport(), traceId),
                    input.getWmoHeader());

            if (report != null) {
                report.setTraceId(traceId);
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
            // If there is no observation time or location, don't bother going
            // further.

            // parser.getObservationTime() get the current day of month. Need to
            // set wmoHeader day.
            Calendar oTime = parser.getObservationTime();
            if (oTime != null) {
                // try {
                // oTime.set(Calendar.DAY_OF_MONTH, day);
                // // oTime.set(Calendar.MONTH, month);
                // // oTime.set(Calendar.YEAR, year);
                // }
                // catch (ArrayIndexOutOfBoundsException e) {
                // }
                // }

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
                    location.setStationId(parser.getReportingStationId());

                    record.setReportData(parser.getReportData());
                    record.setReportType("PIREP"); // parser.getReportType());

                    location.setFlightLevel(parser.getFlightLevel());
                    record.setAircraftType(parser.getAircraftType());

                    record.setTemp(parser.getTemperature());

                    record.setWindDirection(parser.getWindDirection());
                    record.setWindSpeed(parser.getWindSpeed());

                    record.setLocation(location);

                    record.setHorzVisibility(parser.getHorzVisibility());
                    record.setSuspectTimeFlag(parser.getSuspectTimeFlag());

                    // Collect the decoded icing flight conditions data
                    List<AircraftFlightCondition> icing = parser
                            .getIcingLayers();
                    if (icing != null) {
                        for (AircraftFlightCondition layer : icing) {

                            PirepLayerData iceLayer = PirepLayerData
                                    .getIceLayerData(layer);
                            if (iceLayer != null) {

                                record.addLayer(iceLayer);
                            }
                        } // for
                    }
                    // Collect the decoded turbulence flight conditions data
                    List<AircraftFlightCondition> turbc = parser
                            .getTurbulenceLayers();
                    if (turbc != null) {
                        for (AircraftFlightCondition layer : turbc) {
                            PirepLayerData turbLayer = PirepLayerData
                                    .getTurbLayerData(layer);

                            if (turbLayer != null) {

                                record.addLayer(turbLayer);
                                // record.getAncPirepData().
                            }
                        } // for
                    }

                    List<AircraftCloudLayer> clouds = parser.getCloudLayers();
                    if (clouds != null) {
                        for (AircraftCloudLayer layer : clouds) {
                            PirepLayerData cloudLayer = PirepLayerData
                                    .getCloudLayerData(layer);

                            if (cloudLayer != null) {
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
            for (int j = 0; j < (traceIdx + 1); j++) {
                newTrace[j] = trace[j];
            }
            e.setStackTrace(newTrace);
        }
        return e;
    }
}
