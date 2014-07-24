/**
 * This software was modified from Raytheon's airep plugin by
 * NOAA/NWS/NCEP/NCO to order to output point data in HDF5.
 **/
package gov.noaa.nws.ncep.edex.plugin.airep;

import gov.noaa.nws.ncep.common.dataplugin.airep.AirepRecord;
import gov.noaa.nws.ncep.edex.plugin.airep.decoder.AIREPWeather;
import gov.noaa.nws.ncep.edex.plugin.airep.decoder.AirepParser;

import java.util.Calendar;
import java.util.Map;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.pointdata.spatial.AircraftObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * Decoder strategy for Aicraft Report (AIREP) observation data. Most common
 * usage is as follows. <code>
 *   AirepDecoder dec = new AirepDecoder();
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
 * Apr 27, 2011            F.J.Yen     Initial creation from airep.
 * Sep 19, 2011 286        Q.Zhou      Modified populateRecord to add 8 new
 *                                     fields for TB, IC and SK.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Sep 05, 2013 2316       bsteffen    Unify airep and ncairep.
 * May 14, 2014 2536       bclement    moved WMO Header to common, removed PLUGIN_NAME
 * Jul 23, 2014 3410       bclement    location changed to floats
 * </pre>
 * 
 * @author F. J. Yen
 * @version 1.0
 */
public class AirepDecoder extends AbstractDecoder {

    public static class AirepDecoderInput {
        public WMOHeader wmoHeader;

        public String report;
    }

    /**
     * @param pluginName
     *            Name that identifies this decoder.
     * @throws DecoderException
     */
    @Deprecated
    public AirepDecoder(String pluginName) throws DecoderException {
    }

    /**
     * 
     */
    public AirepDecoder() {

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
    public PluginDataObject[] decode(AirepDecoderInput input)
            throws DecoderException {

        PluginDataObject[] reports = null;

        AirepRecord report = null;
        String traceId = null;
        System.out.println("====" + new String(input.report)); // input.report
                                                               // );

        try {
            // traceId = getTraceId(hdrMap);
            logger.debug(traceId + "- AirepDecoder.decode()");

            report = populateRecord(new AirepParser(input.report));

            if (report != null) {
                report.setTraceId(traceId);

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
                location.setLatitude(parser.getLatitude().floatValue());
                location.setLongitude(parser.getLongitude().floatValue());
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

                // record.setWeatherCondition(parser.getWeatherCondition());
                record.setTurbInten(parser.getTurbInten());
                record.setTurbType(parser.getTurbType());
                record.setTurbFreq(parser.getTurbFreq());
                record.setIceInten(parser.getIceInten());
                record.setIceType(parser.getIceType());
                record.setSkyCover(parser.getSkyCover());
                record.setSkyBaseHeight(parser.getSkyBaseHeight());
                record.setSkyTopHeight(parser.getSkyTopHeight());
                record.setSuspectTimeFlag(parser.getSuspectTimeFlag());

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
