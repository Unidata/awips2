/**
 * This software was modified from Raytheon's airep plugin by
 * NOAA/NWS/NCEP/NCO to order to output point data in HDF5.
 **/
package gov.noaa.nws.ncep.edex.plugin.ncairep;

import java.util.Calendar;
import java.util.Map;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.pointdata.spatial.AircraftObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

import gov.noaa.nws.ncep.common.dataplugin.ncairep.NcAirepRecord;
import gov.noaa.nws.ncep.edex.plugin.ncairep.decoder.NcAIREPWeather;
import gov.noaa.nws.ncep.edex.plugin.ncairep.decoder.NcAirepParser;

/**
 * Decoder strategy for Aicraft Report (AIREP) observation data. Most common
 * usage is as follows. <code>
 *   NcAIREPDecoder dec = new NcAIREPDecoder();
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
 * 04/27/2011              F.J.Yen       Initial creation from airep.
 * 09/19/2011    286       Q.Zhou      Modified populateRecord to add 8 new fields for TB, IC and SK.
 * </pre>
 * 
 * @author F. J. Yen
 * @version 1.0
 */
public class NcAirepDecoder extends AbstractDecoder {
    // Name of the plugin controlling this decoder.
    private final String PLUGIN_NAME;

    public static class NcAirepDecoderInput {
        public WMOHeader wmoHeader;

        public String report;
    }

    /**
     * @param pluginName
     *            Name that identifies this decoder.
     * @throws DecoderException
     */
    public NcAirepDecoder(String pluginName) throws DecoderException {
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
    public PluginDataObject[] decode(NcAirepDecoderInput input)
            throws DecoderException {
        
        PluginDataObject[] reports = null;

        NcAirepRecord report = null;
        String traceId = null;
        System.out.println("====" +new String(input.report)); //input.report );
        
        try {
            // traceId = getTraceId(hdrMap);
            logger.debug(traceId + "- NcAirepDecoder.decode()");
            
            report = populateRecord(new NcAirepParser(input.report));                     
            
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
            logger.error(traceId + "- Error in NcAirepDecoder", e);
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
    private NcAirepRecord populateRecord(NcAirepParser parser) {

        NcAirepRecord record = null;
        AircraftObsLocation location = null;
      
        if (parser != null) {
            // If there is no obstime, don't bother going further.
            Calendar oTime = parser.getObservationTime();
            
            if (oTime != null) {
            	
                record = new NcAirepRecord();
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
                location.setLocation(parser.getLatitude(), parser
                        .getLongitude());
                record.setTemp(parser.getTemperature());
                record.setWindDirection(parser.getWindDirection());                
                record.setWindSpeed(parser.getWindSpeed());
                record.setLocation(location);
                
                NcAIREPWeather wx = parser.getWeatherGroup();
                if (wx != null) {
                    record.setFlightConditions(wx.getFlightConditions());
                    record.setFlightHazard(wx.getHazard());
                    record.setFlightWeather(wx.getWeather());
                }
                
                //record.setWeatherCondition(parser.getWeatherCondition());
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
