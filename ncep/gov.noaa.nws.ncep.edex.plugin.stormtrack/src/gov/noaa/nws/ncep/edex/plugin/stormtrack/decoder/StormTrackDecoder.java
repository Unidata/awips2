/**
 *
 * StormTrackDecoder
 * 
 * Decoder Plug-In for StormTrack (Automatic Tropical Cyclone Forecast & Ensemble cyclones).
 * 
 * 12 December 2008
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 */

package gov.noaa.nws.ncep.edex.plugin.stormtrack.decoder;

import gov.noaa.nws.ncep.common.dataplugin.stormtrack.StormTrackRecord;
import gov.noaa.nws.ncep.edex.plugin.stormtrack.util.StormTrackParser;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

import java.util.ArrayList;
import java.util.List;
import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;

/**
 * 
 * StormTrackDecoder
 * 
 * Decoder implementation for StormTrack Plug-In
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 08/2011					T. Lee		ATCF and Ensemble storm tracks	
 * 06/2012       #606       G. Hull     constructDataURI() after setReportType so it gets into the URI	
 * 07/2013				    T. Lee		Improved performance via batch processing	
 * 
 * </pre>
 * 
 * @author tlee
 * @version 1
 * 
 */

public class StormTrackDecoder extends AbstractDecoder {
    // Name of the plugin controlling this decoder.
    public final String pluginName;

    protected Matcher regexMatcher;

    protected Pattern regexPattern;

    public Calendar observTime = null;

    /**
     * Constructor
     * 
     * @throws DecoderException
     */
    public StormTrackDecoder(String name) throws DecoderException {
        pluginName = name;
    }

    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {
        String traceId = "";
        byte[] messageData = null;
        // STORMTRACK_DATA is REGEX for a StormTrack record
        final String STORMTRACK_DATA = IDecoderConstantsN.STORM_BULLSEPARATOR;
        final Pattern stormTrackPattern = Pattern.compile(STORMTRACK_DATA);

        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }

        /*
         * Check if there are more records
         */
        StormTrackRecord record = null;
        List <StormTrackRecord> records =  new ArrayList <StormTrackRecord>();
        StormTrackSeparator sep = StormTrackSeparator.separate(data, headers);
        
        while (sep.hasNext()) {
        messageData = sep.next();
        String theMessage = new String(messageData);

        try {
        		Matcher stormTrackMatcher = stormTrackPattern.matcher(theMessage);

            if (stormTrackMatcher.find()) {
       
            } else {
                System.out.println("StormTrack WARNING:  Ignored invalid record:  "
        					+ theMessage);
            }
        } catch (Exception e) {
            System.out.println("StormTrack WARNING exception:  Unable to decode:  "
        				+ theMessage);
            e.printStackTrace();
        }

        /*
         * Process the StormTrack fields
         */
        	record = StormTrackParser.processFields(theMessage);

        /*
         * Check the StormTrack record object
         */
        if (record != null) {
            try {
            	record.setTraceId(traceId);
            	record.setPluginName(pluginName);

            	/*
            	 * Set report type in record.
            	 */
            	if (theMessage.contains("FOF\n") || theMessage.contains("TCV\n")) {
            		record.setReportType("ENSCYC");
            	} else {
            		record.setReportType("ATCF");
            	}                             

            	record.constructDataURI();
        			if ( record.getClat() != IDecoderConstantsN.FLOAT_MISSING &&
        					record.getClon() != IDecoderConstantsN.FLOAT_MISSING ) {
        				records.add(record);
        			}

            } catch (PluginException e) {
            	throw new DecoderException(
            			"StormTrack WARNING:  Unable to construct dataURI--exception:  ",
            			e);
            }
        }
        }
        
        /*
         * Return StormTrack record object if not null
         */
        if (record == null) {
            return new PluginDataObject[0];
        } else {
            return records.toArray (new PluginDataObject[records.size()]);
        }
    }
}
