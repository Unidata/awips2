/**
 * This software was modified from Raytheon's taf plugin by
 * NOAA/NWS/NCEP/NCO in order to output point data in HDF5.
 **/

package gov.noaa.nws.ncep.edex.plugin.nctaf.decoder;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;

import gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafRecord;
import gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafBulletinRecord;
import gov.noaa.nws.ncep.edex.plugin.nctaf.decoder.NcTafParser;
import gov.noaa.nws.ncep.edex.plugin.nctaf.decoder.NcTafParts;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * 
 * Decoder implementation for nctaf plugin
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/09/2011   458		    sgurung	    Initial Creation from Raytheon's taf plugin
 * 09/23/2011   458			sgurung	    Converted to HDF5
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class NcTafDecoder extends AbstractDecoder {
    // Name of the plugin controlling this decoder.
    public static final String PLUGIN_NAME = "nctaf";

    private String traceId = "";

    /**
     * Constructor.
     * 
     * @param productType
     * @throws DecoderException
     */
    public NcTafDecoder() {

    }

    public static class NcTafDecoderInput {
        public WMOHeader wmoHeader;

        public NcTafParts tafParts;
    }
   
    /**
     * 
     * @param input
     * @return
     * @throws DecoderException
     */
     public PluginDataObject[] decode(NcTafDecoderInput input)
		    throws DecoderException {
		
		NcTafBulletinRecord bulletinRec = null;
		
		NcTafParser parser = null;
		try {
		    parser = new NcTafParser(input.tafParts, input.wmoHeader);		
		    bulletinRec = parser.getDecodedRecord();		
		} catch (DecoderException de) {
		    logger.info(traceId + " -" + de.getMessage());
		    bulletinRec = null;
		} catch (Exception e) {
		    e.printStackTrace();
		    logger.error(traceId, e);
		    bulletinRec = null;
		}
		
		if (bulletinRec == null)
		    return new PluginDataObject[0];
		
		// split the record into multiple pdos
		NcTafRecord[] pdos = NcTafRecord.split(bulletinRec);
		
		//  set the fields that must be done on a per-PDO basis
    	for (NcTafRecord pdo : pdos) {
        		pdo.setTraceId(traceId);
        		pdo.setPluginName("nctaf");
        		try {
        			pdo.constructDataURI();
        		} catch (PluginException e) {
        			throw new DecoderException("Error constructing dataURI", e);
        		}
    	}
		
		return pdos; 
	}    
   
}
