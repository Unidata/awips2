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
package com.raytheon.uf.edex.plugin.mesowest;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.decodertools.core.IDecoderInput;
import com.raytheon.uf.edex.plugin.mesowest.common.MESOWestRecord;
import com.raytheon.uf.edex.plugin.mesowest.decoder.MESOWestConstants;
import com.raytheon.uf.edex.plugin.mesowest.decoder.MESOWestParser;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 3, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class MESOWestDecoder {

    private Log logger = LogFactory.getLog(getClass());

    private String pluginName = "mesowest";

    private Map<String,MESOWestParser> parserMap = new HashMap<String,MESOWestParser>();
    
    /**
     * 
     * @param name
     */
    public MESOWestDecoder(String name) {
        this.pluginName = name;
    }

    /**
     * Get the next decoded data record.
     * 
     * @return One record of decoded data.
     * @throws DecoderException
     *             Thrown if no data is available.
     */
    public PluginDataObject[] decode(IDecoderInput input) {

        PluginDataObject[] decodedData = null;
        String traceId = null;
        
        logger.debug("MESOWestDecoder.decode()");
        
        if(input != null) {
            traceId = (String) input.getProperty(MESOWestConstants.TRACEID);

            MESOWestRecord record = null;
            
            String type = input.getProperty(MESOWestConstants.K_DATATYPE);
            if(MESOWestConstants.T_PARMHEADER.equals(type)) {
                parserMap.put(input.getProperty("uuid"),new MESOWestParser(input.getReport()));
                logger.debug("Created parser ");
            } else if (MESOWestConstants.T_LASTITEM.equals(type)) {
                parserMap.remove(input.getProperty("uuid"));
                logger.debug("Destroyed parser ");
            } else {
                MESOWestParser parser = parserMap.get(input.getProperty("uuid"));
                if(parser != null) {
                    if(input.getReport().length() > 10) {
                        if((record = parser.decode(input.getReport())) != null) {
                            record.setPluginName(pluginName);
                            record.setTraceId(traceId);
                            record.setObsText(input.getReport() + "\n");
                        }
                    }
                } else {
                    logger.error("Unexpected data in data stream");
                }
            }
            
            try {
                if (record != null) {
                    logger.info("Decoded obs " + record.getStationId());
                    
                    try {
                        record.constructDataURI();
                    } catch (PluginException e) {
                        throw new DecoderException("Unable to construct dataURI", e);
                    }

                    decodedData = new PluginDataObject[] { record };
                } else {
                    logger.info(String.format("%s - Decoded no obs", traceId));
                }
            } catch (Exception e) {
                logger.error("Error in MESOWestDecoder", e);
            } finally {
                if(decodedData == null) {
                    decodedData = new PluginDataObject[0];
                }
            }
        } else {
            logger.error("null input data in MESOWestDecoder");
        }

        return decodedData;
    }
    
    /**
     * @return the pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

}
