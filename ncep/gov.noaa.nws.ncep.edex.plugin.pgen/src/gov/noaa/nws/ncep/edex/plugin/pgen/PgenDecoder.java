package gov.noaa.nws.ncep.edex.plugin.pgen;

import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;

public class PgenDecoder extends AbstractDecoder {

    private String pluginName;

    /**
     * 
     */
    public PgenDecoder(String name) {
        pluginName = name;
    }

    public PluginDataObject[] decode(byte[] data) {

        logger.info("PGEN decoder is not yet implemented.");
        return new PluginDataObject[0];

        // record.setPluginName(pluginName);
        // record.constructDataURI();

        // return new PluginDataObject[] { record };
    }
}
