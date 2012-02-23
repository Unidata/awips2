/**
 * SgwhDecoder
 *
 * This java class decodes SGWH (Significant Wave Height or IGDR - Interim Geophysical Data Record)
 * BUFR data.
 *
 ** <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * -------		-------		--------	-----------
 * 08/18/2011               Chin Chen   Initial coding from BufrSgwhDecoder
 * </pre>
 *
 * @author Chin J. Chen
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.sgwh.decoder;

import gov.noaa.nws.ncep.common.dataplugin.sgwh.SgwhRecord;
import gov.noaa.nws.ncep.edex.plugin.sgwh.util.SgwhParser;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.IBinaryDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;

public class SgwhDecoder extends AbstractDecoder implements IBinaryDecoder {
    private static String pluginName;

    private Log logger = LogFactory.getLog(getClass());

    /**
     * Empty constructor required by DecoderFactory.
     * 
     * @throws DecoderException
     */
    public SgwhDecoder(String name) throws DecoderException {
        pluginName = name;
    }

    /**
     * Decode the next SGWH BUFR record.
     * 
     * @return One record of decoded data.
     * @throws DecoderException
     *             Thrown if no data is available.
     */
    public PluginDataObject[] decode(byte[] bufrData) throws DecoderException {
        SgwhRecord sgwhRec;
        SgwhSeparator sep = new SgwhSeparator();

        try {
            if (bufrData != null) {
                sep.setData(bufrData, null);
            } else {
                logger.info("No data to decode while creating BufrSgwhSeparator");
            }
        } catch (Exception e) {
            // need to catch any number of exceptions. Report them here
            // instead of letting them kill the PluginDataProxy.
            if (sep != null) {
                logger.error("Error in " + sep.getWmoHeader().getWmoHeader());
            }
            logger.error("Exception while creating BufrSgwhSeparator", e);
        }

        /*
         * Process BUFR SGWH and add to the database.
         */
        List<PluginDataObject> pdoList = new ArrayList<PluginDataObject>();
        int subsetNum = 0;
        while (sep.hasNext()) {
            subsetNum++;
            sgwhRec = SgwhParser.processSgwh(sep, subsetNum);
            sgwhRec.setWmoHeader(sep.getWmoHeader().getWmoHeader());
            if (sgwhRec != null) {
                try {
                    sgwhRec.setPluginName(pluginName);
                    sgwhRec.constructDataURI();
                    sgwhRec.setReportType("BUFRSGWH");
                } catch (PluginException e) {
                    logger.error("Error constructing dataURI", e);
                }
            }
            if (!pdoList.contains(sgwhRec)) {
                pdoList.add(sgwhRec);
            }
        }
        return pdoList.toArray(new PluginDataObject[pdoList.size()]);
    }
}
