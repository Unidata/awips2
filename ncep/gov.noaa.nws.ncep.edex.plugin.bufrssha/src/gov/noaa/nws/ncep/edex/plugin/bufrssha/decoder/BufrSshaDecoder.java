/**
 * BufrSshaDecoder
 *
 * This java class decodes SSHA (Sea Surface Height Anomaly) BUFR data.
 *
 ** <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * -------		-------		--------	-----------
 * 01/26/11     209         F. J. Yen   Initial coding R1G1-6
 * </pre>
 *
 * @author F. J. Yen
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.bufrssha.decoder;

import gov.noaa.nws.ncep.common.dataplugin.bufrssha.BufrSshaRecord;
import gov.noaa.nws.ncep.edex.plugin.bufrssha.util.BufrSshaParser;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.IBinaryDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;

public class BufrSshaDecoder extends AbstractDecoder implements IBinaryDecoder {
    private static String pluginName;

    private Log logger = LogFactory.getLog(getClass());

    /**
     * Empty constructor required by DecoderFactory.
     * 
     * @throws DecoderException
     */
    public BufrSshaDecoder(String name) throws DecoderException {
        pluginName = name;
    }

    /**
     * Decode the next SSHA BUFR record.
     * 
     * @return One record of decoded data.
     * @throws DecoderException
     *             Thrown if no data is available.
     */
    public PluginDataObject[] decode(byte[] bufrData) throws DecoderException {
        BufrSshaRecord sshaRec;
        BufrSshaSeparator sep = new BufrSshaSeparator();

        try {
            if (bufrData != null) {
                sep.setData(bufrData, null);
            } else {
                logger.info("No data to decode while creating BufrSshaSeparator");
            }
        } catch (Exception e) {
            // need to catch any number of exceptions. Report them here
            // instead of letting them kill the PluginDataProxy.
            if (sep != null) {
                logger.error("Error in " + sep);
            }
            logger.error("Exception while creating BufrSshaSeparator", e);
        }

        /*
         * Process BufrSsha raw data and add to the database.
         */
        List<PluginDataObject> pdoList = new ArrayList<PluginDataObject>();
        int subsetNum = 0;
        while (sep.hasNext()) {
            subsetNum++;
            sshaRec = BufrSshaParser.processBufrSsha(sep, subsetNum);
            if (sshaRec != null) {
                try {
                    sshaRec.setPluginName(pluginName);
                    sshaRec.constructDataURI();
                    sshaRec.setReportType("BUFRSSHA");
                } catch (PluginException e) {
                    logger.error("Error constructing dataURI", e);
                }
            }
            if (!pdoList.contains(sshaRec)) {
                pdoList.add(sshaRec);
            }
        }
        logger.info("Processed " + subsetNum + " subsets");
        return pdoList.toArray(new PluginDataObject[pdoList.size()]);
    }
}
