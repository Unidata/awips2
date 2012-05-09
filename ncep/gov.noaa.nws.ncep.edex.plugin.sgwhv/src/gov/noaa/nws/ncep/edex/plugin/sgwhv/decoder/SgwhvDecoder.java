/**
 * SgwhvDecoder
 *
 * This java class decodes SGWHV (Significant Wave Height from Various satellites) BUFR data
 * from NAVOCEANO including Env and Jason 2. 
 *
 ** <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * -------		-------		--------	-----------
 * 08/23/11					Chin J Chen   Initial coding from BufrSgwhvDecoder
 * </pre>
 *
 * @author Chin J. Chen
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.sgwhv.decoder;

import gov.noaa.nws.ncep.common.dataplugin.sgwhv.SgwhvRecord;
import gov.noaa.nws.ncep.edex.plugin.sgwhv.util.SgwhvParser;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.IBinaryDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;

public class SgwhvDecoder extends AbstractDecoder implements IBinaryDecoder {
    private static String pluginName;

    private Log logger = LogFactory.getLog(getClass());

    /**
     * Empty constructor required by DecoderFactory.
     * 
     * @throws DecoderException
     */
    public SgwhvDecoder(String name) throws DecoderException {
        pluginName = name;
    }

    /**
     * Decode the next SGWHV BUFR record.
     * 
     * @return One record of decoded data.
     * @throws DecoderException
     *             Thrown if no data is available.
     */
    public PluginDataObject[] decode(byte[] bufrData) throws DecoderException {
        SgwhvRecord sgwhvRec;
        SgwhvSeparator sep = new SgwhvSeparator();

        try {
            if (bufrData != null) {
                sep.setData(bufrData, null);
            } else {
                logger.info("No data to decode while creating SgwhvSeparator");
            }
        } catch (Exception e) {
            // need to catch any number of exceptions. Report them here
            // instead of letting them kill the PluginDataProxy.
            if (sep != null) {
                logger.error("Error in " + sep.getWmoHeader().getWmoHeader());
            }
            logger.error("Exception while creating SgwhvSeparator", e);
        }

        /*
         * Process BUFR SGWHV and add to the database.
         */
        List<PluginDataObject> pdoList = new ArrayList<PluginDataObject>();
        int subsetNum = 0;
        while (sep.hasNext()) {
            subsetNum++;
            sgwhvRec = SgwhvParser.processSgwhv(sep, subsetNum);
            if (sgwhvRec != null) {
                try {
                    sgwhvRec.setPluginName(pluginName);
                    sgwhvRec.constructDataURI();
                    sgwhvRec.setReportType("SGWHV");
                } catch (PluginException e) {
                    logger.error("Error constructing dataURI", e);
                }
            }
            if (!pdoList.contains(sgwhvRec)) {
                pdoList.add(sgwhvRec);
            }
        }
        logger.info("Processed " + subsetNum + " subsets");
        return pdoList.toArray(new PluginDataObject[pdoList.size()]);
    }
}
