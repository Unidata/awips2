/**
 * BufrSgwhDecoder
 *
 * This java class decodes SGWH (Significant Wave Height or IGDR - Interim Geophysical Data Record)
 * BUFR data.
 *
 ** <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * -------		-------		--------	-----------
 * 04/21/10     208         F. J. Yen   Initial coding from Tamdar (to11dr3)
 * </pre>
 *
 * @author F. J. Yen
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.bufrsgwh.decoder;

import gov.noaa.nws.ncep.common.dataplugin.bufrsgwh.BufrSgwhRecord;
import gov.noaa.nws.ncep.edex.plugin.bufrsgwh.util.BufrSgwhParser;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.IBinaryDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;

public class BufrSgwhDecoder extends AbstractDecoder implements IBinaryDecoder {
    private static String pluginName;

    private Log logger = LogFactory.getLog(getClass());

    /**
     * Empty constructor required by DecoderFactory.
     * 
     * @throws DecoderException
     */
    public BufrSgwhDecoder(String name) throws DecoderException {
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
        BufrSgwhRecord sgwhRec;
        BufrSgwhSeparator sep = new BufrSgwhSeparator();

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
            sgwhRec = BufrSgwhParser.processBufrSgwh(sep, subsetNum);
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
