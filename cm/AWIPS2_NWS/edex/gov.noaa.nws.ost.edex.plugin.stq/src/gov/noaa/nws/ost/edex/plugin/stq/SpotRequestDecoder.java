package gov.noaa.nws.ost.edex.plugin.stq;

import gov.noaa.nws.ost.dataplugin.stq.SpotRequestRecord;

import java.io.File;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import gov.noaa.nws.ost.edex.plugin.stq.SpotRequestRecordDAO;


/**
 * AWIPS decoder adapter strategy for Spot Forecast Request data.<br/>
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * July 23, 2015 17366     pwang       Initial implementation of SpotRequestDecoder
 * Oct  30, 2015 17366     pwang       Added error handling for DAO, PDD creation
 * 
 * </pre>
 * 
 * @author pwang
 * @version 1.0
 */
public class SpotRequestDecoder {

    private IUFStatusHandler logger = UFStatus
            .getHandler(SpotRequestDecoder.class);
    private static final String STQ_PDD_RES_FILE = "/res/pointdata/stq.xml";

    private final String pluginName;

    private PointDataDescription pdd = null;

    private SpotRequestRecordDAO dao = null;


    /**
     * Construct a Spot Request decoder
     */
    public SpotRequestDecoder(String pluginName) {

        this.pluginName = pluginName;
    }

    /**
     * Get the next decoded data record.
     * 
     * @return One record of decoded data.
     * @throws DecoderException
     *             Thrown if no data is available.
     */
    public PluginDataObject[] decodeFile(File inputFile, Headers headers)
            throws DecoderException {
        logger.debug("STQ Decoder: start decode " + inputFile.getName());
        SpotRequestRecord[] records = new SpotRequestRecord[1];

        this.createPointDataDescription();
        this.createDAO();

        SpotRequestParser srp = new SpotRequestParser(inputFile, pdd, dao);
        records[0] = srp.parse();
        if (records.length > 0) {
            SpotRequestRecord r = records[0];
            logger.debug("STQ Record: dataURI = " + r.getDataURI());
        }

        return records;
    }

    /**
     * Create DAO Object
     * @throws DecoderException
     */
    protected void createDAO() 
            throws DecoderException {

        try {
            if(null == dao) {
                dao = new SpotRequestRecordDAO(pluginName);
            }
        } catch (Exception e) {
            logger.error("SpotRequestRecordDAO object creation failed", e);
            throw new DecoderException("STQ Decoder: create SpotRequestRecordDAO object creation failed", e);
        }
    }

    /**
     * Create PointDataDescription
     * @throws DecoderException
     */
    protected void createPointDataDescription() 
            throws DecoderException {
        try {
            if(null == pdd) {
                pdd = PointDataDescription.fromStream(this.getClass()
                    .getResourceAsStream(STQ_PDD_RES_FILE));
            }

        } catch (Exception e) {
            logger.error("STQ PointDataDescription object failed", e);
            throw new DecoderException("STQ Decoder: create PointDataDescription object creation failed", e);
        }

    }

}
