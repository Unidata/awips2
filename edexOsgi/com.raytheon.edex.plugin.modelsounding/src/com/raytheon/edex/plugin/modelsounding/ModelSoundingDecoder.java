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
package com.raytheon.edex.plugin.modelsounding;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.modelsounding.common.SoundingSite;
import com.raytheon.edex.plugin.modelsounding.decoder.ModelSoundingDataAdapter;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDocument;
import com.raytheon.uf.edex.decodertools.bufr.BUFRFile;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.DefaultDescriptorDelegate;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactoryDelegate;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactorySelector;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * Perform decode on BUFR model sounding data. Currently this decoder does not
 * check for valid time nor the station location, making the assumption that the
 * internal data is good. This decoder does check for possible duplicate data in
 * the database prior to returning the decoded data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080303           1026 jkorman     Initial implementation.
 * 20080408           1039 jkorman     Added traceId for tracing data.
 * 11/25/08          #1684 chammack    Camel Refactor
 * 04/29/13          #1861 bkowal      Create a separate Point Data Container for
 *                                     every record so each forecast hour will
 *                                     receive a unique hdf5 file.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class ModelSoundingDecoder extends AbstractDecoder implements
        IDescriptorFactorySelector {

    public static final String SPI_FILE = "basemaps/modelBufr.spi";

    // Name of the plugin controlling this decoder.
    public static final String PLUGIN_NAME = "modelsounding";

    /** The logger */
    private Log logger = LogFactory.getLog(getClass());

    private PointDataDescription pdd;

    // If failSafe is set to true, this plugin sinks the data but
    // does no processing!
    private boolean failSafe = false;

    private IDescriptorFactoryDelegate delegate;

    /**
     * Construct a ProfilerDecoder instance.
     */
    public ModelSoundingDecoder() {
        try {
            pdd = PointDataDescription.fromStream(this.getClass()
                    .getResourceAsStream("/res/pointdata/modelsounding.xml"));

            logger.info("PointDataDescription loaded");

            delegate = new DefaultDescriptorDelegate(this);
        } catch (Exception e) {
            logger.error("PointDataDescription failed", e);
            logger.error("Plugin set to failSafe mode");
            failSafe = true;
        }
    }

    /**
     * Get one entry from the separator and interpret that data as a single
     * profiler observation.
     * 
     * @return A single decoded observation. May return null if the observation
     *         is a duplicate in the database or is invalid.
     */
    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {
        String traceId = "";
        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }

        if (failSafe) {
            return new PluginDataObject[0];
        }

        PluginDataObject[] decodedData = null;

        logger.debug(traceId + "- Starting decode process");
        if (data != null && data.length > 0) {

            WMOHeader wmoHeader = new WMOHeader(data, headers);

            if ((wmoHeader != null) && (wmoHeader.isValid())) {
                try {

                    Set<String> dataSet = new HashSet<String>();

                    int start = wmoHeader.getMessageDataStart();
                    int len = data.length - start;

                    byte[] messageData = new byte[len];
                    System.arraycopy(data, start, messageData, 0, len);
                    data = null;
                    logger.debug(traceId + "- Starting BUFR data decode");
                    List<BUFRDataDocument> document = decodeBUFRData(
                            messageData, delegate);
                    logger.debug(traceId + "- BUFR data decode complete");
                    messageData = null;

                    Iterator<BUFRDataDocument> iterator = document.iterator();
                    List<SoundingSite> pdoList = new ArrayList<SoundingSite>();

                    while (iterator.hasNext()) {
                        /*
                         * Would it be better to cache the Point Data Container
                         * based on reftime and forecast hour?
                         */
                        PointDataContainer container = PointDataContainer
                                .build(pdd);
                        SoundingSite soundingData = ModelSoundingDataAdapter
                                .createSoundingData(iterator, wmoHeader,
                                        container);
                        if (soundingData != null) {
                            soundingData.setTraceId(traceId);
                            soundingData.setPluginName(PLUGIN_NAME);
                            try {
                                soundingData.constructDataURI();
                            } catch (PluginException e) {
                                logger.error(traceId
                                        + "- Unable to construct dataURI", e);
                            }
                            String uri = soundingData.getDataURI();
                            if (dataSet.add(uri)) {
                                pdoList.add(soundingData);
                            }
                        }
                    }
                    decodedData = pdoList.toArray(new PluginDataObject[pdoList
                            .size()]);
                } catch (Exception ee) {
                    logger.error(traceId + "- Decoder error", ee);
                } finally {
                    if (decodedData == null) {
                        decodedData = new PluginDataObject[0];
                    }
                }
            } else {
                logger.error(traceId + "- Missing or invalid WMOHeader");
                decodedData = new PluginDataObject[0];
            }
        } else {
            logger.info(traceId + "- No data in file");
            decodedData = new PluginDataObject[0];
        }
        logger.debug(traceId + "- ModelSounding decode complete");

        return decodedData;
    }

    /**
     * Decode the entire physical file.
     * 
     * @param messageData
     *            The data message to decode.
     */
    @SuppressWarnings("unchecked")
    private List<BUFRDataDocument> decodeBUFRData(byte[] messageData,
            IDescriptorFactoryDelegate delegate) {
        BUFRFile bFile = new BUFRFile(messageData, delegate);
        List<BUFRDataDocument> reportData = new ArrayList<BUFRDataDocument>();

        for (BUFRDocument doc : bFile) {
            BUFRDataDocument docData = doc.execute();

            List<IBUFRDataPacket> data = docData.getList();
            for (IBUFRDataPacket packet : data) {
                if (packet != null) {
                    if (packet instanceof BUFRSublistPacket) {
                        List<IBUFRDataPacket> p = (List<IBUFRDataPacket>) packet
                                .getValue();
                        BUFRDataDocument dd = new BUFRDataDocument(p);
                        dd.setEnclosingDocument(docData.getEnclosingDocument());
                        reportData.add(dd);
                        logger.debug("Number of packet subsets = "
                                + data.size());
                    }
                }
            }
        }
        return reportData;
    }

    /**
     * Get the selector associated with this separators data. ProfilerSeparator
     * always returns "DEFAULT".
     * 
     * @return Return the string value "DEFAULT".
     * @see com.raytheon.edex.tools.bufr.descriptors.IDescriptorFactorySelector#getSelector()
     */
    @Override
    public String getSelector() {
        return "DEFAULT";
    }

}
