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
package com.raytheon.uf.edex.plugin.modelsounding;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.modelsounding.SoundingSite;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDocument;
import com.raytheon.uf.edex.decodertools.bufr.BUFRFile;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.DefaultDescriptorDelegate;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactoryDelegate;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactorySelector;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.plugin.modelsounding.decoder.ModelSoundingDataAdapter;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * Perform decode on BUFR model sounding data. Currently this decoder does not
 * check for valid time nor the station location, making the assumption that the
 * internal data is good. This decoder does check for possible duplicate data in
 * the database prior to returning the decoded data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 03, 2008  1026     jkorman     Initial implementation.
 * Apr 08, 2008  1039     jkorman     Added traceId for tracing data.
 * Nov 25, 2008  1684     chammack    Camel Refactor
 * Apr 29, 2013  1861     bkowal      Create a separate Point Data Container
 *                                    for  every record so each forecast hour
 *                                    will  receive a unique hdf5 file.
 * Jul 03, 2013  2161     bkowal      Store and retrieve the Point Data
 *                                    Containers  by forecast hour and reftime
 *                                    when completing  a decode operation.
 *                                    Overrode default  Point Data Container
 *                                    size.
 * Jul 16, 2013  2161     bkowal      Store the records in a container that
 *                                    will  be persisted every X (configurable)
 *                                    seconds  by a timer. The timer is started
 *                                    during spring  initialization and
 *                                    destroyed during spring  container
 *                                    destruction.
 * Aug 30, 2013  2298     rjpeter     Make getPluginName abstract
 * Dec 02, 2013  2537     bsteffen    Remove dead/deprecated code.
 * 
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

    private static final int PDC_SIZE = 20;

    /** The logger */
    private final IUFStatusHandler logger = UFStatus
            .getHandler(ModelSoundingDecoder.class);

    private PointDataDescription pdd;

    // If failSafe is set to true, this plugin sinks the data but
    // does no processing!
    private boolean failSafe = false;

    private IDescriptorFactoryDelegate delegate;

    private ModelSoundingPersistenceManager modelSoundingPersistenceManager;

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

    public void start() {
        this.modelSoundingPersistenceManager.start();
    }

    public void shutdown() {
        this.modelSoundingPersistenceManager.shutdown();
    }

    /**
     * Get one entry from the separator and interpret that data as a single
     * profiler observation.
     * 
     * @return A single decoded observation. May return null if the observation
     *         is a duplicate in the database or is invalid.
     */
    public PluginDataObject[] decode(byte[] data, Headers headers) {
        String traceId = "";
        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }

        if (failSafe) {
            return new PluginDataObject[0];
        }

        if (logger.isPriorityEnabled(Priority.DEBUG)) {
            logger.debug(traceId + "- Starting decode process");
        }

        if ((data != null) && (data.length > 0)) {

            WMOHeader wmoHeader = new WMOHeader(data, headers);

            if (wmoHeader.isValid()) {
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
                    Map<String, ModelSoundingStorageContainer> containerMap = new HashMap<String, ModelSoundingStorageContainer>();

                    while (iterator.hasNext()) {
                        BUFRDataDocument dataDoc = iterator.next();
                        if (dataDoc == null) {
                            continue;
                        }

                        SoundingModelTemporalData soundingTemporalData = ModelSoundingDataAdapter
                                .getSoundingTemporalInformation(dataDoc,
                                        wmoHeader);

                        String pdcKey = (soundingTemporalData == null) ? " NULL"
                                : soundingTemporalData.toString();
                        ModelSoundingStorageContainer container = containerMap
                                .get(pdcKey);
                        if (container == null) {
                            // haven't yet decoded a sounding for the given
                            // refTime/forecasthour, check the persistence
                            // manager for one from a previous decode
                            container = modelSoundingPersistenceManager
                                    .checkOut(pdcKey);

                            if (container == null) {
                                if (logger.isPriorityEnabled(Priority.DEBUG)) {
                                    logger.debug("Creating new Point Data Container: "
                                            + pdcKey);
                                }

                                container = new ModelSoundingStorageContainer(
                                        PointDataContainer.build(pdd, PDC_SIZE));
                            } else if (logger.isPriorityEnabled(Priority.DEBUG)) {
                                logger.debug("Reusing Point Data Container: "
                                        + pdcKey);
                            }

                            containerMap.put(pdcKey, container);
                        }

                        PointDataContainer pdc = container.getPdc();
                        SoundingSite soundingData = ModelSoundingDataAdapter
                                .createSoundingData(dataDoc, wmoHeader, pdc,
                                        soundingTemporalData);
                        if (soundingData != null) {
                            soundingData.setTraceId(traceId);
                            String uri = soundingData.getDataURI();
                            if (dataSet.add(uri)) {
                                container.addPdo(soundingData);
                            }
                        }
                    }

                    for (Map.Entry<String, ModelSoundingStorageContainer> entry : containerMap
                            .entrySet()) {
                        modelSoundingPersistenceManager.checkIn(entry.getKey(),
                                entry.getValue());
                    }
                } catch (Exception ee) {
                    logger.error(traceId + "- Decoder error", ee);
                }
            } else {
                logger.error(traceId + "- Missing or invalid WMOHeader");
            }
        } else {
            logger.info(traceId + "- No data in file");
        }

        if (logger.isPriorityEnabled(Priority.DEBUG)) {
            logger.debug(traceId + "- ModelSounding decode complete");
        }

        return new PluginDataObject[0];
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

    public ModelSoundingPersistenceManager getModelSoundingPersistenceManager() {
        return modelSoundingPersistenceManager;
    }

    public void setModelSoundingPersistenceManager(
            ModelSoundingPersistenceManager modelSoundingPersistenceManager) {
        this.modelSoundingPersistenceManager = modelSoundingPersistenceManager;
    }

}
