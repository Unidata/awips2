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
package com.raytheon.edex.plugin.poessounding;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.poessounding.dao.POESSoundingDAO;
import com.raytheon.edex.plugin.poessounding.decoder.POESSoundingDataAdapter;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.poessounding.POESSounding;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.bufrtools.BUFRDataDocument;
import com.raytheon.uf.edex.bufrtools.BUFRDocument;
import com.raytheon.uf.edex.bufrtools.BUFRFile;
import com.raytheon.uf.edex.bufrtools.descriptors.DefaultDescriptorDelegate;
import com.raytheon.uf.edex.bufrtools.descriptors.IDescriptorFactoryDelegate;
import com.raytheon.uf.edex.bufrtools.descriptors.IDescriptorFactorySelector;
import com.raytheon.uf.edex.bufrtools.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.bufrtools.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

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
 * Mar 03, 2008 1026       jkorman     Initial implementation.
 * Apr 08, 2008 1039       jkorman     Added traceId for tracing data.
 * May 15, 2013 1869       bsteffen    Remove DataURI from goes/poes soundings.
 * Jul 17, 2013 2112       bsteffen    Split poes data so it gets stored in
 *                                     correct file.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * May 14, 2014 2536       bclement    moved WMO Header to common
 * Dec 15, 2015 5166       kbisanz     Update logging to use SLF4J
 * Sep 23, 2021 8608       mapeters    Handle PDO.traceId changes
 *
 * </pre>
 *
 * @author jkorman
 */
public class POESSoundingDecoder extends AbstractDecoder
        implements IDescriptorFactorySelector {

    // Name of the plugin controlling this decoder.
    public static final String PLUGIN_NAME = "poessounding";

    /** The logger */
    private final Logger logger = LoggerFactory.getLogger(getClass());

    private PointDataDescription pdd;

    // If failSafe is set to true, this plugin sinks the data but
    // does no processing!
    private boolean failSafe = false;

    private IDescriptorFactoryDelegate delegate;

    /**
     * Construct a POESSoundingDecoder instance.
     */
    public POESSoundingDecoder() {
        try {
            pdd = PointDataDescription.fromStream(this.getClass()
                    .getResourceAsStream("/res/pointdata/poes.xml"));
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

        if ((data != null) && (data.length > 0)) {
            String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
            WMOHeader wmoHeader = new WMOHeader(data, fileName);

            if ((wmoHeader != null) && (wmoHeader.isValid())) {
                try {
                    int start = wmoHeader.getMessageDataStart();
                    int len = data.length - start;

                    byte[] messageData = new byte[len];
                    System.arraycopy(data, start, messageData, 0, len);

                    List<BUFRDataDocument> document = decodeBUFRData(
                            messageData, delegate);
                    Iterator<BUFRDataDocument> iterator = document.iterator();
                    List<POESSounding> pdoList = new ArrayList<>();

                    Map<File, PointDataContainer> container = new HashMap<>();

                    POESSoundingDAO dao = (POESSoundingDAO) PluginFactory
                            .getInstance().getPluginDao(PLUGIN_NAME);

                    while (iterator.hasNext()) {
                        POESSounding soundingData = POESSoundingDataAdapter
                                .createSoundingData(iterator, wmoHeader,
                                        container, pdd, dao);
                        if (soundingData != null) {
                            soundingData.setSourceTraceId(traceId);
                            pdoList.add(soundingData);
                        }
                    }
                    if (!pdoList.isEmpty()) {
                        decodedData = pdoList
                                .toArray(new PluginDataObject[pdoList.size()]);
                    } else {
                        decodedData = new PluginDataObject[0];
                    }
                } catch (Exception ee) {
                    logger.error(traceId + "- Decoder error", ee);
                    decodedData = new PluginDataObject[0];
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
        List<BUFRDataDocument> reportData = new ArrayList<>();

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
                        logger.debug(
                                "Number of packet subsets = " + data.size());
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
     * @see com.raytheon.uf.edex.bufrtools.descriptors.tools.bufr.descriptors.IDescriptorFactorySelector#getSelector()
     */
    @Override
    public String getSelector() {
        return "DEFAULT";
    }

}
