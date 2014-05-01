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
package com.raytheon.edex.plugin.profiler;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.profiler.decoder.ProfilerDataAdapter;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.profiler.ProfilerObs;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
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
 * Perform decode on BUFR Profiler data. Currently this decoder does not check
 * for valid time nor the station location, making the assumption that the
 * internal data is good. This decoder does check for possible duplicate data in
 * the database prior to returning the decoded data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 03, 2008  969      jkorman     Initial implementation.
 * Apr 08, 2008  1039     jkorman     Added traceId for tracing data.
 * Aug 30, 2013  2298     rjpeter     Make getPluginName abstract
 * Dec 03, 2013  2537     bsteffen    Switch logger to ufstatus.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class ProfilerDecoder extends AbstractDecoder implements
        IDescriptorFactorySelector {

    // Name of the plugin controlling this decoder.
    public static final String PLUGIN_NAME = "profiler";

    /** The logger */
    private static final IUFStatusHandler logger = UFStatus
            .getHandler(ProfilerDecoder.class);

    private PointDataDescription pdd;

    // If failSafe is set to true, this plugin sinks the data but
    // does no processing!
    private boolean failSafe = false;

    /**
     * Construct a ProfilerDecoder instance.
     */
    public ProfilerDecoder() {
        try {
            pdd = PointDataDescription.fromStream(this.getClass()
                    .getResourceAsStream("/res/pointdata/profiler.xml"));

            logger.info("PointDataDescription loaded");

        } catch (Exception e) {
            logger.error("PointDataDescription failed", e);
            logger.error("Plugin set to failSafe mode");
            failSafe = true;
        }
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

    /**
     * Get one entry from the separator and interpret that data as a single
     * profiler observation.
     * 
     * @param data
     *            the data
     * 
     * @return A single decoded observation. May return array of size 0 if the
     *         observation is a duplicate in the database or is invalid.
     */
    public PluginDataObject[] decode(byte[] data, Headers headers) {

        String traceId = "";
        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }
        if (failSafe) {
            return new PluginDataObject[0];
        }

        PluginDataObject[] decodedData = null;

        if ((data != null) && (data.length > 0)) {

            WMOHeader wmoHeader = new WMOHeader(data, headers);
            if ((wmoHeader != null) && (wmoHeader.isValid())) {
                try {

                    IDescriptorFactoryDelegate delegate = new DefaultDescriptorDelegate(
                            this);

                    int start = wmoHeader.getMessageDataStart();
                    int len = data.length - start;

                    byte[] messageData = new byte[len];
                    System.arraycopy(data, start, messageData, 0, len);

                    List<BUFRDataDocument> document = decodeBUFRData(
                            messageData, delegate);
                    Iterator<BUFRDataDocument> iterator = document.iterator();
                    List<ProfilerObs> pdoList = new ArrayList<ProfilerObs>();

                    PointDataContainer container = PointDataContainer.build(
                            pdd, document.size());

                    while (iterator.hasNext()) {
                        ProfilerObs soundingData = ProfilerDataAdapter
                                .createProfilerData(iterator, wmoHeader,
                                        container, traceId);
                        if (soundingData != null) {
                            soundingData.setTraceId(traceId);
                            pdoList.add(soundingData);
                        }
                    }
                    if (pdoList.size() > 0) {
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

}
