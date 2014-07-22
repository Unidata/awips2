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
package com.raytheon.edex.plugin.bufrua;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.plugin.bufrua.dao.BufrUADao;
import com.raytheon.edex.plugin.bufrua.decoder.AbstractBUFRUAAdapter;
import com.raytheon.edex.plugin.bufrua.decoder.BUFRUAAdapterFactory;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.bufrua.UAObs;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.bufrtools.AbstractBUFRDecoder;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.DefaultDescriptorDelegate;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;

/**
 * Decoder strategy for BUFR upper air observation data. Most common usage is as
 * follows. <code>
 *   SfcObsDecoder dec = new SfcObsDecoder();
 *   dec.setMessage();
 *   while(dec.hasNext())
 *   {
 *      PluginDataObject r = dec.decode();
 *      // do something with record.
 *   }
 * </code>
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071127            382 jkorman     Initial Coding.
 * 20080116            798 jkorman     Changed logging levels.
 * 20080123            769 jkorman     Added &quot;future&quot; observation rejection.
 * 20080214            862 jkorman     Refactored data separation into BUFRFile.
 * 20080219            863 jkorman     Completed duplicate elimination detection
 *                                     in findDuplicate.
 * 20080408           1039 jkorman     Added traceId for tracing data.
 * 11/25/08          #1684 chammack    Camel Refactor
 * Feb 27, 2013 1638       mschenke   Moved ObStationDao to edex pointdata plugin
 * Mar 19, 2013 1785       bgonzale    Added performance status handler and added status
 *                                     to decodeData.
 * Jul 23, 2014 3410       bclement    removed call to obs.getDataURI()
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BufrUADecoder extends AbstractBUFRDecoder {

    private static final Pattern COR_PTRN = Pattern
            .compile("^(CC[A-Z])($| ).*");

    private BufrUADao dao;

    private BUFRUAAdapterFactory adapterFactory;

    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("BufrUA:");

    /**
     * 
     * @param name
     */
    public BufrUADecoder(String name) {
        super(name);
        try {
            PointDataDescription pdd = dao.getPointDataDescription(null);

            logger.info("PointDataDescription loaded");

            adapterFactory = new BUFRUAAdapterFactory(pdd, dao, pluginName);
        } catch (Exception e) {
            logger.error("PointDataDescription failed", e);
            logger.error("Plugin set to failSafe mode");
            setFailSafe(true);
        }
        setFactoryDelegate(new DefaultDescriptorDelegate(this));
    }

    /**
     * 
     */
    @Override
    public List<PluginDataObject> decodeData(List<BUFRDataDocument> document,
            String traceId, WMOHeader wmoHeader) {

        List<PluginDataObject> decodedData = null;
        if (document != null) {
            decodedData = new ArrayList<PluginDataObject>();
            AbstractBUFRUAAdapter adapter = adapterFactory
                    .getAdapter(wmoHeader);

            Set<String> dataSet = new HashSet<String>();

            logger.debug("List contains " + document.size()
                    + " BUFRDataDocuments");

            Iterator<BUFRDataDocument> iterator = document.iterator();

            String cor = isCor(wmoHeader);
            ITimer timer = TimeUtil.getTimer();

            timer.start();
            while (iterator.hasNext()) {

                logger.debug("Decoding one BUFRDataDocument");
                UAObs obs = adapter.createData(iterator, wmoHeader);
                if (obs != null) {
                    obs.setCorIndicator(cor);
                    obs.setTraceId(traceId);
                    if ((obs = queryStationInfo(obs, traceId)) != null) {
                        String uri = obs.getDataURI();

                        if (dataSet.add(uri)) {
                            decodedData.add(obs);
                        }
                    }
                }
            }
            timer.stop();
            perfLog.logDuration("Time to Decode", timer.getElapsedTime());
        }
        return decodedData;
    }

    /**
     * Get the lat/lon geometry info for this observation.
     * 
     * @param obs
     *            An observation to get the location for.
     * @return The observation data with the location information populated. If
     *         the location identifier could not be found, the observation is
     *         set to null.
     */
    private UAObs queryStationInfo(UAObs obs, String traceId) {

        ObStationDao obSta = new ObStationDao();
        ObStation stationInfo = null;
        String id = obs.getStationId();

        Integer staid = DecoderTools.getInt(id, 0, 5);
        if ((staid != null) && (staid >= 0)) {
            logger.debug(traceId + "-Processing WMO[" + id + "]");
            try {
                String gid = ObStation.createGID(ObStation.CAT_TYPE_SFC_RAOB,
                        id);
                stationInfo = obSta.queryByGid(gid);
                if ((stationInfo != null)
                        && (stationInfo.getUpperAirGeometry() != null)) {
                    SurfaceObsLocation loc = new SurfaceObsLocation(id);

                    loc.setGeometry(stationInfo.getUpperAirGeometry());
                    loc.setElevation(stationInfo.getUpperAirElevation());

                    if ((loc.getLatitude() == null)
                            || (loc.getLongitude() == null)) {
                        logger.error(traceId + "-Station location bad for ["
                                + id + "]");
                        obs = null;
                    } else {
                        PointDataView pdv = obs.getPointDataView();
                        if (pdv != null) {
                            String s = stationInfo.getIcao();
                            if ((s != null) && (s.length() > 3)) {
                                pdv.setString("staName", stationInfo.getIcao());
                            } else {
                                s = String.format("%05d", staid);
                                pdv.setString("staName", s);
                            }
                            obs.setStationName(s);
                        }
                        loc.setLocationDefined(Boolean.TRUE);
                        obs.setLocation(loc);
                    }

                } else {
                    obs = null;
                    logger.error(traceId + "-Station id not found [" + id + "]");
                }
            } catch (DataAccessLayerException e) {
                logger.error(traceId + "-Could not locate observation " + id, e);
                obs = null;
            }
        }
        return obs;
    }

    /**
     * 
     * @param recreate
     */
    @Override
    protected void createDAO(boolean recreate) {
        if (recreate) {
            dao = null;
        }
        try {
            dao = new BufrUADao(pluginName);
        } catch (Exception e) {
            logger.error("BufrUADao creation failed", e);
            logger.error("Plugin set to failSafe mode");
            setFailSafe(true);
        }
    }

    /**
     * 
     * @param header
     * @return
     */
    private String isCor(WMOHeader header) {
        String cor = null;
        if (header != null) {
            String bbb = header.getBBBIndicator();
            if (bbb != null) {
                Matcher m = COR_PTRN.matcher(bbb);
                if (m.find()) {
                    cor = m.group(1);
                }
            }
        }
        return cor;
    }

    public static final void main(String[] args) {

        String[] data = { "CCA", "CCZ", "CCB dfsaB", null, };

        for (String s : data) {
            System.out.println("********************");
            System.out.println("s = " + s);
            if (s != null) {
                Matcher m = COR_PTRN.matcher(s);
                if (m.find()) {
                    System.out.println("-------------------");
                    for (int i = 0; i < m.groupCount(); i++) {
                        System.out.println(m.group(i));
                    }
                }
            }
        }
    }

}
