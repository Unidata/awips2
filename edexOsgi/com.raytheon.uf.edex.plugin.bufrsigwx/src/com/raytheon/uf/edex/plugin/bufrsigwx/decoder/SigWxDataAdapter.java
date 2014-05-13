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
package com.raytheon.uf.edex.plugin.bufrsigwx.decoder;

import static com.raytheon.uf.edex.decodertools.bufr.packets.DataPacketTypes.RepSubList;
import static com.raytheon.uf.edex.decodertools.bufr.packets.DataPacketTypes.SubSetList;

import java.util.Calendar;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.bufrsigwx.SigWxData;
import com.raytheon.uf.common.dataplugin.bufrsigwx.common.SigWxLayer;
import com.raytheon.uf.common.dataplugin.bufrsigwx.common.SigWxType;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.bufrtools.BUFRPointDataAdapter;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * This class contains several utility methods that construct a ProfilerObs
 * instance from the BUFR decoded data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 03, 2008 969        jkorman     Initial implementation.
 * Jul 06, 2009 2538       jsanchez    Added latitude,longitude to point data.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * May 14, 2014 2536       bclement    moved WMO Header to common, removed TimeTools usage
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public abstract class SigWxDataAdapter extends BUFRPointDataAdapter<SigWxData> {

    private static Log logger = LogFactory.getLog(SigWxDataAdapter.class);

    private static final int YEAR_POS = 0;

    private static final int MONTH_POS = 1;

    private static final int DAY_POS = 2;

    private static final int HOUR_POS = 3;

    private static final int MINUTE_POS = 4;

    private static final int TIME_SIG_FCST = 4;

    private static final int BASE_HGT_POS = 13;

    private static final int TOP_HGT_POS = 14;

    private static final int MID_LYR_BASE = 3050;

    private static final int HI_LYR_TOP = 19200;

    static final int MISSING = PointDataDescription.FILL_VALUE_INT;

    /**
     * 
     * @param pdd
     * @param dao
     * @param pluginName
     */
    public SigWxDataAdapter(PointDataDescription pdd,
            PointDataPluginDao<SigWxData> dao, String pluginName) {
        super(pdd, dao, pluginName);
    }

    /**
     * Construct a ProfilerObs instance from the BUFR decoded data contained in
     * the specified separator.
     * 
     * @param iterator
     *            An iterator containing decoded BUFR data.
     * @param wmoHeader
     *            a wmoHeader
     * @return A ProfilerObs instance, or null in the event of an error.
     */
    @Override
    public SigWxData createData(Iterator<BUFRDataDocument> iterator,
            WMOHeader wmoHeader) {
        return null;
    }

    /**
     * 
     * @param iterator
     * @param wmoHeader
     */
    @Override
    public List<SigWxData> createDataList(Iterator<BUFRDataDocument> iterator,
            WMOHeader wmoHeader) {

        List<SigWxData> tropList = null;

        SigWxData sigWx = null;

        BUFRDataDocument dataDoc = iterator.next();
        if (dataDoc != null) {
            // Get the primary data list.
            List<IBUFRDataPacket> dataList = dataDoc.getList();

            // Extract the header data.
            sigWx = getHeaderData(dataList);

            if (sigWx != null) {
                sigWx.setWmoHeader(wmoHeader.getWmoHeader());
                sigWx.setWxType(getType());
                // Go collect the data specific to the data being decoded.
                tropList = getSigWxData(sigWx, dataList);
            }
        }
        return tropList;
    }

    /**
     * 
     * @param dataList
     * @return
     */
    SigWxData getHeaderData(List<IBUFRDataPacket> dataList) {
        SigWxData sigWx = null;

        int timeSig = getInt(dataList.get(7), IDecoderConstants.VAL_MISSING);
        if (TIME_SIG_FCST == timeSig) {
            Calendar baseTime = getTimeInfo(dataList, 2);
            Calendar fcstTime = getTimeInfo(dataList, 8);
            if ((fcstTime != null) && (baseTime != null)) {
                sigWx = new SigWxData();

                int fcstSeconds = (int) (fcstTime.getTimeInMillis() - baseTime
                        .getTimeInMillis()) / 1000;
                sigWx.setDataTime(new DataTime((Calendar) baseTime.clone(),
                        fcstSeconds));
                sigWx.setBaseHeight(getInt(dataList.get(BASE_HGT_POS), MISSING));
                sigWx.setTopHeight(getInt(dataList.get(TOP_HGT_POS), MISSING));
                if (sigWx.getBaseHeight() > MISSING) {
                    if (sigWx.getTopHeight() > MISSING) {
                        if (sigWx.getBaseHeight() == MID_LYR_BASE) {
                            if (sigWx.getTopHeight() == HI_LYR_TOP) {
                                sigWx.setWxLayer(SigWxLayer.SWBOTH);
                            } else {
                                sigWx.setWxLayer(SigWxLayer.SWM);
                            }
                        } else {
                            sigWx.setWxLayer(SigWxLayer.SWH);
                        }
                    } else {
                        sigWx = null;
                    }
                } else {
                    sigWx = null;
                }
            }
        }
        return sigWx;
    }

    /**
     * 
     * @param dataList
     * @return
     */
    private Calendar getTimeInfo(List<IBUFRDataPacket> dataList, int basePos) {

        int year = getInt(dataList.get(basePos + YEAR_POS), MISSING);
        int month = getInt(dataList.get(basePos + MONTH_POS), MISSING);
        int day = getInt(dataList.get(basePos + DAY_POS), MISSING);
        int hour = getInt(dataList.get(basePos + HOUR_POS), MISSING);
        int minute = getInt(dataList.get(basePos + MINUTE_POS), MISSING);

        Calendar baseTime = null;

        // Ensure that we have all of the time info and create the
        // date-time and datatime info.
        if ((year > 0) && (month > 0) && (day > 0) && (hour >= 0)
                && (minute >= 0)) {
            baseTime = TimeUtil.newGmtCalendar(year, month, day);
            baseTime.set(Calendar.HOUR_OF_DAY, hour);
            baseTime.set(Calendar.MINUTE, minute);
            baseTime.set(Calendar.SECOND, 0);
            baseTime.set(Calendar.MILLISECOND, 0);
        }
        return baseTime;
    }

    /**
     * 
     * @param sigWx
     * @param index
     */
    abstract List<SigWxData> getSigWxData(SigWxData sigWx,
            List<IBUFRDataPacket> dataList);

    /**
     * 
     * @return
     */
    abstract SigWxType getType();

    abstract void setType(SigWxType type);

    /**
     * 
     * @param packet
     * @return
     */
    @SuppressWarnings("unchecked")
    static List<IBUFRDataPacket> getPacketSubList(IBUFRDataPacket packet) {
        List<IBUFRDataPacket> list = null;
        if (packet instanceof BUFRSublistPacket) {
            if (RepSubList.getPacketType().equals(packet.getUnits())) {
                list = (List<IBUFRDataPacket>) packet.getValue();
            } else if (SubSetList.getPacketType().equals(packet.getUnits())) {
                list = (List<IBUFRDataPacket>) packet.getValue();
            }
        }
        return list;
    }

    /**
     * 
     * @param pdd
     * @param dao
     * @param pluginName
     * @param wmoHeader
     * @return
     */
    public static SigWxDataAdapter getAdapter(PointDataDescription pdd,
            PointDataPluginDao<SigWxData> dao, String pluginName,
            WMOHeader wmoHeader) {
        SigWxDataAdapter adapter = null;

        String ttaaii = wmoHeader.getTtaaii();

        if ("JUTE97".equals(ttaaii)) {
            pdd = loadPDD("/res/pointdata/trop_swhshm.xml");
            adapter = new SigWxTropData(pdd, dao, pluginName);
            adapter.setType(SigWxType.TROP);
        } else if ("JUOE00".equals(ttaaii)) {
            pdd = loadPDD("/res/pointdata/trop_swhshm.xml");
            adapter = new SigWxTropData(pdd, dao, pluginName);
            adapter.setType(SigWxType.TROP);
        } else if ("JUWE96".equals(ttaaii)) {
            pdd = loadPDD("/res/pointdata/jet_swhshm.xml");
            adapter = new SigWxJetsData(pdd, dao, pluginName);
            adapter.setType(SigWxType.JETS);
        } else if ("JUTE00".equals(ttaaii)) {
            pdd = loadPDD("/res/pointdata/jet_swhshm.xml");
            adapter = new SigWxJetsData(pdd, dao, pluginName);
            adapter.setType(SigWxType.JETS);
        } else if ("JUBE99".equals(ttaaii)) {
            pdd = loadPDD("/res/pointdata/cloud_swh.xml");
            adapter = new SigWxCloudsData(pdd, dao, pluginName);
            adapter.setType(SigWxType.CLOUD);
        } else if ("JUNE00".equals(ttaaii)) {
            pdd = loadPDD("/res/pointdata/cloud_shm.xml");
            adapter = new SigWxCloudsData(pdd, dao, pluginName);
            adapter.setType(SigWxType.CLOUD);
        } else if ("JUCE00".equals(ttaaii)) {
            pdd = loadPDD("/res/pointdata/cat_swhshm.xml");
            adapter = new SigWxCatData(pdd, dao, pluginName);
            adapter.setType(SigWxType.CAT);
        } else if ("JUME00".equals(ttaaii)) {
            pdd = loadPDD("/res/pointdata/cat_swhshm.xml");
            adapter = new SigWxCatData(pdd, dao, pluginName);
            adapter.setType(SigWxType.CAT);
        } else if ("JUFE00".equals(ttaaii)) {
            pdd = loadPDD("/res/pointdata/fronts_swhshm.xml");
            adapter = new SigWxFrontsData(pdd, dao, pluginName);
            adapter.setType(SigWxType.FRONTS);
        } else if ("JUJE00".equals(ttaaii)) {
            pdd = loadPDD("/res/pointdata/fronts_swhshm.xml");
            adapter = new SigWxFrontsData(pdd, dao, pluginName);
            adapter.setType(SigWxType.FRONTS);
        } else if ("JUVE00".equals(ttaaii)) {
            pdd = loadPDD("/res/pointdata/vts_swhshm.xml");
            adapter = new SigWxVTSData(pdd, dao, pluginName);
            adapter.setType(SigWxType.VTS);
        } else {
            adapter = new SigWxNullData(pdd, dao, pluginName);
            logger.error("No decoder adapter for file "
                    + wmoHeader.getWmoHeader());
        }
        return adapter;
    }

    private static PointDataDescription loadPDD(String pddFileName) {

        PointDataDescription pdd = null;

        try {
            pdd = PointDataDescription.fromStream(SigWxDataAdapter.class
                    .getResourceAsStream(pddFileName));
            logger.info(pddFileName + "PointDataDescription loaded");

        } catch (Exception e) {
            logger.error("PointDataDescription failed loading " + pddFileName,
                    e);
        }

        return pdd;
    }
}
