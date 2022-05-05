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
package com.raytheon.uf.edex.plugin.bufrmos.decoder;

import java.util.Calendar;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosAvnData;
import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosData;
import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosData.MOSType;
import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosDataLocation;
import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosEtaData;
import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosGfsData;
import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosHpcData;
import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosLampData;
import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosMrfData;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.bufrtools.BUFRDataDocument;
import com.raytheon.uf.edex.bufrtools.descriptors.BUFRDescriptor;
import com.raytheon.uf.edex.bufrtools.packets.BUFRFloatPacket;
import com.raytheon.uf.edex.bufrtools.packets.BUFRNumericPacket;
import com.raytheon.uf.edex.bufrtools.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.raytheon.uf.edex.plugin.bufrmos.BufrMosSeparator;
import com.raytheon.uf.edex.plugin.bufrmos.MOSPointDataState;

/**
 * This class creates a completed Model Output Statistics (MOS) entry from a
 * single "row" of data decoded from an input file.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 21, 2008 862        jkorman     Initial Coding.
 * May 09, 2013 1869       bsteffen    Modified D2D time series of point data to
 *                                     work without dataURI.
 * Jul 26, 2013 1051       bsteffen    Discard bufrmos data with invalid
 *                                     location.
 * May 14, 2014 2536       bclement    removed TimeTools usage
 * Jul 14, 2015 4543       dgilling    Stop using relative indexing to perform
 *                                     parameter mapping.
 * Jan 19, 2016 4677       tgurney     Stop manually generating primary key
 *                                     with generateId() (now using sequence
 *                                     instead)
 * Feb 09, 2016 5283       nabowle     Remove NGM MOS support.
 * </pre>
 *
 * @author jkorman
 * @version 1.0
 */
public class BufrMOSDataAdapter {

    private static final int STATION_ID_DESCRIPTOR = BUFRDescriptor
            .createDescriptor(0, 1, 63);

    private static final int LATITUDE_DESCRIPTOR = BUFRDescriptor
            .createDescriptor(0, 5, 2);

    private static final int LONGITUDE_DESCRIPTOR = BUFRDescriptor
            .createDescriptor(0, 6, 2);

    private static final int YEAR_DESCRIPTOR = BUFRDescriptor.createDescriptor(
            0, 4, 1);

    private static final int MONTH_DESCRIPTOR = BUFRDescriptor
            .createDescriptor(0, 4, 2);

    private static final int DAY_DESCRIPTOR = BUFRDescriptor.createDescriptor(
            0, 4, 3);

    private static final int HOUR_DESCRIPTOR = BUFRDescriptor.createDescriptor(
            0, 4, 4);

    private static final int FCSTHOUR_DESCRIPTOR = BUFRDescriptor
            .createDescriptor(0, 4, 24);

    public static long hdrSection = 0;

    public static long dataSection = 0;

    private static BufrMosLocationCache locationCache = BufrMosLocationCache
            .getInstance();

    /**
     * Create a MOSData entry from the next available data in given iterator.
     *
     * @param iterator
     *            The iterator that was used to separate and decode the MOS
     *            data.
     * @param mosType
     *            the mos type
     * @param wmoHeader
     *            the wmo header
     */
    public static BufrMosData createMOSData(BufrMosSeparator separator,
            MOSPointDataState state) throws DataAccessLayerException {

        BUFRDataDocument dataDoc = (BUFRDataDocument) separator.next();
        String mosType = separator.getSelector();

        BufrMosData fcstData = null;
        // make sure we can get a container for the data!
        PointDataContainer pdc = state.getContainer(mosType);
        if ((pdc != null) && (dataDoc != null)) {
            String staId = null;
            int year = -1;
            int month = -1;
            int day = -1;
            int hour = -1;
            int fcstHour = -1;
            boolean haveRequiredData = false;
            List<IBUFRDataPacket> dataList = dataDoc.getList();

            if (dataList != null) {
                switch (MOSType.valueOf(mosType)) {
                case AVN:
                    fcstData = new BufrMosAvnData();
                    break;
                case ETA:
                    fcstData = new BufrMosEtaData();
                    break;
                case GFS:
                    fcstData = new BufrMosGfsData();
                    break;
                case HPC:
                    fcstData = new BufrMosHpcData();
                    break;
                case LAMP:
                    fcstData = new BufrMosLampData();
                    break;
                case MRF:
                    fcstData = new BufrMosMrfData();
                    break;
                }

                BufrMosDataLocation location = new BufrMosDataLocation();

                long startTime = System.currentTimeMillis();
                for (IBUFRDataPacket pp : dataList) {
                    int d = pp.getReferencingDescriptor().getDescriptor();

                    if (d == STATION_ID_DESCRIPTOR) {
                        staId = ((String) pp.getValue()).trim();
                        location.setStationId(staId);
                    } else if (d == LATITUDE_DESCRIPTOR) {
                        location.setLatitude(((Double) pp.getValue()));
                    } else if (d == LONGITUDE_DESCRIPTOR) {
                        location.setLongitude(((Double) pp.getValue()));
                    } else if (d == YEAR_DESCRIPTOR) {
                        year = ((Double) pp.getValue()).intValue();
                    } else if (d == MONTH_DESCRIPTOR) {
                        month = ((Double) pp.getValue()).intValue();
                    } else if (d == DAY_DESCRIPTOR) {
                        day = ((Double) pp.getValue()).intValue();
                    } else if (d == HOUR_DESCRIPTOR) {
                        hour = ((Double) pp.getValue()).intValue();
                    } else if (d == FCSTHOUR_DESCRIPTOR) {
                        fcstHour = ((Double) pp.getValue()).intValue();
                        // Once we've found the this data we're done with
                        // the "Header" information.
                        break;
                    }
                } // for

                // lookup mosLocation from cache
                location = locationCache.getLocation(location);

                fcstData.setLocation(location);

                hdrSection += (System.currentTimeMillis() - startTime);
                Calendar baseTime = null;
                // Ensure that we have all of the time info and create the
                // date-time and datatime info.
                if (location.isValid() && (year > 0) && (month > 0)
                        && (day > 0) && (hour >= 0) && (fcstHour >= 0)) {
                    baseTime = TimeUtil.newGmtCalendar(year, month, day);
                    baseTime.set(Calendar.HOUR_OF_DAY, hour);

                    // fcstData.setTimeObs(TimeTools.copy(baseTime));
                    // fcstData.setFcstHour(fcstHour);

                    DataTime dt = new DataTime((Calendar) baseTime.clone(),
                            fcstHour * 3600);
                    fcstData.setDataTime(dt);
                    haveRequiredData = (staId != null);

                }

                startTime = System.currentTimeMillis();
                // Now collect this station's MOS data.
                if (haveRequiredData && (mosType != null)) {
                    Map<Integer, BufrMOSElement> parameterMappings = BUFRMOSStaticData
                            .getInstance().getModelParamterMappings(mosType);
                    PointDataView pdv = pdc.append();
                    for (IBUFRDataPacket packet : dataList) {
                        BufrMOSElement element = parameterMappings.get(packet
                                .getReferencingDescriptor().getDescriptor());
                        if (element != null) {
                            populateMOSElement(packet, element, pdv);
                        }
                    }
                    fcstData.setPointDataView(pdv);
                } else {
                    fcstData = null;
                }
                dataSection += (System.currentTimeMillis() - startTime);
            }
        }
        if (fcstData != null) {
            fcstData.setWmoHeader(separator.getWmoHeader().getWmoHeader());
        }
        return fcstData;
    }

    /**
     * Create a MOSElement entry from a decoded data packet and a definition
     * element.
     *
     * @param packet
     *            A packet containing the decoded data.
     * @param element
     *            The element definition.
     * @return
     */
    private static void populateMOSElement(IBUFRDataPacket packet,
            BufrMOSElement element, PointDataView view) {
        if ((packet != null) && (element != null)) {
            String elementName = element.getElementName();
            if (elementName == null || elementName.equals("")) {
                return;
            }

            if (!view.getContainer().getParameters().contains(elementName)) {
                // Discard any params not in the descriptor
                return;
            }

            // BUFRDescriptor descr = packet.getReferencingDescriptor();
            // if (descr instanceof BUFRTableB) {
            //
            // mElement.setUnits(((BUFRTableB) descr).getUnit());
            // }
            if (packet instanceof BUFRFloatPacket) {
                Double val = (Double) packet.getValue();
                if (val == null || IDecoderConstants.VAL_MISSING >= val) {
                    return;
                } else {
                    view.setFloat(elementName, val.floatValue());
                }
            } else if (packet instanceof BUFRNumericPacket) {
                Integer val = ((Long) packet.getValue()).intValue();
                if (IDecoderConstants.VAL_MISSING >= val) {
                    return;
                } else {
                    view.setInt(elementName, val);
                }
            }
        }
    }
}
