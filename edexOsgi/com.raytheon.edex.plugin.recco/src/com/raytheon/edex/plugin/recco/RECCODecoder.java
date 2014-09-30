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
package com.raytheon.edex.plugin.recco;

import java.util.Calendar;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.recco.common.RECCORecord;
import com.raytheon.edex.plugin.recco.decoder.ReccoParser;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.pointdata.spatial.AircraftObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.raytheon.uf.edex.decodertools.core.IDecoderInput;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * Decoder strategy for text aircraft RECCO observation data. Most common usage
 * is as follows. <code>
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
 * Jan 03, 2008 384        jkorman     Initial Coding.
 * Nov 25, 2008 1684       chammack    Camel Refactor
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * May 14, 2014 2536       bclement    moved WMO Header to common, removed PLUGIN_NAME
 * Jul 23, 2014 3410       bclement    location changed to floats
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class RECCODecoder extends AbstractDecoder {

    private boolean removeNILs = true;

    /**
     * @param pluginName
     *            Name that identifies this decoder.
     * @throws DecoderException
     */
    @Deprecated
    public RECCODecoder(String pluginName) {
    }

    /**
     * Determine the removeNILs status.
     * 
     * @return Should NIL reports be removed.
     */
    public boolean isRemoveNILs() {
        return removeNILs;
    }

    /**
     * Set the removeNILs status.
     * 
     * @param removeNILs
     *            Should NIL reports be removed.
     */
    public void setRemoveNILs(boolean removeNILs) {
        this.removeNILs = removeNILs;
    }

    /**
     * Get the next decoded data record.
     * 
     * @return One record of decoded data.
     * @throws DecoderException
     *             Thrown if no data is available.
     */
    public PluginDataObject[] decode(IDecoderInput input, Headers headers) {

        PluginDataObject[] reports = null;
        String traceId = null;
        try {
            traceId = input.getProperty("traceId");
            logger.debug(traceId + "- RECCODecoder.decode()");

            RECCORecord report = populateRecord(
                    new ReccoParser(input.getReport(), headers),
                    input.getWmoHeader());
            if (report != null) {
                report.setTraceId(traceId);

                reports = new PluginDataObject[] { report };
            }
        } catch (Exception e) {
            logger.error(traceId + "- Error in RECCODecoder", e);
        } finally {
            if (reports == null) {
                reports = new PluginDataObject[0];
            }
        }
        return reports;
    }

    /**
     * @param parser
     *            The reccon parser that contains the decoded data.
     * @return The populated record.
     */
    private RECCORecord populateRecord(ReccoParser parser, WMOHeader wmoHeader) {

        RECCORecord record = null;
        AircraftObsLocation location = null;
        if (parser != null) {

            // If there is no obstime, don't bother going further.
            Calendar oTime = parser.getObservationTime();
            if (oTime != null) {
                record = new RECCORecord();
                location = new AircraftObsLocation();

                record.setTimeObs(oTime);
                record.setRefHour(TimeTools.copyToNearestHour(oTime));
                DataTime dataTime = new DataTime(oTime);
                record.setDataTime(dataTime);

                record.setWmoHeader(wmoHeader.getWmoHeader());
                record.setReportData(parser.getReportData());

                if ("222".equals(parser.getObsType())) {
                    record.setReportType(IDecoderConstants.RECCO_MANOBS);
                } else if ("555".equals(parser.getObsType())) {
                    record.setReportType(IDecoderConstants.RECCO_INTEROBS);
                } else if ("777".equals(parser.getObsType())) {
                    record.setReportType(IDecoderConstants.RECCO_MANOBS);
                }

                location.setLatitude(parser.getLatitude());
                location.setLongitude(parser.getLongitude());
                location.setLocation(parser.getLatitude(),
                        parser.getLongitude());
                location.setFlightLevel(parser.getFlightLevel()
                        .getFlightLevel());
                record.setLocation(location);

                record.setTemp(parser.getTemperature());
                record.setDwpt(parser.getDewpoint());

                record.setWindDirection(parser.getWindDirection());
                record.setWindSpeed(parser.getWindSpeed());
            }
        }
        return record;
    }

}
