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
package com.raytheon.edex.plugin.sfcobs.decoder.buoy;

import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.AbstractSynopticDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;

/**
 * Decode the FM-18 Buoy observation data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070928            391 jkorman     Initial Coding.
 * Jul 23, 2014 3410       bclement    location changed to floats
 * Sep 30, 2014 3629       mapeters    Replaced {@link AbstractSfcObsDecoder#matchElement(String, String)}
 *                                     calls, added Pattern constants.
 * Jan 08, 2015 3897       nabowle     Quietly discard reports with invalid
 *                                     Section 2 data due to receiving a lot of
 *                                     bad files with duplicate valid files.
 * Jan 16, 2018 6976       njensen     Check common_obs_spatial for fixed buoys
 * 
 * </pre>
 * 
 * @author jkorman
 */
public class DRIBUSynopticDecoder extends AbstractSynopticDecoder {

    /** The logger */
    private static final Logger logger = LoggerFactory
            .getLogger(DRIBUSynopticDecoder.class);
    // private Integer dateMM = null; // Month
    // private Integer dateJ = null; // Units digit of year
    // private Integer dategg = null; // Minutes

    private static final Pattern PATTERN_1357d5 = Pattern
            .compile("[1357]\\d{5}");

    private static final Pattern PATTERN_1357d4 = Pattern
            .compile("[1357]\\d{4}/");

    private static final Pattern PATTERN_1357d3 = Pattern
            .compile("[1357]\\d{3}//");

    private static final Pattern PATTERN_d6 = Pattern.compile("\\d{6}");

    private static final Pattern PATTERN_d5 = Pattern.compile("\\d{5}/");

    private static final Pattern PATTERN_d4 = Pattern.compile("\\d{4}//");

    private Float buoyLatitude = null;

    private Float buoyLongitude = null;

    private Integer buoyQuadrant = null;

    /**
     * Create an instance of the Drifting buoy decoder.
     */
    public DRIBUSynopticDecoder() {
        reportPrefix = "ZZYY";
        // Decode the meteorological data.
        addSectionDecoder(new DRIBUSec1Decoder(this), 1);
        // Decode the wave data
        addSectionDecoder(new DRIBUSec2Decoder(this), 2);
        // For now we're not decoding any of the oceanographical data.
        // addSectionDecoder(new DRIBUSec3Decoder(this), 3);
        // Decode the buoy movement
        addSectionDecoder(new DRIBUSec4Decoder(this), 4);
        //
        addSectionDecoder(new DRIBUSec5Decoder(this), 5);
    }

    /**
     * Decode the observation sections with in the current report. Due to seeing
     * a large volume of invalid files that duplicate the data from valid files,
     * Section 2 decoding errors are logged at a lower level and null is
     * returned.
     *
     * @returns The decoded PluginDataObject or null.
     */
    @Override
    public PluginDataObject decode() throws DecoderException {
        decodeSection0();
        decodeSection1();
        try {
            decodeSection2();
        } catch (DecoderException de) {
            logger.info("Discarding buoy report. " + de.getLocalizedMessage(),
                    de);
            return null;
        }
        decodeSection3();
        decodeSection4();
        decodeSection5();
        return consolidateReport();
    }

    /**
     * Perform the section 0 decode for Drifting Buoy reports.
     *
     * ZZYY A1bwnbnbnb YYMMJ GGggiw QcLaLaLaLaLa LoLoLoLoLoLo (6QlQtQA/)
     *
     * @throws DecoderException
     */
    @Override
    protected void decodeSection0() throws DecoderException {
        boolean isValid = false;

        String element = reportParser.getElement();
        isValid = reportPrefix.equals(element);
        if (isValid) {

            // A1bwnbnbnb - This is essentially the BUOY id number.
            reportParser.next();
            setReportIdentifier(reportParser.getElement());
            // YYMMJ
            decodeDate();
            // GGggisubw
            decodeTime();
            // QcLaLaLaLaLa
            decodeLatitude();
            // LoLoLoLoLoLo
            decodeLongitude();
            // If either of the lat or lon is null, then there's no need to
            // continue. Abort this decode.
            if ((buoyLatitude == null) || (buoyLongitude == null)) {
                clearSectionDecoders();
                return;
            }
            adjustLatLon();

            // step over the quality control flags : 6Q1QtQA/
            reportParser.next();
            reportParser.getElement();
        }
    }

    /**
     * Consolidate report gathers together all of the data decoded in the
     * decoder and any sub-decoders used. Any subclass overriding this method
     * must be sure to call back to this method first.
     *
     * @return The decoded data.
     */
    @Override
    protected PluginDataObject consolidateReport() {
        ObsCommon report = null;
        if ((buoyLatitude == null) || (buoyLongitude == null)) {
            return report;
        }
        report = (ObsCommon) super.consolidateReport();
        if (report != null) {
            /*
             * code borrowed from SHIPSynopticDecoder to determine if it's a
             * fixed buoy by doing a geometry lookup on common_obs_spatial table
             */
            ObStationDao obSta = null;
            ObStation staInfo = null;
            String id = getReportIdentifier();
            try {
                obSta = new ObStationDao();
                String gid = ObStation.createGID(ObStation.CAT_TYPE_BUOY_FXD,
                        id);
                staInfo = obSta.queryByGid(gid);
            } catch (DataAccessLayerException e) {
                logger.error("Unable to query station info for " + id
                        + ", assuming drifting buoy", e);
            }
            int buoyElev = 0;
            if (staInfo != null) {
                buoyLatitude = (float) DecoderTools.getCoordinateLatitude(
                        staInfo.getStationGeom().getCoordinate());
                buoyLongitude = (float) DecoderTools.getCoordinateLongitude(
                        staInfo.getStationGeom().getCoordinate());
                if (staInfo.getElevation() != null) {
                    buoyElev = staInfo.getElevation();
                }
                report.setReportType(SYNOPTIC_MOORED_BUOY);
            } else {
                report.setReportType(DRIFTING_BUOY);
            }

            SurfaceObsLocation loc = new SurfaceObsLocation(
                    id);
            loc.assignLocation(buoyLatitude, buoyLongitude);
            loc.setElevation(buoyElev);
            loc.setLocationDefined(Boolean.FALSE);
            report.setLocation(loc);
        }

        return report;
    }

    /**
     * Decode the YYMMJ date group.
     */
    private void decodeDate() {
        reportParser.next();
        Integer month = getHeader().getMonth();
        if (month != -1) {
            setObsMonth(month);
        }

        Integer year = getHeader().getYear();
        if (year != -1) {
            setObsYear(year);
        }
        String element = reportParser.getElement();
        setObsDay(getInt(element, 0, 2));
        // dateMM = getInt(element, 2, 4);
        getInt(element, 2, 4);
        // dateJ = getInt(element, 4, 5);
        getInt(element, 4, 5);
    }

    /**
     * Decode the GGggisubw time group.
     */
    private void decodeTime() {
        reportParser.next();

        String element = reportParser.getElement();
        setObsHour(getInt(element, 0, 2));
        // dategg = getInt(element, 2, 4);
        getInt(element, 2, 4);
        setISubw(getInt(element, 4, 5));
    }

    /**
     * Decode the buoy latitude information and quadrant.
     *
     * <pre>
     * ddddd   Latitude to 1/1000 degree
     * dddd/   Latitude to  1/100 degree
     * ddd//   Latitude to   1/10 degree
     * </pre>
     */
    private void decodeLatitude() {
        reportParser.next();
        String element = reportParser.getElement();
        Integer lat = null;
        float divisor = 1000.0f;
        if (element != null) {
            if (PATTERN_1357d5.matcher(element).find()) {
                buoyQuadrant = getInt(element, 0, 1);
                lat = getInt(element, 1, 6);
            } else if (PATTERN_1357d4.matcher(element).find()) {
                buoyQuadrant = getInt(element, 0, 1);
                lat = getInt(element, 1, 5);
                divisor = 100.0f;
            } else if (PATTERN_1357d3.matcher(element).find()) {
                buoyQuadrant = getInt(element, 0, 1);
                lat = getInt(element, 1, 4);
                divisor = 10.0f;
            }
        }
        if ((lat != null) && (lat >= 0)) {
            buoyLatitude = lat.floatValue() / divisor;
        } else {
            buoyLatitude = null;
        }
    }

    /**
     * Decode the ship longitude element.
     *
     * <pre>
     * dddddd   Longitude to 1/1000 degree
     * ddddd/   Longitude to  1/100 degree
     * dddd//   Longitude to   1/10 degree
     * </pre>
     */
    private void decodeLongitude() {
        reportParser.next();
        String element = reportParser.getElement();
        Integer lon = null;
        float divisor = 1000.0f;
        if (element != null) {
            if (PATTERN_d6.matcher(element).find()) {
                lon = getInt(element, 0, 6);
            } else if (PATTERN_d5.matcher(element).find()) {
                lon = getInt(element, 0, 5);
                divisor = 100.0f;
            } else if (PATTERN_d4.matcher(element).find()) {
                lon = getInt(element, 0, 4);
                divisor = 10.0f;
            }
        }
        if ((lon != null) && (lon >= 0)) {
            buoyLongitude = lon.floatValue() / divisor;
        } else {
            buoyLongitude = null;
        }
    }

    /**
     * Adjust both the latitude and longitude based on the reported quadrant per
     * WMO 306 table 3333.
     */
    private void adjustLatLon() {
        // Do we have good data to work with?
        if ((buoyLatitude != null) && (buoyLongitude != null)
                && (buoyQuadrant != null)) {
            if ((buoyLatitude >= 0) && (buoyLongitude >= 0)) {
                float lat = 0;
                float lon = 0;
                switch (buoyQuadrant) {
                case 1: {
                    lat = 1;
                    lon = 1;
                    break;
                }
                case 3: {
                    lat = -1;
                    lon = 1;
                    break;
                }
                case 5: {
                    lat = -1;
                    lon = -1;
                    break;
                }
                case 7: {
                    lat = 1;
                    lon = -1;
                    break;
                }
                default: {
                    // this is an error condition, since it got past the
                    // longitude checks.
                }
                }
                buoyLatitude = buoyLatitude * lat;
                buoyLongitude = buoyLongitude * lon;
            }
        }
    }
}
