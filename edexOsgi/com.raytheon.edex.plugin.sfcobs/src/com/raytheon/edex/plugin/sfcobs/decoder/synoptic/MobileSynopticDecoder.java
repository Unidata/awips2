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
package com.raytheon.edex.plugin.sfcobs.decoder.synoptic;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

/**
 * Decode Mobile synoptic FM-14 observations. The class decodes section 0 for
 * time and location information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070928            391 jkorman     Initial Coding.
 * Jul 23, 2014 3410       bclement    location changed to floats
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class MobileSynopticDecoder extends AbstractSynopticDecoder {

    // The logger
    private Log logger = LogFactory.getLog(getClass());

    private Float mobileLatitude = null;

    private Float mobileLongitude = null;

    private Integer mobileQuadrant = null;

    private Integer mobileElevation = null;

    // WMO 306 Table 1845
    // private Integer elevationIndicator = null;

    /**
     * 
     */
    public MobileSynopticDecoder() {
        reportPrefix = "OOXX";
        addSectionDecoder(new SynopticSec1Decoder(this), 1);
        addSectionDecoder(new SynopticSec3Decoder(this), 3);
        addSectionDecoder(new SynopticSec4Decoder(this), 4);
        addSectionDecoder(new SynopticSec5Decoder(this), 5);
    }

    /**
     * Perform the section 0 decode for Drifting Buoy reports.
     * 
     * @throws DecoderException
     */
    /**
     * Perform the section 0 decode for Land Synoptic reports.
     * 
     * @throws DecoderException
     */
    protected void decodeSection0() throws DecoderException {
        boolean isValid = false;

        String element = reportParser.getElement();
        isValid = reportPrefix.equals(element);
        if (isValid) {
            reportParser.next();
            setReportIdentifier(reportParser.getElement());
            reportParser.next();
            element = reportParser.getElement();
            if (matchElement(element, ISynoptic.YYGGI_SUB_W)) {
                try {
                    Integer month = getHeader().getMonth();
                    if (month != -1) {
                        setObsMonth(month);
                    }

                    Integer year = getHeader().getYear();
                    if (year != -1) {
                        setObsYear(year);
                    }
                    Integer val = getInt(element, 0, 2);
                    setObsDay(val);

                    val = getInt(element, 2, 4);
                    setObsHour(val);

                    val = getInt(element, 4, 5);
                    setISubw(val);
                } catch (NumberFormatException nfe) {

                }
                isValid = true;
            } else {
                logger.error("BAD:YYGGI_SUB_W : " + reportParser.getReport());
                clearSectionDecoders();
                return;
            }
            logger.info("<-------" + getReportIdentifier()
                    + "---------------->");
            decodeLatitude();
            decodeLongitude();
            if ((mobileLatitude == null) || (mobileLongitude == null)) {
                clearSectionDecoders();
                return;
            }

            adjustLatLon();
            getMarsdenSquareInfo();
            getElevationInfo();
        }
    }

    /**
     * Consolidate report gathers together all of the data decoded in the
     * decoder and any sub-decoders used. Any subclass overriding this method
     * must be sure to call back to this method first.
     * 
     * @return The decoded data.
     */
    protected PluginDataObject consolidateReport() {
        ObsCommon report = null;
        if ((mobileLatitude == null) || (mobileLongitude == null)) {
            return report;
        }
        report = (ObsCommon) super.consolidateReport();
        if (report != null) {
            report.setReportType(IDecoderConstants.SYNOPTIC_MOBILE_LAND);

            SurfaceObsLocation loc = new SurfaceObsLocation(
                    getReportIdentifier());
            loc.assignLocation(mobileLatitude, mobileLongitude);
            loc.setElevation(mobileElevation);
            loc.setLocationDefined(Boolean.FALSE);
            report.setLocation(loc);
        }
        return report;
    }

    /**
     * Decode the ship latitude information.
     * 
     * @param A
     *            text element.
     */
    private void decodeLatitude() {
        reportParser.next();
        String element = reportParser.getElement();

        if (matchElement(element, "99\\d{3}")) {
            Integer lat = getInt(element, 2, 5);
            if (lat != null) {
                mobileLatitude = lat.floatValue() / 10.0f;
            }
        }
    }

    /**
     * Decode the ship longitude element. This will set the longitude and the
     * location quadrant.
     * 
     * @param A
     *            text element.
     */
    private void decodeLongitude() {
        reportParser.next();
        String element = reportParser.getElement();

        if (matchElement(element, "[1357]((0\\d{3})|(1(([0-7]\\d{2})|(800))))")) {
            Integer lon = getInt(element, 2, 5);
            if (lon != null) {
                mobileLongitude = lon.floatValue() / 10.0f;
            }
            mobileQuadrant = getInt(element, 0, 1);
        }
    }

    /**
     * Adjust both the latitude and longitude based on the reported quadrant per
     * WMO 306 table 3333.
     */
    private void adjustLatLon() {
        // Do we have good data to work with?
        if ((mobileLatitude != null) && (mobileLongitude != null)
                && (mobileQuadrant != null)) {
            if ((mobileLatitude >= 0) && (mobileLongitude >= 0)) {
                float lat = 0;
                float lon = 0;
                switch (mobileQuadrant) {
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
                mobileLatitude = mobileLatitude * lat;
                mobileLongitude = mobileLongitude * lon;
            }
        }
    }

    /**
     * Decode the Marsden square info group. We're not storing this info and the
     * ULa, ULo items are redundant.
     */
    private void getMarsdenSquareInfo() {
        reportParser.next();
    }

    /**
     * Decode and set the station elevation info.
     */
    private void getElevationInfo() {
        reportParser.next();
        String element = reportParser.getElement();

        if (matchElement(element, "[/0-9]{4}[1-8]")) {
            Integer elev = getInt(element, 0, 4);
            if (elev != null) {
                if (elev >= 0) {
                    mobileElevation = elev;
                }
            }
            // elevationIndicator = getInt(element, 4, 5);
            getInt(element, 4, 5);
        }
    }

}
