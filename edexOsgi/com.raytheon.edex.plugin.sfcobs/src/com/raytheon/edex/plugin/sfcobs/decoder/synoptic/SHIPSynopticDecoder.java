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
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.regional.Sec5MaritimeDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;

/**
 * Decode SHIP synoptic FM-13 observations. The class decodes section 0 for time
 * and location information. In addition it sets up the appropriate section
 * decoders for specific sections.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070928            391 jkorman     Initial Coding.
 * 20080106            391 jkorman     Corrected ship longitude decode.
 * 20080108            721 jkorman     Added buoy id query.
 * 20120619      DR 14015  mporricelli Added elevation for fixed buoys
 * Feb 27, 2013 1638       mschenke    Moved ObStationDao to edex pointdata plugin
 * Jul 23, 2014 3410       bclement    location changed to floats
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class SHIPSynopticDecoder extends AbstractSynopticDecoder {
    // The logger
    private Log logger = LogFactory.getLog(getClass());

    protected Float shipLatitude = null;

    protected Float shipLongitude = null;

    protected Integer shipElev = null;
    
    protected Integer shipQuadrant = null;

    protected boolean isFixedBuoy = false;

    /**
     * Set up the SHIP synoptic decoder and section decoders.
     */
    public SHIPSynopticDecoder() {
        reportPrefix = "BBXX";
        addSectionDecoder(new SynopticSec1Decoder(this), 1);
        addSectionDecoder(new SynopticSec2Decoder(this), 2);
        addSectionDecoder(new SynopticSec3Decoder(this), 3);
        addSectionDecoder(new SynopticSec4Decoder(this), 4);
        addSectionDecoder(new Sec5MaritimeDecoder(this), 5);
    }

    /**
     * Perform the section 0 decode for Land Synoptic reports.
     * 
     * @throws DecoderException
     */
    protected void decodeSection0() throws DecoderException {
        boolean isValid = false;

        String element = reportParser.getElement();
        isValid = reportPrefix.equals(element);
        Double buoyLat = null;
        Double buoyLon = null;
        Integer buoyElev = null;        
        isFixedBuoy = false;
        if (isValid) {
            reportParser.next();
            String rpdId = reportParser.getElement();
            setReportIdentifier(rpdId);

            String id = rpdId;
            if (id != null) {

                ObStationDao obSta = null;
                ObStation staInfo = null;
                try {
                    obSta = new ObStationDao();
                    String gid = ObStation.createGID(
                            ObStation.CAT_TYPE_BUOY_FXD, id);

                    staInfo = obSta.queryByGid(gid);
                } catch (DataAccessLayerException e) {
                    throw new DecoderException(
                            "Unable to retrieve station info", e);
                }
                if(staInfo != null) {
                    isFixedBuoy = true;
                    buoyLat = DecoderTools.getCoordinateLatitude(staInfo.getStationGeom().getCoordinate());
                    buoyLon = DecoderTools.getCoordinateLongitude(staInfo.getStationGeom().getCoordinate());
                    buoyElev = staInfo.getElevation();
                }
            }
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
            if(isFixedBuoy) {
                // This change selects the common_obs_spatial location over the
                // encoded location. This is so that moored buoy locations agree
                // with the spi files 
                
                
                shipLatitude = buoyLat != null ? buoyLat.floatValue() : null;
                shipLongitude = buoyLon != null ? buoyLon.floatValue() : null;
                shipElev = buoyElev;
                if ((shipLatitude == null) || (shipLongitude == null)) {
                    clearSectionDecoders();
                    logger.error("Bad Geometry for " + getReportIdentifier());
                    return;
                }
            } else {
                if ((shipLatitude == null) || (shipLongitude == null)) {
                    clearSectionDecoders();
                    logger.error("BAD:YYGGI_SUB_W : " + reportParser.getReport());
                    return;
                }
                adjustLatLon();
            }
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
        if ((shipLatitude == null) || (shipLongitude == null)) {
            return report;
        }

        report = (ObsCommon) super.consolidateReport();
        if (report != null) {

            if (isFixedBuoy) {
                report.setReportType(IDecoderConstants.SYNOPTIC_MOORED_BUOY);
            } else {
                report.setReportType(IDecoderConstants.SYNOPTIC_SHIP);
            }

            SurfaceObsLocation loc = new SurfaceObsLocation(
                    getReportIdentifier());
            loc.assignLocation(shipLatitude, shipLongitude);
            loc.setElevation(shipElev);
            loc.setLocationDefined(Boolean.FALSE);
            report.setLocation(loc);
        }
        return report;
    }

    /**
     * Decode the ship latitude information.
     */
    protected void decodeLatitude() {
        reportParser.next();
        String element = reportParser.getElement();

        if (matchElement(element, "99\\d{3}")) {
            Integer lat = getInt(element, 2, 5);
            if ((lat != null) && (lat >= 0)) {
                shipLatitude = lat.floatValue() / 10.0f;
            } else {
                shipLatitude = null;
            }
        }
    }

    /**
     * Decode the ship longitude element. This will set the longitude and the
     * location quadrant.
     */
    protected void decodeLongitude() {
        reportParser.next();
        String element = reportParser.getElement();

        if (matchElement(element, "[1357]((0\\d{3})|(1(([0-7]\\d{2})|(800))))")) {
            Integer lon = getInt(element, 1, 5);
            if ((lon != null) && (lon >= 0)) {
                shipLongitude = lon.floatValue() / 10.0f;
            } else {
                shipLongitude = null;
            }
            shipQuadrant = getInt(element, 0, 1);
        }
    }

    /**
     * Adjust both the latitude and longitude based on the reported quadrant per
     * WMO 306 table 3333.
     */
    protected void adjustLatLon() {
        // Do we have good data to work with?
        if ((shipLatitude != null) && (shipLongitude != null)
                && (shipQuadrant != null)) {
            if ((shipLatitude >= 0) && (shipLongitude >= 0)) {
                float lat = 0;
                float lon = 0;
                switch (shipQuadrant) {
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
                shipLatitude = shipLatitude * lat;
                shipLongitude = shipLongitude * lon;
            }
        }
    }

}
