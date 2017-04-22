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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.regional.Sec5MaritimeDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;

/**
 * Decode CMAN synoptic observations. The class decodes section 0 for time and
 * location information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070928     391        jkorman     Initial Coding.
 * Dec 17, 2007 600        bphillip    Added dao pool usage
 * 20080116     798        jkorman     Changed logging levels.
 * Feb 27, 2013 1638       mschenke    Moved ObStationDao to edex pointdata plugin
 * Sep 30, 2014 3629       mapeters    Replaced {@link AbstractSfcObsDecoder#matchElement()} calls.
 * Dec 17, 2015 5166       kbisanz     Update logging to use SLF4J
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class CMANSynopticDecoder extends LandSynopticDecoder {

    /** The logger */
    private Logger logger = LoggerFactory.getLogger(getClass());

    /**
     * Construct an instance of a Coastal Marine observation decoder.
     */
    public CMANSynopticDecoder() {
        super();
        reportPrefix = "CMAN";
        // Add section 2 and 5 Maritime decoders for CMAN
        addSectionDecoder(new SynopticSec2Decoder(this), 2);
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
        if (isValid) {
            reportParser.next();
            element = reportParser.getElement();
            if (element != null
                    && ISynoptic.YYGGI_SUB_W.matcher(element).find()) {
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
        }
        if (isValid) {
            reportParser.next();
            String staId = reportParser.getElement();
            setReportIdentifier(staId);
            logger.debug("<-------" + staId + "---------------->");
            if (staId != null) {
                ObStationDao obSta = new ObStationDao();
                try {
                    String gid = ObStation.createGID(ObStation.CAT_TYPE_CMAN,
                            staId);
                    stationInfo = obSta.queryByGid(gid);
                    if (stationInfo != null) {
                        logger.debug("Processing CMAN[" + getReportIdentifier()
                                + "]");
                        setWmoRegion(stationInfo.getWmoRegion());
                    } else {
                        // The NDM data doesn't discriminate between CMAN and
                        // fixed buoy identifiers unless manually modified.
                        gid = ObStation.createGID(ObStation.CAT_TYPE_BUOY_FXD,
                                staId);
                        stationInfo = obSta.queryByGid(gid);
                        if (stationInfo != null) {
                            logger.debug("Processing CMAN["
                                    + getReportIdentifier() + "]");
                            setWmoRegion(stationInfo.getWmoRegion());
                        } else {
                            logger.info("Station id not found ["
                                    + getReportIdentifier() + "]");
                            setReportIdentifier(null);
                            clearSectionDecoders();
                        }
                    }
                } catch (DataAccessLayerException e) {
                    throw new DecoderException(
                            "Unable to retrieve station info", e);
                }
            } else {
                logger.info("CMAN entry null");
                setReportIdentifier(null);
                clearSectionDecoders();
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
        ObsCommon report = (ObsCommon) super.consolidateReport();
        if (report != null) {
            // If we didn't find this report in the catalog then don't store it.
            if ((stationInfo != null)) {
                report.setReportType(SYNOPTIC_CMAN);
                // Land synoptic needs to add the Fixed Land report
                SurfaceObsLocation loc = new SurfaceObsLocation(
                        getReportIdentifier());
                loc.setGeometry(stationInfo.getGeometry());
                loc.setElevation(stationInfo.getElevation());
                loc.setLocationDefined(Boolean.TRUE);
                report.setLocation(loc);
            } else {
                report = null;
            }
        }
        return report;
    }

}
