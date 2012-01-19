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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.db.dao.spatial.ObStationDao;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.regional.Sec5Block72Decoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

/**
 * Decode Land synoptic FM-12 observations. The class decodes section 0 for time
 * and location information.
 * 
 * <pre>
 * Sect 0 | AAXX 28184
 * Sect 1 | 84455 31560 60902 10300 20216 39835 40189 70122 8323/
 * Sect 3 | 333 5600/ 58004 83825 83362=
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070928     391        jkorman     Initial Coding.
 * Dec 17, 2007 600        bphillip    Added dao pool usage
 * 20080116            798 jkorman     Changed logging levels.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class LandSynopticDecoder extends AbstractSynopticDecoder {

    // The logger
    private Log logger = LogFactory.getLog(getClass());

    protected ObStation stationInfo = null;

    /**
     * Set up the LAND Synoptic data decoder.
     */
    public LandSynopticDecoder() {
        reportPrefix = "AAXX";
        addSectionDecoder(new SynopticSec1Decoder(this), 1);
        addSectionDecoder(new SynopticSec3Decoder(this), 3);
        addSectionDecoder(new SynopticSec4Decoder(this), 4);
        addSectionDecoder(new SynopticSec5Decoder(this), 5);
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
        }
        if (isValid) {
            reportParser.next();
            setReportIdentifier(reportParser.getElement());

            String staid = getReportIdentifier();
            ObStationDao obSta = null;
            if (staid != null) {
                logger.debug("Processing WMO[" + getReportIdentifier() + "]");
                try {
                    obSta = new ObStationDao();
                    String gid = ObStation.createGID(
                            ObStation.CAT_TYPE_SFC_FXD, staid);
                    stationInfo = obSta.queryByGid(gid);
                    if (stationInfo != null) {
                        setWmoRegion(stationInfo.getWmoRegion());

                        updateSection5Decoder();
                    } else {
                        logger.info("Station id not found ["
                                + getReportIdentifier() + "]");
                        setReportIdentifier(null);
                        clearSectionDecoders();
                    }
                } catch (DataAccessLayerException e) {
                    throw new DecoderException(
                            "Unable to retrieve station info", e);
                }
            } else {
                logger.info("Invalid Station id [" + getReportIdentifier()
                        + "]");
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
            if (stationInfo != null) {
                report.setReportType(IDecoderConstants.SYNOPTIC_FIXED_LAND);

                SurfaceObsLocation loc = new SurfaceObsLocation(
                        getReportIdentifier());
                loc.setGeometry(stationInfo.getGeometry());
                Integer n = stationInfo.getElevation();
                loc.setElevation((n != null) ? n : -9999);
                loc.setLocationDefined(Boolean.TRUE);
                report.setLocation(loc);
            } else {
                report = null;
            }
        }
        return report;
    }

    /**
     * Examine the report identifier to determine if we should override the
     * section 5 decoder for known regional data.
     */
    private void updateSection5Decoder() {
        String block72 = "72\\d{3}";
        Pattern p = Pattern.compile(block72);
        String rptId = getReportIdentifier();
        if (rptId != null) {
            Matcher m = p.matcher(rptId);
            if (m.matches()) {
                addSectionDecoder(new Sec5Block72Decoder(this), 5);
            }
        }
    }

}
