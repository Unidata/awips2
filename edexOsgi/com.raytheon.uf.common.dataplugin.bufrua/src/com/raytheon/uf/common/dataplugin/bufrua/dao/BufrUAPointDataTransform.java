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
package com.raytheon.uf.common.dataplugin.bufrua.dao;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.bufrua.LayerTools;
import com.raytheon.uf.common.dataplugin.bufrua.UAObs;
import com.raytheon.uf.common.dataplugin.bufrua.UAObsLevel;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Converts a PointDataContainer into a UAObs record.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 19, 2009            jkorman     Initial creation
 * Jul 19, 2013 1992       bsteffen    Remove redundant time columns from
 *                                     bufrua.
 * Sep  9, 2013 2277       mschenke    Got rid of ScriptCreator references
 * Jul 23, 2014 3410       bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class BufrUAPointDataTransform {

    /** The logger */
    private static Log logger = LogFactory
            .getLog(BufrUAPointDataTransform.class);

    public static final String[] HDR_PARAMS = new String[] { "wmoStaNum",
            "staName", "validTime", "relTime", "staElev", "latitude",
            "longitude", "dataURI", "sfcPressure", "rptType" };

    public static final String HDR_PARAMS_LIST;
    static {
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < HDR_PARAMS.length - 1; ++i) {
            sb.append(HDR_PARAMS[i]).append(",");
        }
        sb.append(HDR_PARAMS[HDR_PARAMS.length - 1]);
        HDR_PARAMS_LIST = sb.toString();
    }

    private static final String[] OTHER_PARAMS = new String[] { "numMand",
            "prMan", "htMan", "tpMan", "tdMan", "wdMan", "wsMan", "numTrop",
            "prTrop", "tpTrop", "tdTrop", "wdTrop", "wsTrop", "numMwnd",
            "prMaxW", "wdMaxW", "wsMaxW", "numSigT", "prSigT", "tpSigT",
            "tdSigT", "numSigW", "htSigW", "wdSigW", "wsSigW" };

    public static final String[] MAN_PARAMS = Arrays.copyOf(HDR_PARAMS,
            HDR_PARAMS.length + OTHER_PARAMS.length);

    public static final String MAN_PARAMS_LIST;
    static {
        StringBuffer sb = new StringBuffer();
        sb.append(HDR_PARAMS_LIST).append(",");

        for (int i = 0; i < OTHER_PARAMS.length - 1; ++i) {
            sb.append(OTHER_PARAMS[i]).append(",");
            MAN_PARAMS[HDR_PARAMS.length + i] = OTHER_PARAMS[i];
        }
        sb.append(OTHER_PARAMS[OTHER_PARAMS.length - 1]);
        MAN_PARAMS[MAN_PARAMS.length - 1] = OTHER_PARAMS[OTHER_PARAMS.length - 1];

        MAN_PARAMS_LIST = sb.toString();
    }

    private static UAObs getUAObsHdr(PointDataView pdv) {
        UAObs obs = null;
        if (pdv != null) {

            String uri = pdv.getString("dataURI");
            logger.debug("URI = " + uri);
            obs = new UAObs(uri);

            long vt = pdv.getNumber("validTime").longValue();

            obs.setDataTime(new DataTime(TimeUtil.newGmtCalendar(new Date(vt))));

            SurfaceObsLocation location = new SurfaceObsLocation();

            int elev = pdv.getNumber("staElev").intValue();
            location.setElevation(elev);
            float lat = pdv.getNumber("latitude").floatValue();
            float lon = pdv.getNumber("longitude").floatValue();
            location.assignLocation(lat, lon);
            Number sta = pdv.getNumber("wmoStaNum");
            location.setStationId(sta.toString());
            obs.setLocation(location);
            Integer sfcpres = pdv.getNumber("sfcPressure").intValue();
            obs.setPressure_station(sfcpres);

            obs.setStationName(pdv.getString("staName"));
        }
        return obs;
    }

    private static UAObs getManUAObsRecord(PointDataView pdv, UAObs obs) {
        if (obs != null) {
            Number numLvls = pdv.getNumber("numMand");
            Number[] pr = pdv.getNumberAllLevels("prMan");
            Number[] ht = pdv.getNumberAllLevels("htMan");
            Number[] tp = pdv.getNumberAllLevels("tpMan");
            Number[] td = pdv.getNumberAllLevels("tdMan");
            Number[] wd = pdv.getNumberAllLevels("wdMan");
            Number[] ws = pdv.getNumberAllLevels("wsMan");

            if ((numLvls != null) && (numLvls.intValue() > 0)) {
                for (int i = 0; i < numLvls.intValue(); i++) {
                    if (pr[i] != null) {
                        if ((pr[i].intValue() > 120000)
                                || (pr[i].intValue() < 0)) {
                            continue;
                        }
                    }
                    UAObsLevel lvl = new UAObsLevel();
                    if (pr[i].intValue() == obs.getPressure_station()) {
                        lvl.setVertSig(LayerTools.SFC_LEVEL);
                        lvl.setGeoHeight(obs.getElevation());
                    } else {
                        lvl.setVertSig(LayerTools.MANPRE_LEVEL);
                    }
                    lvl.setPressure(pr[i].intValue());
                    lvl.setGeoHeight(ht[i].intValue());
                    lvl.setTemp(tp[i].doubleValue());
                    lvl.setDwpt(td[i].doubleValue());
                    lvl.setWindSpeed(ws[i].doubleValue());
                    lvl.setWindDirection(wd[i].intValue());
                    obs.addLevel(lvl);
                }
            }

            numLvls = pdv.getNumber("numTrop");
            pr = pdv.getNumberAllLevels("prTrop");
            tp = pdv.getNumberAllLevels("tpTrop");
            td = pdv.getNumberAllLevels("tdTrop");
            wd = pdv.getNumberAllLevels("wdTrop");
            ws = pdv.getNumberAllLevels("wsTrop");

            if ((numLvls != null) && (numLvls.intValue() > 0)) {
                for (int i = 0; i < numLvls.intValue(); i++) {
                    if (pr[i] != null) {
                        if ((pr[i].intValue() > 120000)
                                || (pr[i].intValue() < 0)) {
                            continue;
                        }
                    }
                    UAObsLevel lvl = new UAObsLevel();
                    lvl.setVertSig(LayerTools.TROP_LEVEL);
                    lvl.setPressure(pr[i].intValue());
                    lvl.setTemp(tp[i].doubleValue());
                    lvl.setDwpt(td[i].doubleValue());
                    lvl.setWindSpeed(ws[i].doubleValue());
                    lvl.setWindDirection(wd[i].intValue());
                    obs.addLevel(lvl);
                }
            }

            numLvls = pdv.getNumber("numMwnd");
            pr = pdv.getNumberAllLevels("prMaxW");
            wd = pdv.getNumberAllLevels("wdMaxW");
            ws = pdv.getNumberAllLevels("wsMaxW");

            if ((numLvls != null) && (numLvls.intValue() > 0)) {
                for (int i = 0; i < numLvls.intValue(); i++) {
                    if (pr[i] != null) {
                        if ((pr[i].intValue() > 120000)
                                || (pr[i].intValue() < 0)) {
                            continue;
                        }
                    }
                    UAObsLevel lvl = new UAObsLevel();
                    lvl.setVertSig(LayerTools.MAXWND_LEVEL);
                    lvl.setPressure(pr[i].intValue());
                    lvl.setWindSpeed(ws[i].doubleValue());
                    lvl.setWindDirection(wd[i].intValue());
                    obs.addLevel(lvl);
                }
            }
        }
        return obs;
    }

    /**
     * 
     * @param pdv
     * @param obs
     * @return
     */
    private static UAObs getObsSigTLevelData(PointDataView pdv, UAObs obs) {
        if (obs != null) {

            Number numLvls = pdv.getNumber(LayerTools.NUM_SIGT);
            Number[] pr = pdv.getNumberAllLevels(LayerTools.PR_SIGT);
            Number[] tp = pdv.getNumberAllLevels(LayerTools.TP_SIGT);
            Number[] td = pdv.getNumberAllLevels(LayerTools.TD_SIGT);
            if (numLvls != null) {
                for (int i = 0; i < numLvls.intValue(); i++) {
                    if (pr[i] != null) {
                        if ((pr[i].intValue() > 120000)
                                || (pr[i].intValue() < 0)) {
                            continue;
                        }
                    }
                    UAObsLevel lvl = new UAObsLevel();
                    lvl.setVertSig(LayerTools.SIGPRE_LEVEL);
                    lvl.setPressure(pr[i].intValue());
                    lvl.setTemp(tp[i].doubleValue());
                    lvl.setDwpt(td[i].doubleValue());
                    obs.addLevel(lvl);
                }
            }
        }
        return obs;
    }

    /**
     * 
     * @param pdv
     * @param obs
     * @return
     */
    private static UAObs getObsSigWLevelData(PointDataView pdv, UAObs obs) {
        if (obs != null) {

            Number numLvls = pdv.getNumber(LayerTools.NUM_SIGW);
            Number[] ht = pdv.getNumberAllLevels(LayerTools.HT_SIGW);
            Number[] wd = pdv.getNumberAllLevels(LayerTools.WD_SIGW);
            Number[] ws = pdv.getNumberAllLevels(LayerTools.WS_SIGW);
            if (numLvls != null) {
                for (int i = 0; i < numLvls.intValue(); i++) {
                    if (ht[i] != null) {
                        // TODO : remove the surface data for now
                        if (ht[i].intValue() == 0) {
                            continue;
                        }
                    }

                    UAObsLevel lvl = new UAObsLevel();
                    lvl.setVertSig(LayerTools.SIGWND_LEVEL);
                    lvl.setGeoHeight(ht[i].intValue());
                    lvl.setWindSpeed(ws[i].doubleValue());
                    lvl.setWindDirection(wd[i].intValue());
                    obs.addLevel(lvl);
                }
            }
        }
        return obs;
    }

    /**
     * 
     * @param container
     * @return
     */
    public static UAObs[] toUAObsRecords(PointDataContainer container) {
        List<UAObs> records = new ArrayList<UAObs>();
        container.setCurrentSz(container.getAllocatedSz());
        for (int i = 0; i < container.getCurrentSz(); i++) {
            PointDataView pdv = container.readRandom(i);

            UAObs obs = getUAObsHdr(pdv);
            if (obs != null) {

                switch (obs.getReportType()) {

                case LayerTools.MANLVL_LO:
                case LayerTools.MANLVL_HI: {
                    obs = getManUAObsRecord(pdv, obs);
                    break;
                }

                case LayerTools.SIGTLVL_LO:
                case LayerTools.SIGTLVL_HI: {
                    obs = getObsSigTLevelData(pdv, obs);
                    break;
                }

                case LayerTools.SIGWLVL_LO:
                case LayerTools.SIGWLVL_HI: {
                    obs = getObsSigWLevelData(pdv, obs);
                    break;
                }
                }
                records.add(obs);
            }
        }
        return records.toArray(new UAObs[records.size()]);
    }

}
