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
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 19, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class BufrUAPointDataTransform {

    /** The logger */
    private static Log logger = LogFactory.getLog(BufrUAPointDataTransform.class);

    public static final String HDR_PARAMS_LIST;
    static {
        StringBuffer sb = new StringBuffer();

        sb.append("wmoStaNum,");
        sb.append("staName,");
        sb.append("validTime,");
        sb.append("relTime,");
        sb.append("staElev,");
        sb.append("latitude,");
        sb.append("longitude,");
        sb.append("dataURI,");
        sb.append("sfcPressure,");
        sb.append("rptType,");
        
        
        HDR_PARAMS_LIST = sb.toString();
    }
    
    public static final String MAN_PARAMS_LIST;
    static {
        StringBuffer sb = new StringBuffer();
        sb.append(HDR_PARAMS_LIST);
        sb.append("numMand,");
        sb.append("prMan,");
        sb.append("htMan,");
        sb.append("tpMan,");
        sb.append("tdMan,");
        sb.append("wdMan,");
        sb.append("wsMan,");
        //-------------------------
        sb.append("numTrop,");
        sb.append("prTrop,");
        sb.append("tpTrop,");
        sb.append("tdTrop,");
        sb.append("wdTrop,");
        sb.append("wsTrop,");
        //-------------------------
        sb.append("numMwnd,");
        sb.append("prMaxW,");
        sb.append("wdMaxW,");
        sb.append("wsMaxW,");
        //-------------------------
        sb.append("numSigT,");
        sb.append("prSigT,");
        sb.append("tpSigT,");
        sb.append("tdSigT,");
        //-------------------------
        sb.append("numSigW,");
        sb.append("htSigW,");
        sb.append("wdSigW,");
        sb.append("wsSigW");
        //-------------------------
        MAN_PARAMS_LIST = sb.toString();
    }

    private static UAObs getUAObsHdr(PointDataView pdv) {
        UAObs obs = null;
        if (pdv != null) {

            String uri = pdv.getString("dataURI");
            logger.debug("URI = " + uri);
            obs = new UAObs(uri);

            long vt = pdv.getNumber("validTime").longValue();
            obs.setValidTime(TimeTools.newCalendar(vt));

            obs.setRefHour(TimeTools.newCalendar(vt));
            obs.setDataTime(new DataTime(TimeTools.newCalendar(vt)));

            SurfaceObsLocation location = new SurfaceObsLocation();

            int elev = pdv.getNumber("staElev").intValue();
            location.setElevation(elev);
            double lat = pdv.getNumber("latitude").doubleValue();
            double lon = pdv.getNumber("longitude").doubleValue();
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
        if(obs != null) {
            Number numLvls = pdv.getNumber("numMand");
            Number[] pr = pdv.getNumberAllLevels("prMan");
            Number[] ht = pdv.getNumberAllLevels("htMan");
            Number[] tp = pdv.getNumberAllLevels("tpMan");
            Number[] td = pdv.getNumberAllLevels("tdMan");
            Number[] wd = pdv.getNumberAllLevels("wdMan");
            Number[] ws = pdv.getNumberAllLevels("wsMan");

            if ((numLvls != null)&&(numLvls.intValue() > 0)) {
                for (int i = 0; i < numLvls.intValue(); i++) {
                    if (pr[i] != null) {
                        if ((pr[i].intValue() > 120000)
                                || (pr[i].intValue() < 0)) {
                            continue;
                        }
                    }
                    UAObsLevel lvl = new UAObsLevel();
                    if(pr[i].intValue() == obs.getPressure_station()) {
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

            if ((numLvls != null)&&(numLvls.intValue() > 0)) {
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

            if ((numLvls != null)&&(numLvls.intValue() > 0)) {
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
    public static UAObs [] toUAObsRecords(PointDataContainer container) {
        List<UAObs> records = new ArrayList<UAObs>();
        container.setCurrentSz(container.getAllocatedSz());
        for (int i = 0; i < container.getCurrentSz(); i++) {
            PointDataView pdv = container.readRandom(i);
            
            UAObs obs = getUAObsHdr(pdv);
            if(obs != null) {
                
                switch(obs.getReportType()) {
                
                case LayerTools.MANLVL_LO :
                case LayerTools.MANLVL_HI : {
                    obs = getManUAObsRecord(pdv,obs);
                    break;
                }
                
                case LayerTools.SIGTLVL_LO :
                case LayerTools.SIGTLVL_HI : {
                    obs = getObsSigTLevelData(pdv,obs);
                    break;
                }
                
                case LayerTools.SIGWLVL_LO :
                case LayerTools.SIGWLVL_HI : {
                    obs = getObsSigWLevelData(pdv,obs);
                    break;
                }
                } 
                records.add(obs);
            }
        }
        return records.toArray(new UAObs[records.size()]);
    }

}
