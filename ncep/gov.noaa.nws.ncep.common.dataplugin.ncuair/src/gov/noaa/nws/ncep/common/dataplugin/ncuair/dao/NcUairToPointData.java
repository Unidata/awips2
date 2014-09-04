/**
 * NcUairToPointData
 * 
 * This java class provides the data transform from NcUairRecord to 
 * point data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    	Engineer    	Description
 * ------------ ---------- 	----------- 	--------------------------
 * 4/2011				   	T. Lee			Created
 * 7/2011					T. Lee			
 * 09/2011      457         S. Gurung       Renamed H5 to Nc and h5 to nc
 * 09/2011                  Chin Chen       support batch decoding methods for better performance and
 * 											remove xml serialization as well * 
 * 10/2011                  S. Gurung       Added changes related to getting stid/lat/lon/elev 
 * 										    from database instead of snstns.xml file
 *  6/2014                  T.Lee        Added support XXAA, XXBB, XXCC, XXDD
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system. 
 * @author T. Lee
 * @version 1.0
 */

package gov.noaa.nws.ncep.common.dataplugin.ncuair.dao;

import gov.noaa.nws.ncep.common.dataplugin.ncuair.NcUairLiftedIndex;
import gov.noaa.nws.ncep.common.dataplugin.ncuair.NcUairMaxWind;
import gov.noaa.nws.ncep.common.dataplugin.ncuair.NcUairObsLevels;
import gov.noaa.nws.ncep.common.dataplugin.ncuair.NcUairRecord;
import gov.noaa.nws.ncep.common.dataplugin.ncuair.NcUairTropopause;

import java.io.File;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.pointdata.Dimension;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;

public class NcUairToPointData {
    private static final String RAW_DATA = "RAWDATA";

    private static final String UTC = "UTC";

    private static final String REPORT_TYPE = "REPORTTYPE";

    private static final String STATION_ID = "STATIONID";

    private static final String ISSUE_TIME = "ISSUETIME";

    private static final String OBS_TIME = "OBSTIME";

    private static final String SYNOPTIC_TIME = "SYNOPTIME";

    private static final String DATAURI = "DATAURI";

    private static final String DATA_TYPE = "DATATYPE";

    private static final String CORRECTION_INDICATOR = "CORR";

    private static final String WMO_HEADER = "WMOHEADER";

    private static final String STATION_NUMBER = "STNUM";

    private static final String LATITUDE = "LATITUDE";

    private static final String LONGITUDE = "LONGITUDE";

    private static final String ELEVATION = "ELEVATION";

    private static final String NIL = "NIL";

    private static final String TTAA_PRES = "TTAA_PRES";

    private static final String TTAA_TEMP = "TTAA_TEMP";

    private static final String TTAA_DWPT = "TTAA_DWPT";

    private static final String TTAA_DRCT = "TTAA_DRCT";

    private static final String TTAA_SPED = "TTAA_SPED";

    private static final String TTAA_HGHT = "TTAA_HGHT";

    private static final String TTBB_PRES = "TTBB_PRES";

    private static final String TTBB_TEMP = "TTBB_TEMP";

    private static final String TTBB_DWPT = "TTBB_DWPT";

    private static final String TTCC_PRES = "TTCC_PRES";

    private static final String TTCC_TEMP = "TTCC_TEMP";

    private static final String TTCC_DWPT = "TTCC_DWPT";

    private static final String TTCC_DRCT = "TTCC_DRCT";

    private static final String TTCC_SPED = "TTCC_SPED";

    private static final String TTCC_HGHT = "TTCC_HGHT";

    private static final String TTDD_PRES = "TTDD_PRES";

    private static final String TTDD_TEMP = "TTDD_TEMP";

    private static final String TTDD_DWPT = "TTDD_DWPT";

    private static final String PPAA_PRES = "PPAA_PRES";

    private static final String PPAA_TEMP = "PPAA_TEMP";

    private static final String PPAA_DWPT = "PPAA_DWPT";

    private static final String PPAA_DRCT = "PPAA_DRCT";

    private static final String PPAA_SPED = "PPAA_SPED";

    private static final String PPAA_HGHT = "PPAA_HGHT";

    private static final String PPBB_DRCT = "PPBB_DRCT";

    private static final String PPBB_SPED = "PPBB_SPED";

    private static final String PPBB_HGHT = "PPBB_HGHT";

    private static final String PPCC_PRES = "PPCC_PRES";

    private static final String PPCC_TEMP = "PPCC_TEMP";

    private static final String PPCC_DWPT = "PPCC_DWPT";

    private static final String PPCC_DRCT = "PPCC_DRCT";

    private static final String PPCC_SPED = "PPCC_SPED";

    private static final String PPCC_HGHT = "PPCC_HGHT";

    private static final String PPDD_DRCT = "PPDD_DRCT";

    private static final String PPDD_SPED = "PPDD_SPED";

    private static final String PPDD_HGHT = "PPDD_HGHT";

    private static final String TROP_PRES = "TROP_PRES";

    private static final String TROP_TEMP = "TROP_TEMP";

    private static final String TROP_DWPT = "TROP_DWPT";

    private static final String TROP_DRCT = "TROP_DRCT";

    private static final String TROP_SPED = "TROP_SPED";

    private static final String WMAX_PRES = "WMAX_PRES";

    private static final String WMAX_DRCT = "WMAX_DRCT";

    private static final String WMAX_SPED = "WMAX_SPED";

    private static final String WMAX_LO_SHEAR = "WMAX_LO_SHEAR";

    private static final String WMAX_HI_SHEAR = "WMAX_HI_SHEAR";

    private static final String TTAA_LIFT = "TTAA_LIFT";

    private static final String TTAA_LO_MEAN_DRCT = "TTAA_LO_MEAN_DRCT";

    private static final String TTAA_LO_MEAN_SPED = "TTAA_LO_MEAN_SPED";

    private static final String TTAA_HI_MEAN_DRCT = "TTAA_HI_MEAN_DRCT";

    private static final String TTAA_HI_MEAN_SPED = "TTAA_HI_MEAN_SPED";

    private static final String TTBB_LIFT = "TTBB_LIFT";

    private static final String TTBB_LO_MEAN_DRCT = "TTBB_LO_MEAN_DRCT";

    private static final String TTBB_LO_MEAN_SPED = "TTBB_LO_MEAN_SPED";

    private static final String TTBB_HI_MEAN_DRCT = "TTBB_HI_MEAN_DRCT";

    private static final String TTBB_HI_MEAN_SPED = "TTBB_HI_MEAN_SPED";

    private static final String XXAA_PRES = "XXAA_PRES";

    private static final String XXAA_TEMP = "XXAA_TEMP";

    private static final String XXAA_DWPT = "XXAA_DWPT";

    private static final String XXAA_DRCT = "XXAA_DRCT";

    private static final String XXAA_SPED = "XXAA_SPED";

    private static final String XXAA_HGHT = "XXAA_HGHT";

    private static final String XXBB_PRES = "XXBB_PRES";

    private static final String XXBB_TEMP = "XXBB_TEMP";

    private static final String XXBB_DWPT = "XXBB_DWPT";

    private static final String XXCC_PRES = "XXCC_PRES";

    private static final String XXCC_TEMP = "XXCC_TEMP";

    private static final String XXCC_DWPT = "XXCC_DWPT";

    private static final String XXCC_DRCT = "XXCC_DRCT";

    private static final String XXCC_SPED = "XXCC_SPED";

    private static final String XXCC_HGHT = "XXCC_HGHT";

    private static final String XXDD_PRES = "XXDD_PRES";

    private static final String XXDD_TEMP = "XXDD_TEMP";

    private static final String XXDD_DWPT = "XXDD_DWPT";

    private enum REPORTTYPE {
        TTAA, TTBB, TTCC, TTDD, PPAA, PPBB, PPCC, PPDD, UUAA, UUBB, UUCC, UUDD, XXAA, XXBB, XXCC, XXDD
    };

    /**
     * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! It is important to
     * keep this up to date or risk breaking backwards compatibility uu
     */
    private static final String[] ALL_PARAMS = { RAW_DATA, UTC, REPORT_TYPE,
            STATION_ID, ISSUE_TIME, OBS_TIME, SYNOPTIC_TIME, DATAURI,
            DATA_TYPE, CORRECTION_INDICATOR, WMO_HEADER, STATION_NUMBER,
            LATITUDE, LONGITUDE, ELEVATION, NIL, TTAA_PRES, TTAA_TEMP,
            TTAA_DWPT, TTAA_DRCT, TTAA_SPED, TTAA_HGHT, TTBB_PRES, TTBB_TEMP,
            TTBB_DWPT, TTCC_PRES, TTCC_TEMP, TTCC_DWPT, TTCC_DRCT, TTCC_SPED,
            TTCC_HGHT, TTDD_PRES, TTDD_TEMP, TTDD_DWPT, PPAA_PRES, PPAA_TEMP,
            PPAA_DWPT, PPAA_DRCT, PPAA_SPED, PPAA_HGHT, PPBB_DRCT, PPBB_SPED,
            PPBB_HGHT, PPCC_PRES, PPCC_TEMP, PPCC_DWPT, PPCC_DRCT, PPCC_SPED,
            PPCC_HGHT, PPDD_DRCT, PPDD_SPED, PPDD_HGHT, TROP_PRES, TROP_TEMP,
            TROP_DWPT, TROP_DRCT, TROP_SPED, WMAX_PRES, WMAX_DRCT, WMAX_SPED,
            WMAX_LO_SHEAR, WMAX_HI_SHEAR, TTAA_LIFT, TTAA_LO_MEAN_DRCT,
            TTAA_LO_MEAN_SPED, TTAA_HI_MEAN_DRCT, TTAA_HI_MEAN_SPED, TTBB_LIFT,
            TTBB_LO_MEAN_DRCT, TTBB_LO_MEAN_SPED, TTBB_HI_MEAN_DRCT,
            TTAA_HI_MEAN_SPED, XXAA_PRES, XXAA_TEMP, XXAA_DWPT, XXAA_DRCT,
            XXAA_SPED, XXAA_HGHT, XXBB_PRES, XXBB_TEMP, XXBB_DWPT, XXCC_PRES,
            XXCC_TEMP, XXCC_DWPT, XXCC_DRCT, XXCC_SPED, XXCC_HGHT, XXDD_PRES,
            XXDD_TEMP, XXDD_DWPT };

    public static final String ALL_PARAMS_LIST;
    static {
        StringBuffer sb = new StringBuffer();
        boolean first = true;
        for (String s : ALL_PARAMS) {
            if (!first) {
                sb.append(", ");
            } else {
                first = false;
            }
            sb.append(s);
        }
        ALL_PARAMS_LIST = sb.toString();
    }

    private NcUairDao dao;

    private PointDataDescription pdd;

    public NcUairToPointData() {
        try {
            this.dao = new NcUairDao("ncuair");
            this.pdd = dao.getPointDataDescription();
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public PluginDataObject[] toPointData(PluginDataObject[] pdo) {
        // long curTime = System.currentTimeMillis();
        // System.out.println("H5 uair toPointData entered, pdo size = "+
        // pdo.length);
        if (pdo.length > 0) {
            Map<File, PointDataContainer> pointMap = new HashMap<File, PointDataContainer>();

            for (PluginDataObject p : pdo) {
                if (!(p instanceof NcUairRecord))
                    continue;
                File f = this.dao.getFullFilePath(p);
                PointDataContainer pdc = pointMap.get(f);

                if (pdc == null) {
                    pdc = PointDataContainer.build(this.pdd);
                    pointMap.put(f, pdc);
                }
                NcUairRecord mr = (NcUairRecord) p;
                PointDataView pdv = buildView(pdc, mr);
                mr.setPointDataView(pdv);

            }
        }
        // long enqueueTime = System.currentTimeMillis();
        // double latency = (enqueueTime-curTime) ;
        // System.out.println("H5 uair toPointData spend "+ latency);
        return pdo;
    }

    private PointDataView buildView(PointDataContainer container,
            NcUairRecord record) {

        PointDataView pdv = container.append();
        pdv.setString(STATION_ID, record.getStationId());

        int maxManLevel = -1;
        int maxSigTempLevel = -1;
        int maxSigWindLevel = -1;
        int maxWmaxLevel = -1;
        int maxTropLevel = -1;
        int maxMiscLevel = -1;

        Dimension[] dims = pdd.dimensions;
        for (Dimension d : dims) {

            if ("maxManLevel".equals(d.getDimensionName())) {
                maxManLevel = d.getDimensionLength();
            }

            if ("maxSigTempLevel".equals(d.getDimensionName())) {
                maxSigTempLevel = d.getDimensionLength();
            }

            if ("maxSigWindLevel".equals(d.getDimensionName())) {
                maxSigWindLevel = d.getDimensionLength();
            }

            if ("maxWmaxLevel".equals(d.getDimensionName())) {
                maxWmaxLevel = d.getDimensionLength();
            }

            if ("maxTropLevel".equals(d.getDimensionName())) {
                maxTropLevel = d.getDimensionLength();
            }

            if ("maxMiscLevel".equals(d.getDimensionName())) {
                maxMiscLevel = d.getDimensionLength();
            }
        }

        if (record.getCorr() != null) {
            pdv.setString(CORRECTION_INDICATOR, record.getCorr());
        } else {
            pdv.setString(CORRECTION_INDICATOR, "");
        }

        if (record.getLocation() != null) {
            pdv.setFloat(LATITUDE, (float) record.getLatitude());
            pdv.setFloat(LONGITUDE, (float) record.getLongitude());
            if (record.getElevation() != null)
                pdv.setFloat(ELEVATION, (float) record.getElevation());
        }
        if (record.getObsTime() != null)
            pdv.setLong(OBS_TIME, record.getObsTime().getTime().getTime());
        if (record.getSynopticTime() != null)
            pdv.setLong(SYNOPTIC_TIME, record.getSynopticTime().getTime()
                    .getTime());
        if (record.getIssueTime() != null)
            pdv.setLong(ISSUE_TIME, record.getIssueTime().getTime().getTime());
        pdv.setString(DATAURI, record.getDataURI());
        pdv.setString(DATA_TYPE, record.getDataType());
        pdv.setString(REPORT_TYPE, record.getReportType());
        pdv.setString(WMO_HEADER, record.getWmoHeader());
        pdv.setString(STATION_NUMBER, record.getStnum());
        if (record.getNil() != null)
            pdv.setString(NIL, record.getNil().toString());
        pdv.setInt(UTC, record.getUTC());
        pdv.setString(RAW_DATA, record.getBullMessage());
        int index;

        if (record.getObsLevels() != null) {
            Iterator<NcUairObsLevels> sls = record.getObsLevels().iterator();
            switch (REPORTTYPE.valueOf(record.getDataType())) {

            case TTAA:
            case UUAA:

                index = 0;
                if (sls != null) {
                    while (sls.hasNext()) {
                        NcUairObsLevels sl = sls.next();
                        if (sl.getPres() > 0 && index < maxManLevel) {
                            pdv.setFloat(TTAA_PRES, sl.getPres(), index);
                            pdv.setFloat(TTAA_HGHT, sl.getHght(), index);
                            pdv.setFloat(TTAA_TEMP, sl.getTemp(), index);
                            pdv.setFloat(TTAA_DWPT, sl.getDwpt(), index);
                            pdv.setFloat(TTAA_DRCT, sl.getDrct(), index);
                            pdv.setFloat(TTAA_SPED, sl.getSped(), index);
                            index++;
                        }
                    }
                    pdv.setInt("numTTAA", index);
                }

                break;
            case XXAA:
                index = 0;
                if (sls != null) {
                    while (sls.hasNext()) {
                        NcUairObsLevels sl = sls.next();
                        if (sl.getPres() > 0 && index < maxManLevel) {
                            pdv.setFloat(XXAA_PRES, sl.getPres(), index);
                            pdv.setFloat(XXAA_HGHT, sl.getHght(), index);
                            pdv.setFloat(XXAA_TEMP, sl.getTemp(), index);
                            pdv.setFloat(XXAA_DWPT, sl.getDwpt(), index);
                            pdv.setFloat(XXAA_DRCT, sl.getDrct(), index);
                            pdv.setFloat(XXAA_SPED, sl.getSped(), index);
                            index++;
                        }
                    }
                    pdv.setInt("numXXAA", index);
                }

                break;

            case TTBB:
            case UUBB:

                index = 0;
                if (sls != null) {
                    while (sls.hasNext()) {
                        NcUairObsLevels sl = sls.next();
                        if (sl.getPres() > 0 && index < maxSigTempLevel) {
                            pdv.setFloat(TTBB_PRES, sl.getPres(), index);
                            pdv.setFloat(TTBB_TEMP, sl.getTemp(), index);
                            pdv.setFloat(TTBB_DWPT, sl.getDwpt(), index);
                            index++;
                        }
                    }
                    pdv.setInt("numTTBB", index);
                }
                break;
            case XXBB:
                index = 0;
                if (sls != null) {
                    while (sls.hasNext()) {
                        NcUairObsLevels sl = sls.next();
                        if (sl.getPres() > 0 && index < maxSigTempLevel) {
                            pdv.setFloat(XXBB_PRES, sl.getPres(), index);
                            pdv.setFloat(XXBB_TEMP, sl.getTemp(), index);
                            pdv.setFloat(XXBB_DWPT, sl.getDwpt(), index);
                            index++;
                        }
                    }
                    pdv.setInt("numXXBB", index);
                }
                break;

            case TTCC:
            case UUCC:

                index = 0;
                if (sls != null) {
                    while (sls.hasNext()) {
                        NcUairObsLevels sl = sls.next();
                        if (sl.getPres() > 0 && index < maxManLevel) {

                            pdv.setFloat(TTCC_PRES, sl.getPres(), index);
                            pdv.setFloat(TTCC_HGHT, sl.getHght(), index);
                            pdv.setFloat(TTCC_TEMP, sl.getTemp(), index);
                            pdv.setFloat(TTCC_DWPT, sl.getDwpt(), index);
                            pdv.setFloat(TTCC_DRCT, sl.getDrct(), index);
                            pdv.setFloat(TTCC_SPED, sl.getSped(), index);
                            index++;
                        }
                    }
                    pdv.setInt("numTTCC", index);
                }
                break;
            case XXCC:
                index = 0;
                if (sls != null) {
                    while (sls.hasNext()) {
                        NcUairObsLevels sl = sls.next();
                        if (sl.getPres() > 0 && index < maxManLevel) {

                            pdv.setFloat(XXCC_PRES, sl.getPres(), index);
                            pdv.setFloat(XXCC_HGHT, sl.getHght(), index);
                            pdv.setFloat(XXCC_TEMP, sl.getTemp(), index);
                            pdv.setFloat(XXCC_DWPT, sl.getDwpt(), index);
                            pdv.setFloat(XXCC_DRCT, sl.getDrct(), index);
                            pdv.setFloat(XXCC_SPED, sl.getSped(), index);
                            index++;
                        }
                    }
                    pdv.setInt("numXXCC", index);
                }
                break;

            case TTDD:
            case UUDD:

                index = 0;
                if (sls != null) {
                    while (sls.hasNext()) {
                        NcUairObsLevels sl = sls.next();
                        if (sl.getPres() > 0 && index < maxSigTempLevel) {
                            pdv.setFloat(TTDD_PRES, sl.getPres(), index);
                            pdv.setFloat(TTDD_TEMP, sl.getTemp(), index);
                            pdv.setFloat(TTDD_DWPT, sl.getDwpt(), index);
                            index++;
                        }
                    }
                    pdv.setInt("numTTDD", index);
                }
                break;
            case XXDD:
                index = 0;
                if (sls != null) {
                    while (sls.hasNext()) {
                        NcUairObsLevels sl = sls.next();
                        if (sl.getPres() > 0 && index < maxSigTempLevel) {
                            pdv.setFloat(XXDD_PRES, sl.getPres(), index);
                            pdv.setFloat(XXDD_TEMP, sl.getTemp(), index);
                            pdv.setFloat(XXDD_DWPT, sl.getDwpt(), index);
                            index++;
                        }
                    }
                    pdv.setInt("numXXDD", index);
                }
                break;

            case PPAA:
                index = 0;
                if (sls != null) {
                    while (sls.hasNext()) {
                        NcUairObsLevels sl = sls.next();
                        if (sl.getPres() > 0 && index < maxManLevel) {
                            pdv.setFloat(PPAA_PRES, sl.getPres(), index);
                            pdv.setFloat(PPAA_HGHT, sl.getHght(), index);
                            pdv.setFloat(PPAA_TEMP, sl.getTemp(), index);
                            pdv.setFloat(PPAA_DWPT, sl.getDwpt(), index);
                            pdv.setFloat(PPAA_DRCT, sl.getDrct(), index);
                            pdv.setFloat(PPAA_SPED, sl.getSped(), index);
                            index++;
                        }
                    }
                    pdv.setInt("numPPAA", index);
                }
                break;
            case PPBB:
                index = 0;
                if (sls != null) {
                    while (sls.hasNext()) {
                        NcUairObsLevels sl = sls.next();
                        if (sl.getHght() > 0 && index < maxSigWindLevel) {
                            pdv.setFloat(PPBB_HGHT, sl.getHght(), index);
                            pdv.setFloat(PPBB_DRCT, sl.getDrct(), index);
                            pdv.setFloat(PPBB_SPED, sl.getSped(), index);
                            index++;
                        }
                    }
                    pdv.setInt("numPPBB", index);
                }
                break;

            case PPCC:
                index = 0;
                if (sls != null) {
                    while (sls.hasNext()) {
                        NcUairObsLevels sl = sls.next();
                        if ((sl.getPres() > 0 || sl.getHght() > 0)
                                && index < maxManLevel) {
                            pdv.setFloat(PPCC_PRES, sl.getPres(), index);
                            pdv.setFloat(PPCC_HGHT, sl.getHght(), index);
                            pdv.setFloat(PPCC_TEMP, sl.getTemp(), index);
                            pdv.setFloat(PPCC_DWPT, sl.getDwpt(), index);
                            pdv.setFloat(PPCC_DRCT, sl.getDrct(), index);
                            pdv.setFloat(PPCC_SPED, sl.getSped(), index);
                            index++;

                        }
                    }
                    pdv.setInt("numPPCC", index);
                }
                break;

            case PPDD:
                index = 0;
                if (sls != null) {
                    while (sls.hasNext()) {
                        NcUairObsLevels sl = sls.next();
                        if (sl.getHght() > 0 && index < maxSigWindLevel) {
                            pdv.setFloat(PPDD_HGHT, sl.getHght(), index);
                            pdv.setFloat(PPDD_DRCT, sl.getDrct(), index);
                            pdv.setFloat(PPDD_SPED, sl.getSped(), index);
                            index++;
                        }

                    }
                    pdv.setInt("numPPDD", index);
                }
            }
        }

        if (record.getTropopause() != null) {
            Iterator<NcUairTropopause> trops = record.getTropopause()
                    .iterator();
            index = 0;
            if (trops != null) {
                while (trops.hasNext()) {
                    NcUairTropopause trop = trops.next();
                    if (trop.getPres() > 0 && index < maxTropLevel) {
                        pdv.setFloat(TROP_PRES, trop.getPres(), index);
                        pdv.setFloat(TROP_TEMP, trop.getTemp(), index);
                        pdv.setFloat(TROP_DWPT, trop.getDwpt(), index);
                        pdv.setFloat(TROP_DRCT, trop.getDrct(), index);
                        pdv.setFloat(TROP_SPED, trop.getSped(), index);
                        index++;
                    }
                }
                pdv.setInt("numTrop", index);
            }
        }

        if (record.getMaxWind() != null) {
            Iterator<NcUairMaxWind> wmaxs = record.getMaxWind().iterator();
            index = 0;
            if (wmaxs != null) {
                while (wmaxs.hasNext()) {
                    NcUairMaxWind wmax = wmaxs.next();
                    if (wmax.getPres() > 0 && index < maxWmaxLevel) {
                        pdv.setFloat(WMAX_PRES, wmax.getPres(), index);
                        pdv.setFloat(WMAX_DRCT, wmax.getDrct(), index);
                        pdv.setFloat(WMAX_SPED, wmax.getSped(), index);
                        pdv.setFloat(WMAX_LO_SHEAR, wmax.getLoShear(), index);
                        pdv.setFloat(WMAX_HI_SHEAR, wmax.getHiShear(), index);
                        index++;
                    }
                }
                pdv.setInt("numWmax", index);
            }
        }

        if (record.getLiftedIndex() != null) {
            Iterator<NcUairLiftedIndex> miscs = record.getLiftedIndex()
                    .iterator();
            index = 0;
            if (miscs != null) {
                while (miscs.hasNext()) {
                    NcUairLiftedIndex misc = miscs.next();
                    if (index < maxMiscLevel) {
                        if (record.getDataType().equals("TTAA")) {
                            pdv.setFloat(TTAA_LIFT, misc.getLiTemp(), index);
                            pdv.setFloat(TTAA_LO_MEAN_DRCT, misc.getLoDrct(),
                                    index);
                            pdv.setFloat(TTAA_LO_MEAN_SPED, misc.getLoSped(),
                                    index);
                            pdv.setFloat(TTAA_HI_MEAN_DRCT, misc.getHiDrct(),
                                    index);
                            pdv.setFloat(TTAA_HI_MEAN_SPED, misc.getHiSped(),
                                    index);
                        } else {
                            pdv.setFloat(TTBB_LIFT, misc.getLiTemp(), index);
                            pdv.setFloat(TTBB_LO_MEAN_DRCT, misc.getLoDrct(),
                                    index);
                            pdv.setFloat(TTBB_LO_MEAN_SPED, misc.getLoSped(),
                                    index);
                            pdv.setFloat(TTBB_HI_MEAN_DRCT, misc.getHiDrct(),
                                    index);
                            pdv.setFloat(TTBB_HI_MEAN_SPED, misc.getHiSped(),
                                    index);
                        }
                        index++;
                    }
                }
                if (record.getDataType().equals("TTAA")) {
                    pdv.setInt("numMiscTTAA", index);
                } else {
                    pdv.setInt("numMiscTTBB", index);
                }
            }
        }
        return pdv;
    }
}