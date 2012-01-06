/**
 * This software was modified from Raytheon's obs decoder plugin by NOAA/NWS/
 * NCEP/NCO in order to output point data in HDF5,
 **/
package gov.noaa.nws.ncep.common.dataplugin.h5scd.dao;

import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import gov.noaa.nws.ncep.common.dataplugin.h5scd.H5ScdRecord;
import gov.noaa.nws.ncep.common.dataplugin.h5scd.dao.H5ScdDao;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * Provides a transform from H5ScdRecords to PointDataContainer and vice versa.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/29/2011				F. J. Yen	Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class H5ScdPointDataTransform {

    private static final String STATION_ID = "stationId";

    private static final String REPORT_TYPE = "reportType";

    private static final String ISSUE_TIME = "issueTime";
    
    private static final String OBS_TIME = "obsTime";

    // STATION_ID, REPORT_TYPE, ISSUE_TIME, OBS_TIME
    // ------------------
    
    private static final String CORRECTION_CODE = "correctionCode";

    private static final String DATAURI = "dataURI";
    
    private static final String REPORT = "report";

    // CORRECTION_CODE, DATAURI, REPORT
    // ------------------
    
    private static final String TDXC = "TDXC";

    private static final String TDNC = "TDNC";
    
    private static final String P06I = "P06I";
    
    private static final String P24I = "P24I";

    private static final String WTHR = "WTHR";
    
    private static final String SNOW = "SNOW";
    
    private static final String SNEW = "SNEW";
    
    private static final String S24I = "S24I";

    private static final String WEQS = "WEQS";
    
    private static final String MSUN = "MSUN";
    
    private static final String CTYL = "CTYL";

    private static final String CTYM = "CTYM";
    
    private static final String CTYH = "CTYH";
    
    private static final String CFRT = "CFRT";

    private static final String CFRL = "CFRL";
    
    private static final String CBAS = "CBAS";
    
    private static final String SUSPECT_TIME_FLAG = "suspectTimeFlag";
    
    //  TDXC, TDNC, P06I, P24I, WTHR, SNOW, SNEW, S24I, WEQS, MSUN,
    //  CTYL, CTYM, CTYH, CFRT, CFRL, CBAS, SUSPECT_TIME_FLAG
    // ------------------


    /**
     * It is important to keep this up to date or risk breaking backwards
     * compatibility
     */
    private static final String[] ALL_PARAMS = { STATION_ID, REPORT_TYPE, 
    	    ISSUE_TIME, OBS_TIME, CORRECTION_CODE, REPORT, DATAURI, TDXC,
            TDNC, P06I, P24I, WTHR, SNOW, SNEW, S24I, WEQS, MSUN, CTYL, 
            CTYM, CTYH, CFRT, CFRL, CBAS, SUSPECT_TIME_FLAG,};

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

    private H5ScdDao dao;

    private PointDataDescription description;

    public H5ScdPointDataTransform() {
        try {
            this.dao = new H5ScdDao("h5scd");
            this.description = dao.getPointDataDescription(null);
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public PluginDataObject[] toPointData(PluginDataObject[] pdo) {

        if (pdo.length > 0) {
            Map<File, PointDataContainer> pointMap = new HashMap<File, PointDataContainer>();
            for (PluginDataObject p : pdo) {
                if (!(p instanceof H5ScdRecord))
                    continue;
                File f = this.dao.getFullFilePath(p);
                PointDataContainer pdc = pointMap.get(f);
                if (pdc == null) {
                    pdc = PointDataContainer.build(this.description);
                    pointMap.put(f, pdc);
                }
                H5ScdRecord nar = (H5ScdRecord) p;
                PointDataView pdv = buildView(pdc, nar);
                nar.setPointDataView(pdv);
            }
        }
        return pdo;
    }

    private PointDataView buildView(PointDataContainer container,
            H5ScdRecord record) {
        PointDataView pdv = container.append();
        pdv.setLong(OBS_TIME, record.getObsTime().getTime().getTime());
        pdv.setString(REPORT, record.getReport());
        pdv.setString(REPORT_TYPE, record.getReportType());
        pdv.setString(REPORT, record.getReport());
        pdv.setFloat(TDXC, record.getTDXC());
        pdv.setFloat(TDNC, record.getTDNC());
        pdv.setFloat(P06I, record.getP06I());
        pdv.setFloat(P24I, record.getP24I());
        pdv.setString(WTHR, record.getWTHR());
        pdv.setFloat(SNOW, record.getSNOW());
        pdv.setFloat(SNEW, record.getSNEW());
        pdv.setFloat(S24I, record.getS24I());
        pdv.setFloat(WEQS, record.getWEQS());
        pdv.setInt(MSUN, record.getMSUN());
        pdv.setInt(CTYL, record.getCTYL());
        pdv.setInt(CTYM, record.getCTYM());
        pdv.setInt(CTYH, record.getCTYH());
        pdv.setInt(CFRT, record.getCFRT());
        pdv.setInt(CFRL, record.getCFRL());
        pdv.setInt(CBAS, record.getCBAS());
        pdv.setString(SUSPECT_TIME_FLAG, record.getSuspectTimeFlag());
        return pdv;
    }

    public static H5ScdRecord toH5ScdRecord(PointDataView pdv) {
        H5ScdRecord nar = new H5ScdRecord();

        nar.setCorIndicator(pdv.getString(CORRECTION_CODE));
        nar.setDataTime(new DataTime(new Date(pdv.getNumber(OBS_TIME)
                .longValue())));
        nar.setDataURI(pdv.getString(DATAURI));
        nar.setReport(pdv.getString(REPORT));
        nar.setReportType(pdv.getString(REPORT_TYPE));
        nar.setStationID(pdv.getString(STATION_ID));
        
        long tmptime = pdv.getNumber(ISSUE_TIME).longValue();
        nar.setIssueTime(TimeTools.newCalendar(tmptime));
        tmptime = pdv.getNumber(OBS_TIME).longValue();
        nar.setObsTime(TimeTools.newCalendar(tmptime));

        nar.setPluginName("h5scd");
        
        nar.setTDXC(pdv.getNumber(TDXC).floatValue());
        nar.setTDNC(pdv.getNumber(TDNC).floatValue());
        nar.setP06I(pdv.getNumber(P06I).floatValue());
        nar.setP24I(pdv.getNumber(P24I).floatValue());
        nar.setWTHR(pdv.getString(WTHR));
        nar.setSNOW(pdv.getNumber(SNOW).floatValue());
        nar.setSNEW(pdv.getNumber(SNEW).floatValue());
        nar.setS24I(pdv.getNumber(S24I).floatValue());
        nar.setWEQS(pdv.getNumber(WEQS).floatValue());
        
        nar.setMSUN(pdv.getInt(MSUN));
        nar.setCTYL(pdv.getInt(CTYL));
        nar.setCTYM(pdv.getInt(CTYM));
        nar.setCTYH(pdv.getInt(CTYH));        
        nar.setCFRT(pdv.getInt(CFRT));
        nar.setCFRL(pdv.getInt(CFRL));
        nar.setCBAS(pdv.getInt(CBAS));
        nar.setSuspectTimeFlag(pdv.getString(SUSPECT_TIME_FLAG));

        return nar;
    }

    public static H5ScdRecord[] toH5ScdRecords(PointDataContainer container) {
        List<H5ScdRecord> records = new ArrayList<H5ScdRecord>();
        container.setCurrentSz(container.getAllocatedSz());
        for (int i = 0; i < container.getCurrentSz(); i++) {
            PointDataView pdv = container.readRandom(i);
            records.add(toH5ScdRecord(pdv));
        }
        return records.toArray(new H5ScdRecord[records.size()]);
    }
}
