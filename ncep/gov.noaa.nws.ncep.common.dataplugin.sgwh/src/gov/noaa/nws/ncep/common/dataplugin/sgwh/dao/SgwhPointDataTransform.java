/**
 * SgwhPointDataTransform
 * This java class performs the transformation between SwghRecord  Java class to pointdata format.
 * .
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date			Ticket#		Engineer	Description
 * ------------ -----------	----------- --------------------------
 * Aug17 2011	   		    Chin Chen	Initial Coding 
 * </pre>
 * 
 * @author chin chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.common.dataplugin.sgwh.dao;

import gov.noaa.nws.ncep.common.dataplugin.sgwh.SgwhRecord;

import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.time.TimeTools;



public class SgwhPointDataTransform {

    private static final String FORECASTTIME = "FORECASTTIME";

    private static final String OBSTIME = "OBSTIME";

    private static final String REFTIME = "REFTIME";

    private static final String DATAURI = "DATAURI";
    private static final String REPORTTYPE = "REPORTTYPE";

    private static final String WMOHEADER = "WMOHEADER";

    private static final String CLATH = "CLATH";

    private static final String CLONH = "CLONH";
    private static final String SELV = "SELV";

    private static final String HINC = "HINC";

    private static final String SAID = "SAID";

    private static final String SWID = "SWID";

    private static final String OGCE = "OGCE";

    private static final String SSIN1 = "SSIN1";

    private static final String SSIN2 = "SSIN2";

    private static final String ORBN = "ORBN";

    private static final String RSST = "RSST";

    private static final String AETP = "AETP";
    private static final String LSQL = "LSQL";

    private static final String ASFL = "ASFL";

    private static final String RSFL = "RSFL";

    private static final String EENO = "EENO";

    private static final String AFSSGWH = "AFSSGWH";

    private static final String SGWH = "SGWH";

    private static final String FOSTSGWH = "FOSTSGWH";

    private static final String SGWHSTD = "SGWHSTD";

    private static final String NVPP = "NVPP";

    private static final String TOBDG1R1 = "TOBDG1R1";

    private static final String TOBDG1R2 = "TOBDG1R2";

    private static final String AFSBKSTG1R1 = "AFSBKSTG1R1";

    private static final String AFSBKSTG1R2 = "AFSBKSTG1R2";

    private static final String BKSTG1R1 = "BKSTG1R1";

    private static final String BKSTG1R2 = "BKSTG1R2";

    private static final String FOSTBKSTG1R1 = "FOSTBKSTG1R1";

    private static final String FOSTBKSTG1R2 = "FOSTBKSTG1R2";

    private static final String BKSTSTDG1R1 = "BKSTSTDG1R1";

    private static final String BKSTSTDG1R2 = "BKSTSTDG1R2";

    private static final String SONA = "SONA";

    private static final String AFSSONA = "AFSSONA";

    private static final String RWVC = "RWVC";

    private static final String RLQC = "RLQC";

    private static final String AFSSELVG1R1 = "AFSSELVG1R1";

    private static final String AFSSELVG1R2 = "AFSSELVG1R2";

    private static final String SELVG1R1 = "SELVG1R1";
    
    private static final String SELVG1R2 = "SELVG1R2";
    
    private static final String HINCG1R1 = "HINCG1R1";
    
    private static final String HINCG1R2 = "HINCG1R2";
    
    private static final String FOSTSELVG1R1 = "FOSTSELVG1R1";
    
    private static final String FOSTSELVG1R2 = "FOSTSELVG1R2";
    
    private static final String SELVSTDG1R1 = "SELVSTDG1R1";
    
    private static final String SELVSTDG1R2 = "SELVSTDG1R2";
    
    private static final String NVPPG1R1 = "NVPPG1R1";
    
    private static final String NVPPG1R2 = "NVPPG1R2";
    
    private static final String MEFRG2R1 = "MEFRG2R1";
    
    private static final String MEFRG2R2 = "MEFRG2R2";
    
    private static final String MEFRG2R3 = "MEFRG2R3";
    
    private static final String AFSTMBRG2R1 = "AFSTMBRG2R1";
    
    private static final String AFSTMBRG2R2 = "AFSTMBRG2R2";
    
    private static final String AFSTMBRG2R3 = "AFSTMBRG2R3";
    
    private static final String TMBRG2R1 = "TMBRG2R1";
    
    private static final String TMBRG2R2 = "TMBRG2R2";
    
    private static final String TMBRG2R3 = "TMBRG2R3";
    
    private static final String SWCMG3R1 = "SWCMG3R1";
    
    private static final String SWCMG3R2 = "SWCMG3R2";
    
    private static final String WS10G3R1 = "WS10G3R1";
    
    private static final String WS10G3R2 = "WS10G3R2";
    
   /**
     * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! It is important to
     * keep this up to date or risk breaking backwards compatibility
     * 
     */
    private static final String[] ALL_PARAMS = { FORECASTTIME,OBSTIME,REFTIME, DATAURI,REPORTTYPE,
    	WMOHEADER, CLATH, CLONH, SELV, HINC, SAID,SWID,OGCE,SSIN1,SSIN2,ORBN, RSST,AETP,LSQL,ASFL,
    	RSFL, EENO, AFSSGWH,SGWH,FOSTSGWH,SGWHSTD,NVPP,TOBDG1R1, TOBDG1R2, AFSBKSTG1R1, AFSBKSTG1R2,
    	BKSTG1R1, BKSTG1R2,FOSTBKSTG1R1, FOSTBKSTG1R2,BKSTSTDG1R1, BKSTSTDG1R2, SONA,AFSSONA,
    	RWVC,RLQC,AFSSELVG1R1, AFSSELVG1R2, SELVG1R1,SELVG1R2,HINCG1R1, HINCG1R2,FOSTSELVG1R1, FOSTSELVG1R2,
    	SELVSTDG1R1, SELVSTDG1R2,NVPPG1R1, NVPPG1R2,MEFRG2R1, MEFRG2R2,MEFRG2R3,AFSTMBRG2R1,AFSTMBRG2R2,
    	AFSTMBRG2R3,TMBRG2R1,TMBRG2R2,TMBRG2R3,SWCMG3R1,SWCMG3R2,WS10G3R1,WS10G3R2,};

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

    private SgwhDao dao;

    private PointDataDescription description;

    public SgwhPointDataTransform() {
        try {
            this.dao = new SgwhDao("sgwh");
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
                if (!(p instanceof SgwhRecord))
                    continue;

                File f = this.dao.getFullFilePath(p);
                PointDataContainer pdc = pointMap.get(f);
                if (pdc == null) {
                    pdc = PointDataContainer.build(this.description);
                    pointMap.put(f, pdc);
                }

                SgwhRecord mr = (SgwhRecord) p;
                PointDataView pdv = buildView(pdc, mr);
                mr.setPointDataView(pdv);
            }
        }
        return pdo;
    }

    private PointDataView buildView(PointDataContainer container,
    		SgwhRecord record) {
        PointDataView pdv = container.append();
        if(record.getDataTime()!= null)
        	pdv.setLong(FORECASTTIME, record.getDataTime().getFcstTime());
        if(record.getObsTime()!= null)
        	pdv.setLong(OBSTIME, record.getObsTime().getTime().getTime());
        if(record.getDataTime()!= null)
        	pdv.setLong(REFTIME, record.getDataTime().getRefTime().getTime());
        if(record.getDataURI()!= null)    pdv.setString(DATAURI, record.getDataURI());
        if(record.getReportType()!= null)    pdv.setString(REPORTTYPE, record.getReportType());
        if(record.getWmoHeader()!= null)    pdv.setString(WMOHEADER, record.getWmoHeader());
        if(record.getClath()!= null)    pdv.setFloat(CLATH, record.getClath().floatValue());
        if(record.getClonh()!= null)    pdv.setFloat(CLONH, record.getClonh().floatValue());
        if(record.getSelv()!= null)    pdv.setFloat(SELV, record.getSelv().floatValue());
        if(record.getHinc()!= null)    pdv.setFloat(HINC, record.getHinc().floatValue());
        if(record.getSaid()!= null)    pdv.setLong(SAID, record.getSaid());
        if(record.getSwid()!= null)    pdv.setLong(SWID, record.getSwid());
        if(record.getOgce()!= null)    pdv.setLong(OGCE, record.getOgce());
        if(record.getSsin1()!= null)    pdv.setLong(SSIN1, record.getSsin1());
        if(record.getSsin2()!= null)    pdv.setLong(SSIN2, record.getSsin2());
        if(record.getOrbn()!= null)    pdv.setLong(ORBN, record.getOrbn());
        if(record.getRsst()!= null)    pdv.setLong(RSST, record.getRsst());
        if(record.getAetp()!= null)    pdv.setLong(AETP, record.getAetp());
        if(record.getLsql()!= null)    pdv.setLong(LSQL, record.getLsql());
        if(record.getAsfl()!= null)    pdv.setLong(ASFL, record.getAsfl());
        if(record.getRsfl()!= null)    pdv.setLong(RSFL, record.getRsfl());
        if(record.getEeno()!= null)    pdv.setLong(EENO, record.getEeno());
        if(record.getAfssgwh()!= null)    pdv.setLong(AFSSGWH, record.getAfssgwh());
        if(record.getSgwh()!= null)    pdv.setFloat(SGWH, record.getSgwh().floatValue());
        if(record.getFostsgwh()!= null)    pdv.setLong(FOSTSGWH, record.getFostsgwh());
        if(record.getSgwhstd()!= null)    pdv.setFloat(SGWHSTD, record.getSgwhstd().floatValue());
        if(record.getNvpp()!= null)    pdv.setLong(NVPP, record.getNvpp());
        if(record.getTobdg1r1()!= null)    pdv.setLong(TOBDG1R1, record.getTobdg1r1());
        if(record.getTobdg1r2()!= null)    pdv.setLong(TOBDG1R2, record.getTobdg1r2());
        if(record.getAfsbkstg1r1()!= null)    pdv.setLong(AFSBKSTG1R1, record.getAfsbkstg1r1());
        if(record.getAfsbkstg1r2()!= null)    pdv.setLong(AFSBKSTG1R2, record.getAfsbkstg1r2());
        if(record.getBkstg1r1()!= null)    pdv.setFloat(BKSTG1R1, record.getBkstg1r1().floatValue());
        if(record.getBkstg1r2()!= null)    pdv.setFloat(BKSTG1R2, record.getBkstg1r2().floatValue());
        if(record.getFostbkstg1r1()!= null)    pdv.setLong(FOSTBKSTG1R1, record.getFostbkstg1r1());
        if(record.getFostbkstg1r2()!= null)    pdv.setLong(FOSTBKSTG1R2, record.getFostbkstg1r2());
        if(record.getBkststdg1r1()!= null)    pdv.setFloat(BKSTSTDG1R1, record.getBkststdg1r1().floatValue());
        if(record.getBkststdg1r2()!= null)    pdv.setFloat(BKSTSTDG1R2, record.getBkststdg1r2().floatValue());
        if(record.getSona()!= null)		pdv.setFloat(SONA, record.getSona().floatValue());
        if(record.getAfssona()!= null)    pdv.setLong(AFSSONA, record.getAfssona());
        if(record.getRwvc()!=null)    pdv.setFloat(RWVC, record.getRwvc().floatValue());
        if(record.getRlqc()!= null)	pdv.setFloat(RLQC, record.getRlqc().floatValue());
        if(record.getAfsselvg1r1()!= null)    pdv.setLong(AFSSELVG1R1, record.getAfsselvg1r1());
        if(record.getAfsselvg1r2()!= null)    pdv.setLong(AFSSELVG1R2, record.getAfsselvg1r2());
        if(record.getSelvg1r1()!= null)    pdv.setFloat(SELVG1R1, record.getSelvg1r1().floatValue());
        if(record.getSelvg1r2()!= null)    pdv.setFloat(SELVG1R2, record.getSelvg1r2().floatValue());
        if(record.getHincg1r1()!= null)    pdv.setFloat(HINCG1R1, record.getHincg1r1().floatValue());
        if(record.getHincg1r2()!= null)    pdv.setFloat(HINCG1R2, record.getHincg1r2().floatValue());
        if(record.getFostselvg1r1()!= null)    pdv.setLong(FOSTSELVG1R1, record.getFostselvg1r1());
        if(record.getFostselvg1r2()!= null)    pdv.setLong(FOSTSELVG1R2, record.getFostselvg1r2());
        if(record.getSelvstdg1r1()!= null)    pdv.setFloat(SELVSTDG1R1, record.getSelvstdg1r1().floatValue());
        if(record.getSelvstdg1r2()!= null)    pdv.setFloat(SELVSTDG1R2, record.getSelvstdg1r2().floatValue());
        if(record.getNvppg1r1()!= null)    pdv.setLong(NVPPG1R1, record.getNvppg1r1());
        if(record.getNvppg1r2()!= null)    pdv.setLong(NVPPG1R2, record.getNvppg1r2());
        if(record.getMefrg2r1()!= null)    pdv.setFloat(MEFRG2R1, record.getMefrg2r1().floatValue());
        if(record.getMefrg2r2()!= null)    pdv.setFloat(MEFRG2R2, record.getMefrg2r2().floatValue());
        if(record.getMefrg2r3()!= null)    pdv.setFloat(MEFRG2R3, record.getMefrg2r3().floatValue());
        if(record.getAfstmbrg2r1()!= null)    pdv.setLong(AFSTMBRG2R1, record.getAfstmbrg2r1());
        if(record.getAfstmbrg2r2()!= null)    pdv.setLong(AFSTMBRG2R2, record.getAfstmbrg2r2());
        if(record.getAfstmbrg2r3()!= null)    pdv.setLong(AFSTMBRG2R3, record.getAfstmbrg2r3());
        if(record.getTmbrg2r1()!= null)    pdv.setFloat(TMBRG2R1, record.getTmbrg2r1().floatValue());
        if(record.getTmbrg2r2()!= null)    pdv.setFloat(TMBRG2R2, record.getTmbrg2r2().floatValue());
        if(record.getTmbrg2r3()!= null)    pdv.setFloat(TMBRG2R3, record.getTmbrg2r3().floatValue());
        if(record.getSwcmg3r1()!= null)    pdv.setLong(SWCMG3R1, record.getSwcmg3r1());
        if(record.getSwcmg3r2()!= null)    pdv.setLong(SWCMG3R2, record.getSwcmg3r2());
        if(record.getWs10g3r1()!= null)    pdv.setFloat(WS10G3R1, record.getWs10g3r1().floatValue());
        if(record.getWs10g3r2()!= null)    pdv.setFloat(WS10G3R2, record.getWs10g3r2().floatValue());
        
        return pdv;

    }

    public static SgwhRecord toSgwhRecord(PointDataView pdv) {
    	SgwhRecord mr = new SgwhRecord();
    	 
    	mr.setDataTime(new DataTime(new Date(pdv.getLong(REFTIME)),pdv.getNumber(FORECASTTIME).intValue()));
    	mr.setObsTime(TimeTools.newCalendar(pdv.getLong(OBSTIME)));
    	mr.setDataURI(pdv.getString(DATAURI));
    	mr.setReportType(pdv.getString(REPORTTYPE));
    	mr.setWmoHeader(pdv.getString(WMOHEADER));
    	mr.setClath(pdv.getNumber(CLATH).doubleValue());
    	mr.setClonh(pdv.getNumber(CLONH).doubleValue());
    	mr.setSelv(pdv.getNumber(SELV).doubleValue());
    	mr.setHinc(pdv.getNumber(HINC).doubleValue());
    	mr.setSaid(pdv.getNumber(SAID).longValue());
    	mr.setSwid(pdv.getNumber(SWID).longValue());
    	mr.setOgce(pdv.getNumber(OGCE).longValue());
    	mr.setSsin1(pdv.getNumber(SSIN1).longValue());
    	mr.setSsin2(pdv.getNumber(SSIN2).longValue());
    	mr.setOrbn(pdv.getNumber(ORBN).longValue());
    	mr.setRsst(pdv.getNumber(RSST).longValue());
    	mr.setAetp(pdv.getNumber(AETP).longValue());
    	mr.setLsql(pdv.getNumber(LSQL).longValue());
    	mr.setAsfl(pdv.getNumber(ASFL).longValue());
    	mr.setRsfl(pdv.getNumber(RSFL).longValue());
    	mr.setEeno(pdv.getNumber(EENO).longValue());
    	mr.setAfssgwh(pdv.getNumber(AFSSGWH).longValue());
    	mr.setSgwh(pdv.getNumber(SGWH).doubleValue());
    	mr.setFostsgwh(pdv.getNumber(FOSTSGWH).longValue());
    	mr.setSgwhstd(pdv.getNumber(SGWHSTD).doubleValue());
    	mr.setNvpp(pdv.getNumber(NVPP).longValue());
    	mr.setTobdg1r1(pdv.getNumber(TOBDG1R1).longValue());
    	mr.setTobdg1r2(pdv.getNumber(TOBDG1R2).longValue());
    	mr.setAfsbkstg1r1(pdv.getNumber(AFSBKSTG1R1).longValue());
    	mr.setAfsbkstg1r2(pdv.getNumber(AFSBKSTG1R2).longValue());
    	mr.setBkstg1r1(pdv.getNumber(BKSTG1R1).doubleValue());
    	mr.setBkstg1r2(pdv.getNumber(BKSTG1R2).doubleValue());
    	mr.setFostbkstg1r1(pdv.getNumber(FOSTBKSTG1R1).longValue());
    	mr.setFostbkstg1r2(pdv.getNumber(FOSTBKSTG1R2).longValue());
    	mr.setBkststdg1r1(pdv.getNumber(BKSTSTDG1R1).doubleValue());
    	mr.setBkststdg1r2(pdv.getNumber(BKSTSTDG1R2).doubleValue());
    	mr.setSona(pdv.getNumber(SONA).doubleValue());
    	mr.setAfssona(pdv.getNumber(AFSSONA).longValue());
    	mr.setRwvc(pdv.getNumber(RWVC).doubleValue());
    	mr.setRlqc(pdv.getNumber(RLQC).doubleValue());
    	mr.setAfsselvg1r1(pdv.getNumber(AFSSELVG1R1).longValue());
    	mr.setAfsselvg1r2(pdv.getNumber(AFSSELVG1R2).longValue());
    	mr.setSelvg1r1(pdv.getNumber(SELVG1R1).doubleValue());
    	mr.setSelvg1r2(pdv.getNumber(SELVG1R2).doubleValue());
    	mr.setHincg1r1(pdv.getNumber(HINCG1R1).doubleValue());
    	mr.setHincg1r2(pdv.getNumber(HINCG1R2).doubleValue());
    	mr.setFostselvg1r1(pdv.getNumber(FOSTSELVG1R1).longValue());
    	mr.setFostselvg1r2(pdv.getNumber(FOSTSELVG1R2).longValue());
    	mr.setSelvstdg1r1(pdv.getNumber(SELVSTDG1R1).doubleValue());
    	mr.setSelvstdg1r2(pdv.getNumber(SELVSTDG1R2).doubleValue());
    	mr.setNvppg1r1(pdv.getNumber(NVPPG1R1).longValue());
    	mr.setNvppg1r2(pdv.getNumber(NVPPG1R2).longValue());
    	mr.setMefrg2r1(pdv.getNumber(MEFRG2R1).doubleValue());
    	mr.setMefrg2r2(pdv.getNumber(MEFRG2R2).doubleValue());
    	mr.setMefrg2r3(pdv.getNumber(MEFRG2R3).doubleValue());
    	mr.setAfstmbrg2r1(pdv.getNumber(AFSTMBRG2R1).longValue());
    	mr.setAfstmbrg2r2(pdv.getNumber(AFSTMBRG2R2).longValue());
    	mr.setAfstmbrg2r3(pdv.getNumber(AFSTMBRG2R3).longValue());
    	mr.setTmbrg2r1(pdv.getNumber(TMBRG2R1).doubleValue());
    	mr.setTmbrg2r2(pdv.getNumber(TMBRG2R2).doubleValue());
    	mr.setTmbrg2r3(pdv.getNumber(TMBRG2R3).doubleValue());
    	mr.setSwcmg3r1(pdv.getNumber(SWCMG3R1).longValue());
    	mr.setSwcmg3r2(pdv.getNumber(SWCMG3R2).longValue());
    	mr.setWs10g3r1(pdv.getNumber(WS10G3R1).doubleValue());
    	mr.setWs10g3r2(pdv.getNumber(WS10G3R2).doubleValue());
   	

        return mr;
    }

    public static SgwhRecord[] toSgwhRecords(PointDataContainer container) {
        List<SgwhRecord> records = new ArrayList<SgwhRecord>();
        container.setCurrentSz(container.getAllocatedSz());
        for (int i = 0; i < container.getCurrentSz(); i++) {
            PointDataView pdv = container.readRandom(i);
            records.add(toSgwhRecord(pdv));
        }
        return records.toArray(new SgwhRecord[records.size()]);

    }
}
