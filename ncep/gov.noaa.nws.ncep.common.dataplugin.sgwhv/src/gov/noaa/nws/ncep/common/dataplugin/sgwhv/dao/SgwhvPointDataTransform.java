/**
 * SgwhvPointDataTransform
 * This java class performs the transformation between SwghvRecord  Java class to pointdata format.
 * .
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date			Ticket#		Engineer	Description
 * ------------ -----------	----------- --------------------------
 * Aug23 2011	   		    Chin Chen	Initial Coding 
 * </pre>
 * 
 * @author chin chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.common.dataplugin.sgwhv.dao;

import gov.noaa.nws.ncep.common.dataplugin.sgwhv.SgwhvRecord;

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



public class SgwhvPointDataTransform {

    private static final String FORECASTTIME = "FORECASTTIME";

    private static final String OBSTIME = "OBSTIME";

    private static final String REFTIME = "REFTIME";
    //private static final String INSERTTIME = "INSERTTIME";
    private static final String DATAURI = "DATAURI";
    private static final String REPORTTYPE = "REPORTTYPE";
    private static final String LAT = "LAT";

    private static final String LON = "LON";
    
    private static final String ALTITUDE = "ALTITUDE";

    private static final String ALTCORRI = "ALTCORRI";

    private static final String ALTCORRD = "ALTCORRD";

    private static final String ALTCORRW = "ALTCORRW";

    private static final String LOOPCORR = "LOOPCORR";

    private static final String BKST = "BKST";

    private static final String SATELLITEID = "SATELLITEID";

    private static final String WSPD10 = "WSPD10";

    private static final String HTWAVES = "HTWAVES";

    private static final String SGWHSD = "SGWHSD";
    private static final String PEAK = "PEAK";

    
   /**
     * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! It is important to
     * keep this up to date or risk breaking backwards compatibility
     * 
     */
    private static final String[] ALL_PARAMS = { FORECASTTIME,OBSTIME,REFTIME,  DATAURI,REPORTTYPE,
    	LAT,LON,ALTITUDE, ALTCORRI,ALTCORRD, ALTCORRW, LOOPCORR, BKST, SATELLITEID, 
    	WSPD10, HTWAVES, SGWHSD, PEAK,};

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

    private SgwhvDao dao;

    private PointDataDescription description;

    public SgwhvPointDataTransform() {
        try {
            this.dao = new SgwhvDao("sgwhv");
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
                if (!(p instanceof SgwhvRecord))
                    continue;

                File f = this.dao.getFullFilePath(p);
                PointDataContainer pdc = pointMap.get(f);
                if (pdc == null) {
                    pdc = PointDataContainer.build(this.description);
                    pointMap.put(f, pdc);
                }

                SgwhvRecord mr = (SgwhvRecord) p;
                PointDataView pdv = buildView(pdc, mr);
                mr.setPointDataView(pdv);
            }
        }
        return pdo;
    }
    private PointDataView buildView(PointDataContainer container,
    		SgwhvRecord record) {
        PointDataView pdv = container.append();
        if(record.getDataTime()!= null)
        	pdv.setLong(FORECASTTIME, record.getDataTime().getFcstTime());
        if(record.getObsTime()!= null)
        	pdv.setLong(OBSTIME, record.getObsTime().getTime().getTime());
        if(record.getDataTime()!= null)
        	pdv.setLong(REFTIME, record.getDataTime().getRefTime().getTime());
        //if(record.getInsertTime()!= null)
        //	pdv.setLong(INSERTTIME, record.getInsertTime().getTime().getTime());
        if(record.getDataURI()!= null)    pdv.setString(DATAURI, record.getDataURI());
        if(record.getReportType()!= null)    pdv.setString(REPORTTYPE, record.getReportType());
        if(record.getAltCorrD()!= null)    pdv.setFloat(ALTCORRD, record.getAltCorrD().floatValue());
        if(record.getAltCorrI()!= null)    pdv.setFloat(ALTCORRI, record.getAltCorrI().floatValue());
        if(record.getAltCorrW()!= null)    pdv.setFloat(ALTCORRW, record.getAltCorrW().floatValue());
        if(record.getAltitude()!= null)    pdv.setFloat(ALTITUDE, record.getAltitude().floatValue());
        if(record.getBkst()!= null)    pdv.setFloat(BKST, record.getBkst().floatValue());
        if(record.getPeak()!= null)    pdv.setLong(PEAK, record.getPeak());
        if(record.getSatelliteId()!= null)    pdv.setLong(SATELLITEID, record.getSatelliteId());
        
        if(record.getLat()!= null)    pdv.setFloat(LAT, record.getLat().floatValue());
        if(record.getLon()!= null)    pdv.setFloat(LON, record.getLon().floatValue());
        
        if(record.getLoopCorr()!= null)    pdv.setFloat(LOOPCORR, record.getLoopCorr().floatValue());
        if(record.getWspd10()!= null)    pdv.setFloat(WSPD10, record.getWspd10().floatValue());
        if(record.getHtwaves()!= null)    pdv.setFloat(HTWAVES, record.getHtwaves().floatValue());
        if(record.getSgwhSd()!= null)    pdv.setFloat(SGWHSD, record.getSgwhSd().floatValue());
                
        return pdv;

    }
    public static SgwhvRecord toSgwhvRecord(PointDataView pdv) {
    	SgwhvRecord mr = new SgwhvRecord();
    	 
    	mr.setDataTime(new DataTime(new Date(pdv.getLong(REFTIME)),
    			pdv.getNumber(FORECASTTIME).intValue()));
    	mr.setObsTime(TimeTools.newCalendar(pdv.getLong(OBSTIME)));
    	mr.setDataURI(pdv.getString(DATAURI));
    	mr.setReportType(pdv.getString(REPORTTYPE));
    	mr.setLat(pdv.getNumber(LAT).doubleValue());
    	mr.setLon(pdv.getNumber(LON).doubleValue());
    	mr.setAltitude(pdv.getNumber(ALTITUDE).doubleValue());
    	mr.setAltCorrI(pdv.getNumber(ALTCORRI).doubleValue());
    	mr.setAltCorrD(pdv.getNumber(ALTCORRD).doubleValue());
    	mr.setSatelliteId(pdv.getNumber(SATELLITEID).longValue());
    	mr.setPeak(pdv.getNumber(PEAK).longValue());
    	mr.setAltCorrW(pdv.getNumber(ALTCORRW).doubleValue());
    	mr.setLoopCorr(pdv.getNumber(LOOPCORR).doubleValue());
    	mr.setBkst(pdv.getNumber(BKST).doubleValue());
    	mr.setWspd10(pdv.getNumber(WSPD10).doubleValue());
    	mr.setHtwaves(pdv.getNumber(HTWAVES).doubleValue());
    	mr.setSgwhSd(pdv.getNumber(SGWHSD).doubleValue());
    	//mr.setInsertTime(TimeTools.newCalendar(pdv.getLong(INSERTTIME)));
    	
        return mr;
    }

    public static SgwhvRecord[] toSgwhvRecords(PointDataContainer container) {
        List<SgwhvRecord> records = new ArrayList<SgwhvRecord>();
        container.setCurrentSz(container.getAllocatedSz());
        for (int i = 0; i < container.getCurrentSz(); i++) {
            PointDataView pdv = container.readRandom(i);
            records.add(toSgwhvRecord(pdv));
        }
        return records.toArray(new SgwhvRecord[records.size()]);

    }
}
