/**
 * SshaPointDataTransform
 * This java class performs the transformation between SshaRecord  Java class to pointdata format.
 * .
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date			Ticket#		Engineer	Description
 * ------------ -----------	----------- --------------------------
 * Sep 2011	   		        Chin Chen	Initial Coding 
 * </pre>
 * 
 * @author chin chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.common.dataplugin.ssha.dao;

import gov.noaa.nws.ncep.common.dataplugin.ssha.SshaRecord;

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



public class SshaPointDataTransform {

    private static final String FORECASTTIME = "FORECASTTIME";

    private static final String OBSTIME = "OBSTIME";

    private static final String REFTIME = "REFTIME";

    private static final String DATAURI = "DATAURI";
    private static final String REPORTTYPE = "REPORTTYPE";

    private static final String CLATH = "CLATH";

    private static final String CLONH = "CLONH";
    private static final String SAID = "SAID";

    private static final String STAQ = "STAQ";

    private static final String SIID = "SIID";

    private static final String SOFTV = "SOFTV";

    private static final String SACYLN = "SACYLN";

    private static final String ORBN = "ORBN";

    private static final String RSST = "RSST";

    private static final String NUMID = "NUMID";
    private static final String AETP = "AETP";

    private static final String DSST = "DSST";

    private static final String INTF = "INTF";

    private static final String EENO = "EENO";

    private static final String ASFL = "ASFL";

    private static final String ADQF = "ADQF";

    private static final String ARQF = "ARQF";

    private static final String ALRF = "ALRF";

    private static final String RSFL = "RSFL";

    private static final String RDQF = "RDQF";

    private static final String RBIF = "RBIF";

    private static final String IPIN = "IPIN";

    private static final String AASF = "AASF";

    private static final String MMAP = "MMAP";

    private static final String IFDT = "IFDT";

    private static final String KBOR = "KBOR";

    private static final String RKBOR = "RKBOR";

    private static final String NVPK2 = "NVPK2";

    private static final String KBIC = "KBIC";

    private static final String SBCK = "SBCK";

    private static final String KBSW = "KBSW";

    private static final String RKSW = "RKSW";

    private static final String NVKSW = "NVKSW";

    private static final String KNCS = "KNCS";

    private static final String KOBC = "KOBC";

    private static final String SKOBC = "SKOBC";
    
    private static final String NVPKB = "NVPKB";
    
    private static final String KNIC = "KNIC";
    
    private static final String ACRS1 = "ACRS1";
    
    private static final String ACRS2 = "ACRS2";
    
    private static final String KAGC = "KAGC";
    
    private static final String RKAGC = "RKAGC";
    
    private static final String NVKG = "NVKG";
    
    private static final String CBOR = "CBOR";
    
    private static final String RCBOR = "RCBOR";
    
    private static final String NVPC = "NVPC";
    
    private static final String CBIC = "CBIC";
    
    private static final String SBCC = "SBCC";
    
    private static final String CBSW = "CBSW";
    
    private static final String RCSW = "RCSW";
    
    private static final String NVCSW = "NVCSW";
    
    private static final String CNCS = "CNCS";
    
    private static final String CCOB = "CCOB";
    
    private static final String RCCOB = "RCCOB";
    
    private static final String NVPCB = "NVPCB";
    
    private static final String CNIA = "CNIA";
    
    private static final String CAGC = "CAGC";
    
    private static final String RCAGC = "RCAGC";
    
    private static final String NVPCA = "NVPCA";
    
    private static final String SCCF1 = "SCCF1";
    
    private static final String SCCF2 = "SCCF2";
    
    private static final String SCCF3 = "SCCF3";
    
    private static final String TMBRST1 = "TMBRST1";
    
    private static final String TMBRST2 = "TMBRST2";
    
    private static final String TMBRST3 = "TMBRST3";
    
    private static final String RWVC = "RWVC";
    
    private static final String RLQC = "RLQC";
    
    private static final String HMSL1 = "HMSL1";
    
    private static final String HMSL2 = "HMSL2";
    
    private static final String WSPA = "WSPA";
    
    private static final String WSPR = "WSPR";
    
    private static final String UMWV = "UMWV";
    
    private static final String VWMV = "VWMV";
    
    private static final String MDYT = "MDYT";
    
    private static final String ALRE = "ALRE";
    
    private static final String IALR = "IALR";
    
    private static final String ONAP = "ONAP";
    
    private static final String SONAW = "SONAW";
    
    private static final String ICMK = "ICMK";
    
    private static final String AICK = "AICK";
    
    private static final String MDTC = "MDTC";
    
    private static final String MWTC = "MWTC";
    
    private static final String RWTC = "RWTC";
    
    private static final String MSSH = "MSSH";
    
    private static final String MSHA = "MSHA";
    
    private static final String GEODH = "GEODH";
    
    private static final String ODLE = "ODLE";
    
    private static final String SETH = "SETH";
    
    private static final String TGOTH1 = "TGOTH1";
    
    private static final String TGOTH2 = "TGOTH2";
    
    private static final String LTHS1 = "LTHS1";
    
    private static final String LTHS2 = "LTHS2";
    
    private static final String LPTH = "LPTH";
    
    private static final String NLTH = "NLTH";
    
    private static final String GPTH = "GPTH";
    
    private static final String IBCO = "IBCO";
    
    private static final String HFSTC = "HFSTC";
    
    private static final String SSHA = "SSHA";
    
    
   /**
     * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! It is important to
     * keep this up to date or risk breaking backwards compatibility
     * 
     */
    private static final String[] ALL_PARAMS = { FORECASTTIME,OBSTIME,REFTIME, DATAURI,REPORTTYPE,
    	 CLATH, CLONH, STAQ,  SAID,SIID,SOFTV,SACYLN,NUMID,ORBN,RSST,AETP,DSST,INTF,EENO,ASFL,ADQF,
    	 ARQF,RSFL,ALRF,RDQF,RBIF,IPIN,AASF,MMAP,IFDT,KBOR,RKBOR,NVPK2,KBIC,SBCK,KBSW,RKSW,NVKSW,
    	 KNCS,KOBC,SKOBC,NVPKB,KNIC,ACRS1,ACRS2,KAGC,RKAGC,NVKG,CBOR,RCBOR,NVPC,CBIC,SBCC,CBSW,
    	 RCSW,NVCSW,CNCS,CCOB,RCCOB,NVPCB,CNIA,CAGC,RCAGC,NVPCA,SCCF1,SCCF2,SCCF3,TMBRST1,
    	 TMBRST2,TMBRST3,RWVC,RLQC,HMSL1,HMSL2,WSPA,WSPR,UMWV,VWMV,MDYT,ALRE,IALR,ONAP,SONAW,ICMK,
    	 AICK,MDTC,MWTC,RWTC,MSSH,MSHA,GEODH,ODLE,SETH,TGOTH1,TGOTH2,LTHS1,LTHS2,LPTH,NLTH,GPTH,
    	 IBCO,HFSTC,SSHA,};

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

    private SshaDao dao;

    private PointDataDescription description;

    public SshaPointDataTransform() {
        try {
            this.dao = new SshaDao("ssha");
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
                if (!(p instanceof SshaRecord))
                    continue;

                File f = this.dao.getFullFilePath(p);
                PointDataContainer pdc = pointMap.get(f);
                if (pdc == null) {
                    pdc = PointDataContainer.build(this.description);
                    pointMap.put(f, pdc);
                }

                SshaRecord mr = (SshaRecord) p;
                PointDataView pdv = buildView(pdc, mr);
                mr.setPointDataView(pdv);
            }
        }
        return pdo;
    }
    private PointDataView buildView(PointDataContainer container,
    		SshaRecord record) {
        PointDataView pdv = container.append();
        if(record.getDataTime()!= null)
        	pdv.setLong(FORECASTTIME, record.getDataTime().getFcstTime());
        if(record.getObsTime()!= null)
        	pdv.setLong(OBSTIME, record.getObsTime().getTime().getTime());
        if(record.getDataTime()!= null)
        	pdv.setLong(REFTIME, record.getDataTime().getRefTime().getTime());
        if(record.getDataURI()!= null)    pdv.setString(DATAURI, record.getDataURI());
        if(record.getReportType()!= null)    pdv.setString(REPORTTYPE, record.getReportType());
        if(record.getSaid()!= null)    pdv.setLong(SAID, record.getSaid());
        if(record.getSiid()!= null)    pdv.setLong(SIID, record.getSiid());
        if(record.getClath()!= null)    pdv.setFloat(CLATH, record.getClath().floatValue());
        if(record.getClonh()!= null)    pdv.setFloat(CLONH, record.getClonh().floatValue());
        if(record.getStaq()!= null)    pdv.setString(STAQ, record.getStaq());
        if(record.getSoftv()!= null)    pdv.setString(SOFTV, record.getSoftv());
        if(record.getSacyln()!= null)    pdv.setLong(SACYLN, record.getSacyln());
        if(record.getOrbn()!= null)    pdv.setLong(ORBN, record.getOrbn());
        if(record.getNumid()!= null)    pdv.setString(NUMID, record.getNumid());
        if(record.getRsst()!= null)    pdv.setLong(RSST, record.getRsst());
        if(record.getAetp()!= null)    pdv.setLong(AETP, record.getAetp());
        if(record.getIntf()!= null)    pdv.setLong(INTF, record.getIntf());
        if(record.getAsfl()!= null)    pdv.setLong(ASFL, record.getAsfl());
        if(record.getRsfl()!= null)    pdv.setLong(RSFL, record.getRsfl());
        if(record.getEeno()!= null)    pdv.setLong(EENO, record.getEeno());
        if(record.getAdqf()!= null)    pdv.setLong(ADQF, record.getAdqf());
        if(record.getArqf()!= null)    pdv.setLong(ARQF, record.getArqf());
        if(record.getAlrf()!= null)    pdv.setLong(ALRF, record.getAlrf());
        if(record.getRdqf()!= null)    pdv.setLong(RDQF, record.getRdqf());
        if(record.getRbif()!= null)    pdv.setLong(RBIF, record.getRbif());
        if(record.getIpin()!= null)    pdv.setLong(IPIN, record.getIpin());
        if(record.getAasf()!= null)    pdv.setLong(AASF, record.getAasf());
        if(record.getKbor()!= null)    pdv.setFloat(KBOR, record.getKbor().floatValue());
        if(record.getRkbor()!= null)    pdv.setFloat(RKBOR, record.getRkbor().floatValue());
        if(record.getMmap()!= null)    pdv.setLong(MMAP, record.getMmap());
        if(record.getIfdt()!= null)    pdv.setLong(IFDT, record.getIfdt());
        if(record.getKbic()!= null)    pdv.setFloat(KBIC, record.getKbic().floatValue());
        if(record.getSbck()!= null)    pdv.setFloat(SBCK, record.getSbck().floatValue());
        if(record.getKbsw()!= null)		pdv.setFloat(KBSW, record.getKbsw().floatValue());
        if(record.getNvpk2()!= null)    pdv.setLong(NVPK2, record.getNvpk2());
        if(record.getRwvc()!=null)    pdv.setFloat(RWVC, record.getRwvc().floatValue());
        if(record.getRlqc()!= null)	pdv.setFloat(RLQC, record.getRlqc().floatValue());
        if(record.getNvksw()!= null)    pdv.setLong(NVKSW, record.getNvksw());
        if(record.getNvpkb()!= null)    pdv.setLong(NVPKB, record.getNvpkb());
        if(record.getRksw()!= null)    pdv.setFloat(RKSW, record.getRksw().floatValue());
        if(record.getKncs()!= null)    pdv.setFloat(KNCS, record.getKncs().floatValue());
        if(record.getKobc()!= null)    pdv.setFloat(KOBC, record.getKobc().floatValue());
        if(record.getSkobc()!= null)    pdv.setFloat(SKOBC, record.getSkobc().floatValue());
        if(record.getNvkg()!= null)    pdv.setLong(NVKG, record.getNvkg());
        if(record.getNvpc()!= null)    pdv.setLong(NVPC, record.getNvpc());
        if(record.getKnic()!= null)    pdv.setFloat(KNIC, record.getKnic().floatValue());
        if(record.getAcrs1()!= null)    pdv.setFloat(ACRS1, record.getAcrs1().floatValue());
        if(record.getAcrs2()!= null)    pdv.setFloat(ACRS2, record.getAcrs2().floatValue());
        if(record.getNvcsw()!= null)    pdv.setLong(NVCSW, record.getNvcsw());
        if(record.getNvpcb()!= null)    pdv.setLong(NVPCB, record.getNvpcb());
        if(record.getNvpca()!= null)    pdv.setLong(NVPCA, record.getNvpca());
        if(record.getKagc()!= null)    pdv.setFloat(KAGC, record.getKagc().floatValue());
        if(record.getRkagc()!= null)    pdv.setFloat(RKAGC, record.getRkagc().floatValue());
        if(record.getCbor()!= null)    pdv.setFloat(CBOR, record.getCbor().floatValue());
        if(record.getRcbor()!= null)    pdv.setFloat(RCBOR, record.getRcbor().floatValue());
        if(record.getDsst()!= null)    pdv.setLong(DSST, record.getDsst());
        if(record.getCbic()!= null)    pdv.setFloat(CBIC, record.getCbic().floatValue());
        if(record.getSbcc()!= null)    pdv.setFloat(SBCC, record.getSbcc().floatValue());
        if(record.getCbsw()!= null)    pdv.setFloat(CBSW, record.getCbsw().floatValue());
        if(record.getRcsw()!= null)    pdv.setFloat(RCSW, record.getRcsw().floatValue());
        if(record.getCncs()!= null)    pdv.setFloat(CNCS, record.getCncs().floatValue());
        if(record.getCcob()!= null)    pdv.setFloat(CCOB, record.getCcob().floatValue());
        if(record.getRccob()!= null)    pdv.setFloat(RCCOB, record.getRccob().floatValue());
        if(record.getCnia()!= null)    pdv.setFloat(CNIA, record.getCnia().floatValue());
        if(record.getCagc()!= null)    pdv.setFloat(CAGC, record.getCagc().floatValue());
        if(record.getRcagc()!= null)    pdv.setFloat(RCAGC, record.getRcagc().floatValue());
        if(record.getSccf1()!= null)    pdv.setFloat(SCCF1, record.getSccf1().floatValue());
        if(record.getSccf2()!= null)    pdv.setFloat(SCCF2, record.getSccf2().floatValue());
        if(record.getSccf3()!= null)    pdv.setFloat(SCCF3, record.getSccf3().floatValue());
        if(record.getTmbrst1()!= null)    pdv.setFloat(TMBRST1, record.getTmbrst1().floatValue());
        if(record.getTmbrst2()!= null)    pdv.setFloat(TMBRST2, record.getTmbrst2().floatValue());
        if(record.getTmbrst3()!= null)    pdv.setFloat(TMBRST3, record.getTmbrst3().floatValue());
        if(record.getHmsl1()!= null)    pdv.setFloat(HMSL1, record.getHmsl1().floatValue());
        if(record.getHmsl2()!= null)    pdv.setFloat(HMSL2, record.getHmsl2().floatValue());
        if(record.getWspa()!= null)    pdv.setFloat(WSPA, record.getWspa().floatValue());
        if(record.getWspr()!= null)    pdv.setFloat(WSPR, record.getWspr().floatValue());
        if(record.getUmwv()!= null)    pdv.setFloat(UMWV, record.getUmwv().floatValue());
        if(record.getVwmv()!= null)    pdv.setFloat(VWMV, record.getVwmv().floatValue());
        if(record.getMdyt()!= null)    pdv.setFloat(MDYT, record.getMdyt().floatValue());
        if(record.getAlre()!= null)    pdv.setFloat(ALRE, record.getAlre().floatValue());
        if(record.getIalr()!= null)    pdv.setFloat(IALR, record.getIalr().floatValue());
        if(record.getOnap()!= null)    pdv.setFloat(ONAP, record.getOnap().floatValue());
        if(record.getSonaw()!= null)    pdv.setFloat(SONAW, record.getSonaw().floatValue());
        if(record.getIcmk()!= null)    pdv.setFloat(ICMK, record.getIcmk().floatValue());
        if(record.getAick()!= null)    pdv.setFloat(AICK, record.getAick().floatValue());
        if(record.getMdtc()!= null)    pdv.setFloat(MDTC, record.getMdtc().floatValue());
        if(record.getMwtc()!= null)    pdv.setFloat(MWTC, record.getMwtc().floatValue());
        if(record.getRwtc()!= null)    pdv.setFloat(RWTC, record.getRwtc().floatValue());
        if(record.getMssh()!= null)    pdv.setFloat(MSSH, record.getMssh().floatValue());
        if(record.getMsha()!= null)    pdv.setFloat(MSHA, record.getMsha().floatValue());
        if(record.getGeodh()!= null)    pdv.setFloat(GEODH, record.getGeodh().floatValue());
        if(record.getOdle()!= null)    pdv.setFloat(ODLE, record.getOdle().floatValue());
        if(record.getSeth()!= null)    pdv.setFloat(SETH, record.getSeth().floatValue());
        if(record.getTgoth1()!= null)    pdv.setFloat(TGOTH1, record.getTgoth1().floatValue());
        if(record.getTgoth2()!= null)    pdv.setFloat(TGOTH2, record.getTgoth2().floatValue());
        if(record.getLths1()!= null)    pdv.setFloat(LTHS1, record.getLths1().floatValue());
        if(record.getLths2()!= null)    pdv.setFloat(LTHS2, record.getLths2().floatValue());
        if(record.getLpth()!= null)    pdv.setFloat(LPTH, record.getLpth().floatValue());
        if(record.getNlth()!= null)    pdv.setFloat(NLTH, record.getNlth().floatValue());
        if(record.getGpth()!= null)    pdv.setFloat(GPTH, record.getGpth().floatValue());
        if(record.getIbco()!= null)    pdv.setFloat(IBCO, record.getIbco().floatValue());
        if(record.getHfstc()!= null)    pdv.setFloat(HFSTC, record.getHfstc().floatValue());
        if(record.getSsha()!= null)    pdv.setFloat(SSHA, record.getSsha().floatValue());
        
        return pdv;

    }
    public static SshaRecord toSshaRecord(PointDataView pdv) {
    	SshaRecord mr = new SshaRecord();
    	mr.setDataTime(new DataTime(new Date(pdv.getLong(REFTIME)),pdv.getNumber(FORECASTTIME).intValue()));
    	mr.setSaid(pdv.getNumber(SAID).longValue());
    	mr.setSiid(pdv.getNumber(SIID).longValue());
    	mr.setStaq(pdv.getString(STAQ));
    	mr.setSoftv(pdv.getString(SOFTV));
    	mr.setSacyln(pdv.getNumber(SACYLN).longValue());
    	mr.setOrbn(pdv.getNumber(ORBN).longValue());
    	mr.setObsTime(TimeTools.newCalendar(pdv.getLong(OBSTIME)));
    	mr.setClath(pdv.getNumber(CLATH).doubleValue());
    	mr.setClonh(pdv.getNumber(CLONH).doubleValue());
    	mr.setRsst(pdv.getNumber(RSST).longValue());
    	mr.setAetp(pdv.getNumber(AETP).longValue());
    	mr.setDsst(pdv.getNumber(DSST).longValue());
    	mr.setIntf(pdv.getNumber(INTF).longValue());
    	mr.setEeno(pdv.getNumber(EENO).longValue());
    	mr.setAsfl(pdv.getNumber(ASFL).longValue());
    	mr.setAdqf(pdv.getNumber(ADQF).longValue());
    	mr.setArqf(pdv.getNumber(ARQF).longValue());
    	mr.setAlrf(pdv.getNumber(ALRF).longValue());
    	mr.setRsfl(pdv.getNumber(RSFL).longValue());
    	mr.setRdqf(pdv.getNumber(RDQF).longValue());
    	mr.setRbif(pdv.getNumber(RBIF).longValue());
    	mr.setIpin(pdv.getNumber(IPIN).longValue());
    	mr.setAasf(pdv.getNumber(AASF).longValue());
    	mr.setMmap(pdv.getNumber(MMAP).longValue());
    	mr.setIfdt(pdv.getNumber(IFDT).longValue());
    	mr.setKbor(pdv.getNumber(KBOR).doubleValue());
    	mr.setRkbor(pdv.getNumber(RKBOR).doubleValue());
    	mr.setNvpk2(pdv.getNumber(NVPK2).longValue());
    	mr.setKbic(pdv.getNumber(KBIC).doubleValue());
    	mr.setKbsw(pdv.getNumber(KBSW).doubleValue());
    	mr.setRksw(pdv.getNumber(RKSW).doubleValue());
    	mr.setSbck(pdv.getNumber(SBCK).doubleValue());
    	mr.setNvksw(pdv.getNumber(NVKSW).longValue());
    	mr.setKncs(pdv.getNumber(KNCS).doubleValue());
    	mr.setKobc(pdv.getNumber(KOBC).doubleValue());
    	mr.setSkobc(pdv.getNumber(SKOBC).doubleValue());
    	mr.setNvpkb(pdv.getNumber(NVPKB).longValue());
    	mr.setKnic(pdv.getNumber(KNIC).doubleValue());
    	mr.setAcrs1(pdv.getNumber(ACRS1).doubleValue());
    	mr.setAcrs2(pdv.getNumber(ACRS2).doubleValue());
    	mr.setKagc(pdv.getNumber(KAGC).doubleValue());
    	mr.setRkagc(pdv.getNumber(RKAGC).doubleValue());
    	mr.setNvkg(pdv.getNumber(NVKG).longValue());
    	mr.setCbor(pdv.getNumber(CBOR).doubleValue());
    	mr.setRcbor(pdv.getNumber(RCBOR).doubleValue());
    	mr.setNvpc(pdv.getNumber(NVPC).longValue());
    	mr.setCbic(pdv.getNumber(CBIC).doubleValue());
    	mr.setSbcc(pdv.getNumber(SBCC).doubleValue());
    	mr.setCbsw(pdv.getNumber(CBSW).doubleValue());
    	mr.setRcsw(pdv.getNumber(RCSW).doubleValue());
    	mr.setNvcsw(pdv.getNumber(NVCSW).longValue());
    	mr.setCncs(pdv.getNumber(CNCS).doubleValue());
    	mr.setCcob(pdv.getNumber(CCOB).doubleValue());
    	mr.setRccob(pdv.getNumber(RCCOB).doubleValue());
    	mr.setNvpcb(pdv.getNumber(NVPCB).longValue());
    	mr.setCnia(pdv.getNumber(CNIA).doubleValue());
    	mr.setCagc(pdv.getNumber(CAGC).doubleValue());
    	mr.setRcagc(pdv.getNumber(RCAGC).doubleValue());
    	mr.setNvpca(pdv.getNumber(NVPCA).longValue());
    	mr.setSccf1(pdv.getNumber(SCCF1).doubleValue());
    	mr.setSccf2(pdv.getNumber(SCCF2).doubleValue());
    	mr.setTmbrst1(pdv.getNumber(TMBRST1).doubleValue());
    	mr.setTmbrst2(pdv.getNumber(TMBRST2).doubleValue());
    	mr.setTmbrst3(pdv.getNumber(TMBRST3).doubleValue());	
    	mr.setRwvc(pdv.getNumber(RWVC).doubleValue());
    	mr.setRlqc(pdv.getNumber(RLQC).doubleValue());
    	mr.setHmsl1(pdv.getNumber(HMSL1).doubleValue());
    	mr.setHmsl2(pdv.getNumber(HMSL2).doubleValue());
    	mr.setWspa(pdv.getNumber(WSPA).doubleValue()); 	
    	mr.setWspr(pdv.getNumber(WSPR).doubleValue());
    	mr.setUmwv(pdv.getNumber(UMWV).doubleValue());
    	mr.setVwmv(pdv.getNumber(VWMV).doubleValue());
    	mr.setMdyt(pdv.getNumber(MDYT).doubleValue());
    	mr.setAlre(pdv.getNumber(ALRE).doubleValue());
    	mr.setIalr(pdv.getNumber(IALR).doubleValue());
    	mr.setOnap(pdv.getNumber(ONAP).doubleValue());
    	mr.setSonaw(pdv.getNumber(SONAW).doubleValue());
    	mr.setIcmk(pdv.getNumber(ICMK).doubleValue());
    	mr.setAick(pdv.getNumber(AICK).doubleValue());
    	mr.setMdtc(pdv.getNumber(MDTC).doubleValue());
    	mr.setMwtc(pdv.getNumber(MWTC).doubleValue());
    	mr.setRwtc(pdv.getNumber(RWTC).doubleValue());
    	mr.setMssh(pdv.getNumber(MSSH).doubleValue());
    	mr.setMsha(pdv.getNumber(MSHA).doubleValue());
    	mr.setGeodh(pdv.getNumber(GEODH).doubleValue());
    	mr.setOdle(pdv.getNumber(ODLE).doubleValue());
    	mr.setSeth(pdv.getNumber(SETH).doubleValue());
    	mr.setTgoth1(pdv.getNumber(TGOTH1).doubleValue());
    	mr.setTgoth2(pdv.getNumber(TGOTH2).doubleValue());
    	mr.setLths1(pdv.getNumber(LTHS1).doubleValue());
    	mr.setLths2(pdv.getNumber(LTHS2).doubleValue());
    	mr.setLpth(pdv.getNumber(LPTH).doubleValue());
    	mr.setNlth(pdv.getNumber(NLTH).doubleValue());
    	mr.setGpth(pdv.getNumber(GPTH).doubleValue());
    	mr.setIbco(pdv.getNumber(IBCO).doubleValue());
    	mr.setHfstc(pdv.getNumber(HFSTC).doubleValue());
    	mr.setSsha(pdv.getNumber(SSHA).doubleValue());
    	mr.setDataURI(pdv.getString(DATAURI));
    	mr.setReportType(pdv.getString(REPORTTYPE));
    	
        return mr;
    }

    public static SshaRecord[] toSshaRecords(PointDataContainer container) {
        List<SshaRecord> records = new ArrayList<SshaRecord>();
        container.setCurrentSz(container.getAllocatedSz());
        for (int i = 0; i < container.getCurrentSz(); i++) {
            PointDataView pdv = container.readRandom(i);
            records.add(toSshaRecord(pdv));
        }
        return records.toArray(new SshaRecord[records.size()]);

    }
}
