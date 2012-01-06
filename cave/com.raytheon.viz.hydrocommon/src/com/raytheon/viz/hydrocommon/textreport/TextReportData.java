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
package com.raytheon.viz.hydrocommon.textreport;

import java.util.ArrayList;
import java.util.Date;

import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Holds the Text Report Data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2009 2260       mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class TextReportData {
    private String lid = null;
    
    private Location location = new Location();
    
    private Riverstat riverstat = new Riverstat();
    
    private Stnclass stnclass = new Stnclass();
    
    private Observer observer = new Observer();
    
    private Descrip descrip = new Descrip();
    
    private Dcp dcp = new Dcp();
    
    private FloodCat floodCat = new FloodCat();
    
    private Telem telem = new Telem();
    
    private Gage gage = new Gage();
    
    private Pub pub = new Pub();
    
    private Datum datum = new Datum();
    
    private Crest crest = new Crest();
    
    private LowWater lowWater = new LowWater();
    
    private Flood flood = new Flood();
    
    private Contacts contacts = new Contacts();
    
    private StaffGageData staffData = null;
    
    private ServiceBackupData serviceBackupData = null;

    private ArrayList<String> refer = new ArrayList<String>();

    private ArrayList<Benchmark> benchmark = new ArrayList<Benchmark>();
    
    private ArrayList<Gage> gageList = new ArrayList<Gage>();
    
    private ArrayList<Pub> pubList = new ArrayList<Pub>();
    
    private ArrayList<Datum> datumList = new ArrayList<Datum>();
    
    private ArrayList<Crest> crestList = new ArrayList<Crest>();
    
    private ArrayList<LowWater> lowWaterList = new ArrayList<LowWater>();
    
    private ArrayList<Flood> floodList = new ArrayList<Flood>();
    
    private ArrayList<Contacts> contactList = new ArrayList<Contacts>();
    
    protected class Stnclass {
        // Stnclass Vars
        private String displayClass = null;

        private String dcp = null;

        private String observer = null;

        private String telem_type = null;

        /**
         * @return the displayClass
         */
        public String getDisplayClass() {
            return displayClass;
        }

        /**
         * @param displayClass
         *            the displayClass to set
         */
        public void setDisplayClass(String displayClass) {
            this.displayClass = displayClass;
        }

        /**
         * @return the dcp
         */
        public String getDcp() {
            return dcp;
        }

        /**
         * @param dcp
         *            the dcp to set
         */
        public void setDcp(String dcp) {
            this.dcp = dcp;
        }

        /**
         * @return the observer
         */
        public String getObserver() {
            return observer;
        }

        /**
         * @param observer
         *            the observer to set
         */
        public void setObserver(String observer) {
            this.observer = observer;
        }

        /**
         * @return the telem_type
         */
        public String getTelem_type() {
            return telem_type;
        }

        /**
         * @param telem_type
         *            the telem_type to set
         */
        public void setTelem_type(String telem_type) {
            this.telem_type = telem_type;
        }

    }

    protected class Location {
        // Location Vars
        private String county = null;

        private String coe = null;

        private String cpm = null;

        private String detail = null;

        private double elev = HydroConstants.MISSING_VALUE;

        private String hdatum = null;

        private String hsa = null;

        private String hu = null;

        private double lat = HydroConstants.MISSING_VALUE;

        private double lon = HydroConstants.MISSING_VALUE;

        private String lremark = null;

        private Date lrevise = null;

        private String name = null;

        private String network = null;

        private String rb = null;

        private String rfc = null;

        private Date sbd = null;

        private String sn = null;

        private String state = null;

        private String waro = null;

        private String wfo = null;

        private String wsfo = null;

        private String type = null;

        private String des = null;

        private String det = null;

        private String stntype = null;

        private String tzone = null;

        /**
         * @return the county
         */
        public String getCounty() {
            return county;
        }

        /**
         * @param county
         *            the county to set
         */
        public void setCounty(String county) {
            this.county = county;
        }

        /**
         * @return the coe
         */
        public String getCoe() {
            return coe;
        }

        /**
         * @param coe
         *            the coe to set
         */
        public void setCoe(String coe) {
            this.coe = coe;
        }

        /**
         * @return the cpm
         */
        public String getCpm() {
            return cpm;
        }

        /**
         * @param cpm
         *            the cpm to set
         */
        public void setCpm(String cpm) {
            this.cpm = cpm;
        }

        /**
         * @return the detail
         */
        public String getDetail() {
            return detail;
        }

        /**
         * @param detail
         *            the detail to set
         */
        public void setDetail(String detail) {
            this.detail = detail;
        }

        /**
         * @return the elev
         */
        public double getElev() {
            return elev;
        }

        /**
         * @param elev
         *            the elev to set
         */
        public void setElev(double elev) {
            this.elev = elev;
        }

        /**
         * @return the hdatum
         */
        public String getHdatum() {
            return hdatum;
        }

        /**
         * @param hdatum
         *            the hdatum to set
         */
        public void setHdatum(String hdatum) {
            this.hdatum = hdatum;
        }

        /**
         * @return the hsa
         */
        public String getHsa() {
            return hsa;
        }

        /**
         * @param hsa
         *            the hsa to set
         */
        public void setHsa(String hsa) {
            this.hsa = hsa;
        }

        /**
         * @return the hu
         */
        public String getHu() {
            return hu;
        }

        /**
         * @param hu
         *            the hu to set
         */
        public void setHu(String hu) {
            this.hu = hu;
        }

        /**
         * @return the lat
         */
        public double getLat() {
            return lat;
        }

        /**
         * @param lat
         *            the locationLat to set
         */
        public void setLat(double lat) {
            this.lat = lat;
        }

        /**
         * @return the lon
         */
        public double getLon() {
            return lon;
        }

        /**
         * @param lon
         *            the lon to set
         */
        public void setLon(double lon) {
            this.lon = lon;
        }

        /**
         * @return the lremark
         */
        public String getLremark() {
            return lremark;
        }

        /**
         * @param lremark
         *            the lremark to set
         */
        public void setLremark(String lremark) {
            this.lremark = lremark;
        }

        /**
         * @return the lrevise
         */
        public Date getLrevise() {
            return lrevise;
        }

        /**
         * @param lrevise
         *            the lrevise to set
         */
        public void setLrevise(Date lrevise) {
            this.lrevise = lrevise;
        }

        /**
         * @return the name
         */
        public String getName() {
            return name;
        }

        /**
         * @param name
         *            the name to set
         */
        public void setName(String name) {
            this.name = name;
        }

        /**
         * @return the network
         */
        public String getNetwork() {
            return network;
        }

        /**
         * @param network
         *            the network to set
         */
        public void setNetwork(String network) {
            this.network = network;
        }

        /**
         * @return the rb
         */
        public String getRb() {
            return rb;
        }

        /**
         * @param rb
         *            the rb to set
         */
        public void setRb(String rb) {
            this.rb = rb;
        }

        /**
         * @return the rfc
         */
        public String getRfc() {
            return rfc;
        }

        /**
         * @param rfc
         *            the rfc to set
         */
        public void setRfc(String rfc) {
            this.rfc = rfc;
        }

        /**
         * @return the sbd
         */
        public Date getSbd() {
            return sbd;
        }

        /**
         * @param sbd
         *            the sbd to set
         */
        public void setSbd(Date sbd) {
            this.sbd = sbd;
        }

        /**
         * @return the sn
         */
        public String getSn() {
            return sn;
        }

        /**
         * @param sn
         *            the sn to set
         */
        public void setSn(String sn) {
            this.sn = sn;
        }

        /**
         * @return the state
         */
        public String getState() {
            return state;
        }

        /**
         * @param state
         *            the state to set
         */
        public void setState(String state) {
            this.state = state;
        }

        /**
         * @return the waro
         */
        public String getWaro() {
            return waro;
        }

        /**
         * @param waro
         *            the waro to set
         */
        public void setWaro(String waro) {
            this.waro = waro;
        }

        /**
         * @return the wfo
         */
        public String getWfo() {
            return wfo;
        }

        /**
         * @param wfo
         *            the wfo to set
         */
        public void setWfo(String wfo) {
            this.wfo = wfo;
        }

        /**
         * @return the wsfo
         */
        public String getWsfo() {
            return wsfo;
        }

        /**
         * @param wsfo
         *            the wsfo to set
         */
        public void setWsfo(String wsfo) {
            this.wsfo = wsfo;
        }

        /**
         * @return the type
         */
        public String getType() {
            return type;
        }

        /**
         * @param type
         *            the type to set
         */
        public void setType(String type) {
            this.type = type;
        }

        /**
         * @return the des
         */
        public String getDes() {
            return des;
        }

        /**
         * @param des
         *            the des to set
         */
        public void setDes(String des) {
            this.des = des;
        }

        /**
         * @return the det
         */
        public String getDet() {
            return det;
        }

        /**
         * @param det
         *            the det to set
         */
        public void setDet(String det) {
            this.det = det;
        }

        /**
         * @return the stntype
         */
        public String getStntype() {
            return stntype;
        }

        /**
         * @param stntype
         *            the stntype to set
         */
        public void setStntype(String stntype) {
            this.stntype = stntype;
        }

        /**
         * @return the tzone
         */
        public String getTzone() {
            return tzone;
        }

        /**
         * @param tzone
         *            the tzone to set
         */
        public void setTzone(String tzone) {
            this.tzone = tzone;
        }
    }

    protected class Riverstat {
        private double lat = HydroConstants.MISSING_VALUE;

        private double lon = HydroConstants.MISSING_VALUE;
        
        private String stream = null;

        private double da = HydroConstants.MISSING_VALUE;
        
        private double fs = HydroConstants.MISSING_VALUE;
        
        private double fq = HydroConstants.MISSING_VALUE;
        
        private double mile = HydroConstants.MISSING_VALUE;
        
        private double wstg = HydroConstants.MISSING_VALUE;
        
        private String gsno = null;
        
        private double zd = HydroConstants.MISSING_VALUE;
        
        private double bf = HydroConstants.MISSING_VALUE;
        
        private double cb = HydroConstants.MISSING_VALUE;
        
        private double pool = HydroConstants.MISSING_VALUE;
        
        private String tide = null;
        
        private String por = null;
        
        private String remark = null;
        
        private Date rrevise = null;
        
        private String rsource = null;
        
        private String vdatum = null;
        
        private String level = null;
        
        private String rated = null;
        
        private double actionFlow = HydroConstants.MISSING_VALUE; 
        
        /**
         * @return the lat
         */
        public double getLat() {
            return lat;
        }

        /**
         * @param lat
         *            the lat to set
         */
        public void setLat(double lat) {
            this.lat = lat;
        }

        /**
         * @return the lon
         */
        public double getLon() {
            return lon;
        }

        /**
         * @param lon
         *            the lon to set
         */
        public void setLon(double lon) {
            this.lon = lon;
        }

        /**
         * @return the stream
         */
        public String getStream() {
            return stream;
        }

        /**
         * @param stream the stream to set
         */
        public void setStream(String stream) {
            this.stream = stream;
        }

        /**
         * @return the da
         */
        public double getDa() {
            return da;
        }

        /**
         * @param da the da to set
         */
        public void setDa(double da) {
            this.da = da;
        }

        /**
         * @return the fs
         */
        public double getFs() {
            return fs;
        }

        /**
         * @param fs the fs to set
         */
        public void setFs(double fs) {
            this.fs = fs;
        }

        /**
         * @return the fq
         */
        public double getFq() {
            return fq;
        }

        /**
         * @param fq the fq to set
         */
        public void setFq(double fq) {
            this.fq = fq;
        }

        /**
         * @return the mile
         */
        public double getMile() {
            return mile;
        }

        /**
         * @param mile the mile to set
         */
        public void setMile(double mile) {
            this.mile = mile;
        }

        /**
         * @return the wstg
         */
        public double getWstg() {
            return wstg;
        }

        /**
         * @param wstg the wstg to set
         */
        public void setWstg(double wstg) {
            this.wstg = wstg;
        }

        /**
         * @return the gsno
         */
        public String getGsno() {
            return gsno;
        }

        /**
         * @param gsno the gsno to set
         */
        public void setGsno(String gsno) {
            this.gsno = gsno;
        }

        /**
         * @return the zd
         */
        public double getZd() {
            return zd;
        }

        /**
         * @param zd the zd to set
         */
        public void setZd(double zd) {
            this.zd = zd;
        }

        /**
         * @return the bf
         */
        public double getBf() {
            return bf;
        }

        /**
         * @param bf the bf to set
         */
        public void setBf(double bf) {
            this.bf = bf;
        }

        /**
         * @return the cb
         */
        public double getCb() {
            return cb;
        }

        /**
         * @param cb the cb to set
         */
        public void setCb(double cb) {
            this.cb = cb;
        }

        /**
         * @return the pool
         */
        public double getPool() {
            return pool;
        }

        /**
         * @param pool the pool to set
         */
        public void setPool(double pool) {
            this.pool = pool;
        }

        /**
         * @return the tide
         */
        public String getTide() {
            return tide;
        }

        /**
         * @param tide the tide to set
         */
        public void setTide(String tide) {
            this.tide = tide;
        }

        /**
         * @return the por
         */
        public String getPor() {
            return por;
        }

        /**
         * @param por the por to set
         */
        public void setPor(String por) {
            this.por = por;
        }

        /**
         * @return the remark
         */
        public String getRemark() {
            return remark;
        }

        /**
         * @param remark the remark to set
         */
        public void setRemark(String remark) {
            this.remark = remark;
        }

        /**
         * @return the rrevise
         */
        public Date getRrevise() {
            return rrevise;
        }

        /**
         * @param rrevise the rrevise to set
         */
        public void setRrevise(Date rrevise) {
            this.rrevise = rrevise;
        }

        /**
         * @return the rsource
         */
        public String getRsource() {
            return rsource;
        }

        /**
         * @param rsource the rsource to set
         */
        public void setRsource(String rsource) {
            this.rsource = rsource;
        }

        /**
         * @return the vdatum
         */
        public String getVdatum() {
            return vdatum;
        }

        /**
         * @param vdatum the vdatum to set
         */
        public void setVdatum(String vdatum) {
            this.vdatum = vdatum;
        }

        /**
         * @return the level
         */
        public String getLevel() {
            return level;
        }

        /**
         * @param level the level to set
         */
        public void setLevel(String level) {
            this.level = level;
        }

        /**
         * @return the rated
         */
        public String getRated() {
            return rated;
        }

        /**
         * @param rated the rated to set
         */
        public void setRated(String rated) {
            this.rated = rated;
        }

        /**
         * @return the actionFlow
         */
        public double getActionFlow() {
            return actionFlow;
        }

        /**
         * @param actionFlow the actionFlow to set
         */
        public void setActionFlow(double actionFlow) {
            this.actionFlow = actionFlow;
        }
    }
    
    protected class Observer {
        // Observer
        private Date dos = null;

        private String firstname = null;

        private String lastname = null;

        private String gn = null;

        private String a1 = null;

        private String a2 = null;

        private String a3 = null;

        private String hphone = null;

        private String phone = null;

        private String recip = null;

        private String city = null;

        private String state = null;

        private String zip = null;

        private String comm = null;

        private String email = null;

        private String rprt = null;
        
        private String sponsor = null;
        
        private String ornr = null;
        
        private double rate = HydroConstants.MISSING_VALUE;
        
        private String tsk = null;

        /**
         * @return the tsk
         */
        public String getTsk() {
            return tsk;
        }

        /**
         * @param tsk the tsk to set
         */
        public void setTsk(String tsk) {
            this.tsk = tsk;
        }

        /**
         * @return the dos
         */
        public Date getDos() {
            return dos;
        }

        /**
         * @param dos the dos to set
         */
        public void setDos(Date dos) {
            this.dos = dos;
        }

        /**
         * @return the firstname
         */
        public String getFirstname() {
            return firstname;
        }

        /**
         * @param firstname the firstname to set
         */
        public void setFirstname(String firstname) {
            this.firstname = firstname;
        }

        /**
         * @return the lastname
         */
        public String getLastname() {
            return lastname;
        }

        /**
         * @param lastname the lastname to set
         */
        public void setLastname(String lastname) {
            this.lastname = lastname;
        }

        /**
         * @return the gn
         */
        public String getGn() {
            return gn;
        }

        /**
         * @param gn the gn to set
         */
        public void setGn(String gn) {
            this.gn = gn;
        }

        /**
         * @return the a1
         */
        public String getA1() {
            return a1;
        }

        /**
         * @param a1 the a1 to set
         */
        public void setA1(String a1) {
            this.a1 = a1;
        }

        /**
         * @return the a2
         */
        public String getA2() {
            return a2;
        }

        /**
         * @param a2 the a2 to set
         */
        public void setA2(String a2) {
            this.a2 = a2;
        }

        /**
         * @return the a3
         */
        public String getA3() {
            return a3;
        }

        /**
         * @param a3 the a3 to set
         */
        public void setA3(String a3) {
            this.a3 = a3;
        }

        /**
         * @return the hphone
         */
        public String getHphone() {
            return hphone;
        }

        /**
         * @param hphone the hphone to set
         */
        public void setHphone(String hphone) {
            this.hphone = hphone;
        }

        /**
         * @return the phone
         */
        public String getPhone() {
            return phone;
        }

        /**
         * @param phone the phone to set
         */
        public void setPhone(String phone) {
            this.phone = phone;
        }

        /**
         * @return the recip
         */
        public String getRecip() {
            return recip;
        }

        /**
         * @param recip the recip to set
         */
        public void setRecip(String recip) {
            this.recip = recip;
        }

        /**
         * @return the city
         */
        public String getCity() {
            return city;
        }

        /**
         * @param city the city to set
         */
        public void setCity(String city) {
            this.city = city;
        }

        /**
         * @return the state
         */
        public String getState() {
            return state;
        }

        /**
         * @param state the state to set
         */
        public void setState(String state) {
            this.state = state;
        }

        /**
         * @return the zip
         */
        public String getZip() {
            return zip;
        }
        /**
         * @param zip the zip to set
         */
        public void setZip(String zip) {
            this.zip = zip;
        }

        /**
         * @return the comm
         */
        public String getComm() {
            return comm;
        }

        /**
         * @param comm the comm to set
         */
        public void setComm(String comm) {
            this.comm = comm;
        }

        /**
         * @return the email
         */
        public String getEmail() {
            return email;
        }

        /**
         * @param email the email to set
         */
        public void setEmail(String email) {
            this.email = email;
        }

        /**
         * @return the rprt
         */
        public String getRprt() {
            return rprt;
        }

        /**
         * @param rprt the rprt to set
         */
        public void setRprt(String rprt) {
            this.rprt = rprt;
        }

        /**
         * @return the sponsor
         */
        public String getSponsor() {
            return sponsor;
        }

        /**
         * @param sponsor the sponsor to set
         */
        public void setSponsor(String sponsor) {
            this.sponsor = sponsor;
        }

        /**
         * @return the ornr
         */
        public String getOrnr() {
            return ornr;
        }

        /**
         * @param ornr the ornr to set
         */
        public void setOrnr(String ornr) {
            this.ornr = ornr;
        }

        /**
         * @return the rate
         */
        public double getRate() {
            return rate;
        }

        /**
         * @param rate the rate to set
         */
        public void setRate(double rate) {
            this.rate = rate;
        }
    }
    
    protected class Descrip {
        private String proximity = null;
        private String bed = null;
        private String reach = null;
        private String res = null;
        private String divert = null;
        private String ice = null;
        private String topo = null;
        private String remark = null;

        /**
         * @return the proximity
         */
        public String getProximity() {
            return proximity;
        }

        /**
         * @param proximity the proximity to set
         */
        public void setProximity(String proximity) {
            this.proximity = proximity;
        }

        /**
         * @return the bed
         */
        public String getBed() {
            return bed;
        }

        /**
         * @param bed the bed to set
         */
        public void setBed(String bed) {
            this.bed = bed;
        }

        /**
         * @return the reach
         */
        public String getReach() {
            return reach;
        }

        /**
         * @param reach the reach to set
         */
        public void setReach(String reach) {
            this.reach = reach;
        }

        /**
         * @return the res
         */
        public String getRes() {
            return res;
        }

        /**
         * @param res the res to set
         */
        public void setRes(String res) {
            this.res = res;
        }

        /**
         * @return the divert
         */
        public String getDivert() {
            return divert;
        }

        /**
         * @param divert the divert to set
         */
        public void setDivert(String divert) {
            this.divert = divert;
        }

        /**
         * @return the ice
         */
        public String getIce() {
            return ice;
        }

        /**
         * @param ice the ice to set
         */
        public void setIce(String ice) {
            this.ice = ice;
        }

        /**
         * @return the topo
         */
        public String getTopo() {
            return topo;
        }

        /**
         * @param topo the topo to set
         */
        public void setTopo(String topo) {
            this.topo = topo;
        }

        /**
         * @return the remark
         */
        public String getRemark() {
            return remark;
        }

        /**
         * @param remark the remark to set
         */
        public void setRemark(String remark) {
            this.remark = remark;
        }
    }
    
    protected class Dcp {
        private String goes = null;
        
        private String owner = null;
        
        private String rptime = null;
        
        private String rptfreq = null;
        
        private String criteria = null;

        /**
         * @return the goes
         */
        public String getGoes() {
            return goes;
        }

        /**
         * @param goes the goes to set
         */
        public void setGoes(String goes) {
            this.goes = goes;
        }

        /**
         * @return the owner
         */
        public String getOwner() {
            return owner;
        }

        /**
         * @param owner the owner to set
         */
        public void setOwner(String owner) {
            this.owner = owner;
        }

        /**
         * @return the rptime
         */
        public String getRptime() {
            return rptime;
        }

        /**
         * @param rptime the rptime to set
         */
        public void setRptime(String rptime) {
            this.rptime = rptime;
        }

        /**
         * @return the rptfreq
         */
        public String getRptfreq() {
            return rptfreq;
        }

        /**
         * @param rptfreq the rptfreq to set
         */
        public void setRptfreq(String rptfreq) {
            this.rptfreq = rptfreq;
        }

        /**
         * @return the criteria
         */
        public String getCriteria() {
            return criteria;
        }

        /**
         * @param criteria the criteria to set
         */
        public void setCriteria(String criteria) {
            this.criteria = criteria;
        }
    }
    
    protected class FloodCat {
        private double majorStage = HydroConstants.MISSING_VALUE;
        private double moderateStage = HydroConstants.MISSING_VALUE;
        private double minorStage = HydroConstants.MISSING_VALUE;
        
        /**
         * @return the majorStage
         */
        public double getMajorStage() {
            return majorStage;
        }
        /**
         * @param majorStage the majorStage to set
         */
        public void setMajorStage(double majorStage) {
            this.majorStage = majorStage;
        }
        /**
         * @return the moderateStage
         */
        public double getModerateStage() {
            return moderateStage;
        }
        /**
         * @param moderateStage the moderateStage to set
         */
        public void setModerateStage(double moderateStage) {
            this.moderateStage = moderateStage;
        }
        /**
         * @return the minorStage
         */
        public double getMinorStage() {
            return minorStage;
        }
        /**
         * @param minorStage the minorStage to set
         */
        public void setMinorStage(double minorStage) {
            this.minorStage = minorStage;
        }
    }
    
    protected class Telem {
        private String type = null;
        private String owner = null;
        private String phone = null;
        private String rptFreq = null;
        private String criteria = null;
        private double cost = HydroConstants.MISSING_VALUE;
        private String payor = null;
        
        /**
         * @return the type
         */
        public String getType() {
            return type;
        }
        /**
         * @param type the type to set
         */
        public void setType(String type) {
            this.type = type;
        }
        /**
         * @return the owner
         */
        public String getOwner() {
            return owner;
        }
        /**
         * @param owner the owner to set
         */
        public void setOwner(String owner) {
            this.owner = owner;
        }
        /**
         * @return the phone
         */
        public String getPhone() {
            return phone;
        }
        /**
         * @param phone the phone to set
         */
        public void setPhone(String phone) {
            this.phone = phone;
        }
        /**
         * @return the rptFreq
         */
        public String getRptFreq() {
            return rptFreq;
        }
        /**
         * @param rptFreq the rptFreq to set
         */
        public void setRptFreq(String rptFreq) {
            this.rptFreq = rptFreq;
        }
        /**
         * @return the criteria
         */
        public String getCriteria() {
            return criteria;
        }
        /**
         * @param criteria the criteria to set
         */
        public void setCriteria(String criteria) {
            this.criteria = criteria;
        }
        /**
         * @return the cost
         */
        public double getCost() {
            return cost;
        }
        /**
         * @param cost the cost to set
         */
        public void setCost(double cost) {
            this.cost = cost;
        }
        /**
         * @return the payor
         */
        public String getPayor() {
            return payor;
        }
        /**
         * @param payor the payor to set
         */
        public void setPayor(String payor) {
            this.payor = payor;
        }
    }
    
    protected class Gage {
        private Date begin = null;
        private String owner = null;
        private String type = null;
        private String remark = null;
        private String maint = null;
        private Date end = null;
        
        /**
         * @return the begin
         */
        public Date getBegin() {
            return begin;
        }
        /**
         * @param begin the begin to set
         */
        public void setBegin(Date begin) {
            this.begin = begin;
        }
        /**
         * @return the owner
         */
        public String getOwner() {
            return owner;
        }
        /**
         * @param owner the owner to set
         */
        public void setOwner(String owner) {
            this.owner = owner;
        }
        /**
         * @return the type
         */
        public String getType() {
            return type;
        }
        /**
         * @param type the type to set
         */
        public void setType(String type) {
            this.type = type;
        }
        /**
         * @return the remark
         */
        public String getRemark() {
            return remark;
        }
        /**
         * @param remark the remark to set
         */
        public void setRemark(String remark) {
            this.remark = remark;
        }
        /**
         * @return the maint
         */
        public String getMaint() {
            return maint;
        }
        /**
         * @param maint the maint to set
         */
        public void setMaint(String maint) {
            this.maint = maint;
        }
        /**
         * @return the end
         */
        public Date getEnd() {
            return end;
        }
        /**
         * @param end the end to set
         */
        public void setEnd(Date end) {
            this.end = end;
        }
    }
    
    protected class Benchmark {
        private String bnum = null;
        private double elev = HydroConstants.MISSING_VALUE;
        private String remark = null;
        
        /**
         * @return the bnum
         */
        public String getBnum() {
            return bnum;
        }
        /**
         * @param bnum the bnum to set
         */
        public void setBnum(String bnum) {
            this.bnum = bnum;
        }
        /**
         * @return the elev
         */
        public double getElev() {
            return elev;
        }
        /**
         * @param elev the elev to set
         */
        public void setElev(double elev) {
            this.elev = elev;
        }
        /**
         * @return the remark
         */
        public String getRemark() {
            return remark;
        }
        /**
         * @param remark the remark to set
         */
        public void setRemark(String remark) {
            this.remark = remark;
        }
    }
    
    protected class Pub {
        private Date begin = null;
        private Date end = null;
        private String ppub = null;
        
        /**
         * @return the begin
         */
        public Date getBegin() {
            return begin;
        }
        
        /**
         * @param begin the begin to set
         */
        public void setBegin(Date begin) {
            this.begin = begin;
        }
        
        /**
         * @return the end
         */
        public Date getEnd() {
            return end;
        }
        
        /**
         * @param end the end to set
         */
        public void setEnd(Date end) {
            this.end = end;
        }
        
        /**
         * @return the ppub
         */
        public String getPpub() {
            return ppub;
        }
        
        /**
         * @param ppub the ppub to set
         */
        public void setPpub(String ppub) {
            this.ppub = ppub;
        }
    }
    
    protected class Datum {
        private Date date = null;
        private double elevation = HydroConstants.MISSING_VALUE;
        
        /**
         * @return the date
         */
        public Date getDate() {
            return date;
        }
        
        /**
         * @param date the date to set
         */
        public void setDate(Date date) {
            this.date = date;
        }
        
        /**
         * @return the elevation
         */
        public double getElevation() {
            return elevation;
        }
        
        /**
         * @param elevation the elevation to set
         */
        public void setElevation(double elevation) {
            this.elevation = elevation;
        }
    }
    
    protected class Crest {
        private Date datcrst = null;
        private String cremark = null;
        private String hw = null;
        private String jam = null;
        private String oldDatum = null;
        private int q = HydroConstants.MISSING_VALUE;
        private double stage = HydroConstants.MISSING_VALUE;
        private String suppress = null;
        private String timcrst = null;
        private String prelim = null;
        
        /**
         * @return the datcrst
         */
        public Date getDatcrst() {
            return datcrst;
        }
        /**
         * @param datcrst the datcrst to set
         */
        public void setDatcrst(Date datcrst) {
            this.datcrst = datcrst;
        }
        /**
         * @return the cremark
         */
        public String getCremark() {
            return cremark;
        }
        /**
         * @param cremark the cremark to set
         */
        public void setCremark(String cremark) {
            this.cremark = cremark;
        }
        /**
         * @return the hw
         */
        public String getHw() {
            return hw;
        }
        /**
         * @param hw the hw to set
         */
        public void setHw(String hw) {
            this.hw = hw;
        }
        /**
         * @return the jam
         */
        public String getJam() {
            return jam;
        }
        /**
         * @param jam the jam to set
         */
        public void setJam(String jam) {
            this.jam = jam;
        }
        /**
         * @return the oldDatum
         */
        public String getOldDatum() {
            return oldDatum;
        }
        /**
         * @param oldDatum the oldDatum to set
         */
        public void setOldDatum(String oldDatum) {
            this.oldDatum = oldDatum;
        }
        /**
         * @return the q
         */
        public int getQ() {
            return q;
        }
        /**
         * @param q the q to set
         */
        public void setQ(int q) {
            this.q = q;
        }
        /**
         * @return the stage
         */
        public double getStage() {
            return stage;
        }
        /**
         * @param stage the stage to set
         */
        public void setStage(double stage) {
            this.stage = stage;
        }
        /**
         * @return the suppress
         */
        public String getSuppress() {
            return suppress;
        }
        /**
         * @param suppress the suppress to set
         */
        public void setSuppress(String suppress) {
            this.suppress = suppress;
        }
        /**
         * @return the timcrst
         */
        public String getTimcrst() {
            return timcrst;
        }
        /**
         * @param timcrst the timcrst to set
         */
        public void setTimcrst(String timcrst) {
            this.timcrst = timcrst;
        }
        /**
         * @return the prelim
         */
        public String getPrelim() {
            return prelim;
        }
        /**
         * @param prelim the prelim to set
         */
        public void setPrelim(String prelim) {
            this.prelim = prelim;
        }

    }
    
    protected class LowWater {
        private Date date = null;
        private int q = HydroConstants.MISSING_VALUE;
        private String remarks = null;
        private double stage = HydroConstants.MISSING_VALUE;
        
        /**
         * @return the date
         */
        public Date getDate() {
            return date;
        }
        /**
         * @param date the date to set
         */
        public void setDate(Date date) {
            this.date = date;
        }
        /**
         * @return the q
         */
        public int getQ() {
            return q;
        }
        /**
         * @param q the q to set
         */
        public void setQ(int q) {
            this.q = q;
        }
        /**
         * @return the remarks
         */
        public String getRemarks() {
            return remarks;
        }
        /**
         * @param remarks the remarks to set
         */
        public void setRemarks(String remarks) {
            this.remarks = remarks;
        }
        /**
         * @return the stage
         */
        public double getStage() {
            return stage;
        }
        /**
         * @param stage the stage to set
         */
        public void setStage(double stage) {
            this.stage = stage;
        }
    }
    
    protected class Flood {
        private double stage = HydroConstants.MISSING_VALUE;
        private String damage = null;
        private String dispStmt = null;
        
        /**
         * @return the stage
         */
        public double getStage() {
            return stage;
        }
        /**
         * @param stage the stage to set
         */
        public void setStage(double stage) {
            this.stage = stage;
        }
        /**
         * @return the damage
         */
        public String getDamage() {
            return damage;
        }
        /**
         * @param damage the damage to set
         */
        public void setDamage(String damage) {
            this.damage = damage;
        }
        /**
         * @return the dispStmt
         */
        public String getDispStmt() {
            return dispStmt;
        }
        /**
         * @param dispStmt the dispStmt to set
         */
        public void setDispStmt(String dispStmt) {
            this.dispStmt = dispStmt;
        }
    }
    
    protected class StaffGageData {
        private ArrayList<Flood> floodList = null;
        private ArrayList<Crest> crestList = null;
        
        /**
         * @return the floodList
         */
        public ArrayList<Flood> getFloodList() {
            return floodList;
        }
        /**
         * @param floodList the floodList to set
         */
        public void setFloodList(ArrayList<Flood> floodList) {
            this.floodList = floodList;
        }
        /**
         * @return the crestList
         */
        public ArrayList<Crest> getCrestList() {
            return crestList;
        }
        /**
         * @param crestList the crestList to set
         */
        public void setCrestList(ArrayList<Crest> crestList) {
            this.crestList = crestList;
        }
    }

    protected class Contacts {
        private String contact = null;
        private String phone = null;
        private String email = null;
        private String remark = null;
        private int priority = HydroConstants.MISSING_VALUE;
        
        /**
         * @return the contact
         */
        public String getContact() {
            return contact;
        }
        /**
         * @param contact the contact to set
         */
        public void setContact(String contact) {
            this.contact = contact;
        }
        /**
         * @return the phone
         */
        public String getPhone() {
            return phone;
        }
        /**
         * @param phone the phone to set
         */
        public void setPhone(String phone) {
            this.phone = phone;
        }
        /**
         * @return the email
         */
        public String getEmail() {
            return email;
        }
        /**
         * @param email the email to set
         */
        public void setEmail(String email) {
            this.email = email;
        }
        /**
         * @return the remark
         */
        public String getRemark() {
            return remark;
        }
        /**
         * @param remark the remark to set
         */
        public void setRemark(String remark) {
            this.remark = remark;
        }
        /**
         * @return the priority
         */
        public int getPriority() {
            return priority;
        }
        /**
         * @param priority the priority to set
         */
        public void setPriority(int priority) {
            this.priority = priority;
        }
    }
    
    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @param lid
     *            the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * @return the descrip
     */
    public Descrip getDescrip() {
        return descrip;
    }

    /**
     * @param descrip the descrip to set
     */
    public void setDescrip(Descrip descrip) {
        this.descrip = descrip;
    }

    /**
     * @return the location
     */
    public Location getLocation() {
        return location;
    }

    /**
     * @param location the location to set
     */
    public void setLocation(Location location) {
        this.location = location;
    }

    /**
     * @return the riverstat
     */
    public Riverstat getRiverstat() {
        return riverstat;
    }

    /**
     * @param riverstat the riverstat to set
     */
    public void setRiverstat(Riverstat riverstat) {
        this.riverstat = riverstat;
    }

    /**
     * @return the stnclass
     */
    public Stnclass getStnclass() {
        return stnclass;
    }

    /**
     * @param stnclass the stnclass to set
     */
    public void setStnclass(Stnclass stnclass) {
        this.stnclass = stnclass;
    }

    /**
     * @return the observer
     */
    public Observer getObserver() {
        return observer;
    }

    /**
     * @param observer the observer to set
     */
    public void setObserver(Observer observer) {
        this.observer = observer;
    }
    
    /**
     * @return the dcp
     */
    public Dcp getDcp() {
        return dcp;
    }

    /**
     * @param dcp the dcp to set
     */
    public void setDcp(Dcp dcp) {
        this.dcp = dcp;
    }

    /**
     * @return the floodCat
     */
    public FloodCat getFloodCat() {
        return floodCat;
    }

    /**
     * @param fcat the floodCat to set
     */
    public void setFloodCat(FloodCat floodCat) {
        this.floodCat = floodCat;
    }

    /**
     * @return the telem
     */
    public Telem getTelem() {
        return telem;
    }

    /**
     * @param telem the telem to set
     */
    public void setTelem(Telem telem) {
        this.telem = telem;
    }

    /**
     * @return the gage
     */
    public Gage getGage() {
        return gage;
    }

    /**
     * @param gage the gage to set
     */
    public void setGage(Gage gage) {
        this.gage = gage;
    }

    /**
     * @return the refer
     */
    public ArrayList<String> getRefer() {
        return refer;
    }

    /**
     * @param refer the refer to set
     */
    public void setRefer(ArrayList<String> refer) {
        this.refer = refer;
    }

    /**
     * @return the benchmark
     */
    public ArrayList<Benchmark> getBenchmark() {
        return benchmark;
    }

    /**
     * @param benchmark the benchmark to set
     */
    public void setBenchmark(ArrayList<Benchmark> benchmark) {
        this.benchmark = benchmark;
    }

    /**
     * @return the gageList
     */
    public ArrayList<Gage> getGageList() {
        return gageList;
    }

    /**
     * @param gageList the gageList to set
     */
    public void setGageList(ArrayList<Gage> gageList) {
        this.gageList = gageList;
    }

    /**
     * @return the pub
     */
    public Pub getPub() {
        return pub;
    }

    /**
     * @param pub the pub to set
     */
    public void setPub(Pub pub) {
        this.pub = pub;
    }

    /**
     * @return the pubList
     */
    public ArrayList<Pub> getPubList() {
        return pubList;
    }

    /**
     * @param pubList the pubList to set
     */
    public void setPubList(ArrayList<Pub> pubList) {
        this.pubList = pubList;
    }

    /**
     * @return the datum
     */
    public Datum getDatum() {
        return datum;
    }

    /**
     * @param datum the datum to set
     */
    public void setDatum(Datum datum) {
        this.datum = datum;
    }

    /**
     * @return the datumList
     */
    public ArrayList<Datum> getDatumList() {
        return datumList;
    }

    /**
     * @param datumList the datumList to set
     */
    public void setDatumList(ArrayList<Datum> datumList) {
        this.datumList = datumList;
    }

    /**
     * @return the crest
     */
    public Crest getCrest() {
        return crest;
    }

    /**
     * @param crest the crest to set
     */
    public void setCrest(Crest crest) {
        this.crest = crest;
    }

    /**
     * @return the crestList
     */
    public ArrayList<Crest> getCrestList() {
        return crestList;
    }

    /**
     * @param crestList the crestList to set
     */
    public void setCrestList(ArrayList<Crest> crestList) {
        this.crestList = crestList;
    }

    /**
     * @return the lowWater
     */
    public LowWater getLowWater() {
        return lowWater;
    }

    /**
     * @param lowWater the lowWater to set
     */
    public void setLowWater(LowWater lowWater) {
        this.lowWater = lowWater;
    }

    /**
     * @return the lowWaterList
     */
    public ArrayList<LowWater> getLowWaterList() {
        return lowWaterList;
    }

    /**
     * @param lowWaterList the lowWaterList to set
     */
    public void setLowWaterList(ArrayList<LowWater> lowWaterList) {
        this.lowWaterList = lowWaterList;
    }

    /**
     * @return the flood
     */
    public Flood getFlood() {
        return flood;
    }

    /**
     * @param flood the flood to set
     */
    public void setFlood(Flood flood) {
        this.flood = flood;
    }

    /**
     * @return the floodList
     */
    public ArrayList<Flood> getFloodList() {
        return floodList;
    }

    /**
     * @param floodList the floodList to set
     */
    public void setFloodList(ArrayList<Flood> floodList) {
        this.floodList = floodList;
    }

    /**
     * @return the staffData
     */
    public StaffGageData getStaffData() {
        return staffData;
    }

    /**
     * @param staffData the staffData to set
     */
    public void setStaffData(StaffGageData staffData) {
        this.staffData = staffData;
    }

    /**
     * @return the contacts
     */
    public Contacts getContacts() {
        return contacts;
    }

    /**
     * @param contacts the contacts to set
     */
    public void setContacts(Contacts contacts) {
        this.contacts = contacts;
    }

    /**
     * @return the contactList
     */
    public ArrayList<Contacts> getContactList() {
        return contactList;
    }

    /**
     * @param contactList the contactList to set
     */
    public void setContactList(ArrayList<Contacts> contactList) {
        this.contactList = contactList;
    }

    /**
     * @return the serviceBackupData
     */
    public ServiceBackupData getServiceBackupData() {
        return serviceBackupData;
    }

    /**
     * @param serviceBackupData the serviceBackupData to set
     */
    public void setServiceBackupData(ServiceBackupData serviceBackupData) {
        this.serviceBackupData = serviceBackupData;
    }
}
