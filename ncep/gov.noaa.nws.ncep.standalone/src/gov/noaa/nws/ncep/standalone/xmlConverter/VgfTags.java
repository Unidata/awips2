package gov.noaa.nws.ncep.standalone.xmlConverter;

/**
 * XmlConvert
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 3/25/2009   137         Q. Zhou     Initial created
 * 6/17/2010   137         Q. Zhou     Add Gfa  
 * 9/09/2010   137         Q. Zhou     Added Vol and Ash  
 * 3/15/2011   137         Q. Zhou     Added subtyp for list 
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */

public class VgfTags {
	//version
	static final String vgfHead = "!\n" + "<vg_type>22<vg_class>0<delete>0<filled>0<closed>0<smooth>0<version>0<grptyp>0<grpnum>0" +
    "<maj_col>0<min_col>0<recsz>424<range_min_lat>  0.00<range_min_lon>   0.00<range_max_lat>  0.00<range_max_lon>   0.00<version>Version 6.4<notes>NAWIPS Vector Graphic Format\n";
   
	//hdr
	static final String vg_type = "<vg_type>";
	static final String vg_class = "<vg_class>";
	static final String delete = "<delete>";
	static final String filled = "<filled>";
	static final String closed = "<closed>";
	static final String smooth = "<smooth>";
	static final String version = "<version>";
	static final String grptyp = "<grptyp>";
	static final String grpnum = "<grpnum>";
	static final String maj_col = "<maj_col>";
	static final String min_col = "<min_col>";
	static final String recsz = "<recsz>";
	static final String range_min_lat = "<range_min_lat>";
	static final String range_min_lon = "<range_min_lon>";
	static final String range_max_lat = "<range_max_lat>";
	static final String range_max_lon = "<range_max_lon>";
	
	//common
	static final String numpts = "<numpts>";
	static final String latlon = "<latlon>";
	static final String numsym = "<numsym>";
	static final String offset_xy = "<offset_xy>";
	static final String offset_x = "<offset_x>";
	static final String offset_y = "<offset_y>";
	static final String numwnd = "<numwnd>";
	static final String width = "<width>";
	static final String size = "<size>";
	static final String itxfn = "<itxfn>";
	static final String ithw = "<ithw>";
	static final String sztext = "<sztext>";
	
	
	//line
	static final String lintyp = "<lintyp>";
	static final String lthw = "<lthw>";
	
	static final String lwhw = "<lwhw>";
	
	static final String spltyp = "<spltyp>";
	static final String splstr = "<splstr>";
	static final String spldir = "<spldir>";
	static final String splsiz = "<splsiz>";
	static final String splwid = "<splwid>";
    
	//front
	static final String fcode = "<fcode>";
	static final String fpipsz = "<fpipsz>";
	static final String fpipst = "<fpipst>";
	static final String fpipdr = "<fpipdr>";
	static final String fwidth = "<fwidth>";
	static final String frtlbl = "<frtlbl>";
	
	//sym
	static final String ityp = "<ityp>";
	static final String code = "<code>";

	//text cir
	static final String rotn = "<rotn>";
	static final String sptxtyp = "<sptxtyp>";
	static final String turbsym = "<turbsym>";
	static final String iwidth = "<iwidth>";
	static final String txtcol = "<txtcol>";
	static final String lincol = "<lincol>";
	static final String filcol = "<filcol>";
	static final String ialign = "<ialign>";
	static final String text = "<text>";
	static final String lat = "<lat>";
	static final String lon = "<lon>";
	
	//vect
	static final String wndtyp = "<wndtyp>";
	static final String hdsiz = "<hdsiz>";
	static final String spddir = "<spddir>";
	
	//track
	static final String subtype = "<subtype>";
	static final String npts = "<npts>";
	static final String nipts = "<nipts>";
	static final String ltype1 = "<ltype1>";
	static final String ltype2 = "<ltype2>";
	static final String mtype1 = "<mtype1>";
	static final String mtype2 = "<mtype2>";
	static final String speed = "<speed>";
	static final String dir = "<dir>";
	static final String incr = "<incr>";
	static final String skip = "<skip>";
	static final String times = "<times>";
	
	//sigmet
	static final String linwid = "<linwid>";
	static final String sol = "<sol>";
	static final String area = "<area>";
	static final String fir = "<fir>";
	static final String status = "<status>";
	static final String distance = "<distance>";
	static final String msgid = "<msgid>";
	static final String seqnum = "<seqnum>";
	static final String stime = "<stime>";
	static final String etime = "<etime>";
	static final String remarks = "<remarks>";
	static final String sonic = "<sonic>";
	static final String phenom = "<phenom>";
	static final String phenom2 = "<phenom2>";
	static final String phennam = "<phennam>";
	static final String phenlat = "<phenlat>";
	static final String phenlon = "<phenlon>";
	static final String pres = "<pres>";
	static final String maxwind = "<maxwind>";
	static final String freetext = "<freetext>";
	static final String trend = "<trend>";
	static final String move = "<move>";
	static final String obsfcst = "<obsfcst>";
	static final String obstime = "<obstime>";
	static final String fl = "<fl>";
	static final String spd = "<spd>";
	static final String tops = "<tops>";
	static final String fcstr = "<fcstr>";
	
	//watch
	static final String w_style = "<w_style>";
	static final String w_shape = "<w_shape>";
	static final String w_mrktyp = "<w_mrktyp>";
	static final String w_mrksiz = "<w_mrksiz>";
	static final String w_mrkwid = "<w_mrkwid>";
	static final String w_a0id = "<w_a0id>";
	static final String w_a0lt = "<w_a0lt>";
	static final String w_a0ln = "<w_a0ln>";
	static final String w_a0dis = "<w_a0dis>";
	static final String w_a0dir = "<w_a0dir>";
	static final String w_a1id = "<w_a1id>";
	static final String w_a1lt = "<w_a1lt>";
	static final String w_a1ln = "<w_a1ln>";
	static final String w_a1dis = "<w_a1dis>";
	static final String w_a1dir = "<w_a1dir>";
	static final String w_istat = "<w_istat>";
	static final String w_number = "<w_number>";
	static final String w_iss_t = "<w_iss_t>";
	static final String w_exp_t = "<w_exp_t>";
	static final String w_type = "<w_type>";
	static final String w_severiy = "<w_severiy>";
	static final String w_timezone = "<w_timezone>";
	static final String w_hailsz = "<w_hailsz>";
	static final String w_windg = "<w_windg>";
	static final String w_tops = "<w_tops>";
	static final String w_msmv_d = "<w_msmv_d>";
	static final String w_msmv_s = "<w_msmv_s>";
	static final String w_states = "<w_states>";
	static final String w_adjarea = "<w_adjarea>";
	static final String w_replw = "<w_replw>";
	static final String w_fcstr = "<w_fcstr>";
	static final String w_file = "<w_file>";
	static final String w_issued = "<w_issued>";
	static final String wsm_iss_t = "<wsm_iss_t>";
	static final String wsm_exp_t = "<wsm_exp_t>";
	static final String wsm_ref = "<wsm_ref>";
	static final String wsm_from = "<wsm_from>";
	static final String wsm_meso = "<wsm_meso>";
	static final String wsm_fcstr = "<wsm_fcstr>";
	static final String numcnty = "<numcnty>";
	static final String cn_flag = "<cn_flag>";
	static final String cn_fips = "<cn_fips>";
	static final String cn_ltln = "<cn_ltln>";
	static final String subtyp = "<subtyp>";

	//jet
	static final String splcol = "<splcol>";
	static final String line_latlon = "<line_latlon>";
	static final String nbarb = "<nbarb>";
	static final String jet_barb_ = "<jet_barb_"; //unimplemented tag
	static final String jet_text_ = "<jet_text_"; 
	static final String nhash = "<nhash>";
	static final String jet_hash_ = "<jet_hash_"; 
	
	//tca
	static final String tca_stormNum = "<tca_stormNum>";
	static final String tca_issueStatus = "<tca_issueStatus>";
	static final String tca_basin = "<tca_basin>";
	static final String tca_advisoryNum = "<tca_advisoryNum>";
	static final String tca_stormName = "<tca_stormName>";
	static final String tca_stormType = "<tca_stormType>";
	static final String tca_validTime = "<tca_validTime>";
	static final String tca_timezone = "<tca_timezone>";
	static final String tca_textLat = "<tca_textLat>";
	static final String tca_textLon = "<tca_textLon>";
	static final String tca_textFont = "<tca_textFont>";
	static final String tca_textSize = "<tca_textSize>";
	static final String tca_textWidth = "<tca_textWidth>";
	static final String tca_wwNum = "<tca_wwNum>";
	static final String tca_tcawwStr_ = "<tca_tcawwStr_"; //unimplemented tag
	static final String tca_numBreakPts_ = "<tca_numBreakPts_";
	static final String tca_breakPts_ = "<tca_breakPts_";

	static final String mrktyp = "<mrktyp>";
	static final String mrksiz = "<mrktsiz>";
	static final String mrkwid = "<mrkwid>";
	static final String nitems = "<nitems>";
	static final String item = "<item>";
	
	//gfa
	static final String gfa_nblocks = "<gfa_nblocks>";
	static final String gfa_npts = "<gfa_npts>";
	static final String gfa_areaType = "<gfa_areaType>";
	static final String gfa_fcstHr = "<gfa_fcstHr>";
	static final String gfa_tag = "<gfa_tag>";
	static final String gfa_cycle = "<gfa_cycle>";
	static final String gfa_status = "<gfa_status>";
	static final String Type = "<Type>";
	static final String gfa_areas = "<gfa_areas>";
	static final String gfa_statesList = "<gfa_statesList>";
	static final String gfa_condsBegin = "<gfa_condsBegin>";
	static final String gfa_condsEnd = "<gfa_condsEnd>";
	static final String gfa_subType = "<gfa_subType>";
	static final String gfa_lineWidth = "<gfa_lineWidth>";
	static final String gfa_linelm = "<gfa_linelm>";
	static final String gfa_linetype = "<gfa_linetype>";
	static final String gfa_arrowSize = "<gfa_arrowSize>";
	static final String gfa_txtColor = "<gfa_txtColor>";
	static final String gfa_txtSize = "<gfa_txtSize>";
	static final String gfa_txtFont = "<gfa_txtFont>";
	static final String gfa_txtHardware = "<gfa_txtHardware>";
	static final String gfa_txtWidth = "<gfa_txtWidth>";
	static final String gfa_txtAlign = "<gfa_txtAlign>";
	static final String gfa_txtLayout = "<gfa_txtLayout>";
	static final String gfa_arrow_lat = "<gfa_arrow_lat>";
	static final String gfa_arrow_lon = "<gfa_arrow_lon>";
	static final String gfa_lat = "<gfa_lat>";
	static final String gfa_lon = "<gfa_lon>";
	static final String gfa_points = "<gfa_points>";
	//gfa specific
	static final String gfa_region = "<gfa_region>";
	static final String gfa_top = "<gfa_top>";
	static final String gfa_bottom = "<gfa_bottom>";
	static final String Category = "<Category>";
	static final String Frequency = "<Frequency>";
	static final String Contour = "<Contour>";
	static final String Level = "<Level>";
	static final String gfa_fzlRange = "<gfa_fzlRange>";
	static final String Intensity = "<Intensity>";
	static final String Speed = "<Speed>";
	static final String DUETO = "<DUE TO>";
	static final String Severity = "<Severity>";
	static final String Coverage = "<Coverage>";
	static final String fzlTop = "<gfa_fzlTop>";
	static final String fzlBottom = "<gfa_fzlBottom>";
	static final String CIG = "<VgfTags.CIG>";
	static final String VIS = "<VgfTags.VIS>";
	
	//vol
	static final String name = "<name>";
	static final String number = "<number>";
	static final String location = "<location>";
	static final String elev = "<elev>";
	static final String origstn = "<origstn>";
	static final String vaac = "<vaac>";
	static final String wmoid = "<wmoid>";
	static final String hdrnum = "<hdrnum>";
	static final String year = "<year>";
	static final String advnum = "<advnum>";
	static final String infosorc = "<infosorc>";
	static final String addlsorc = "<addlsorc>";
	static final String details = "<details>";
	static final String obsdate = "<obsdate>";
	//static final String obstime = "<obstime>"; dup
	static final String obsashcld = "<obsashcld>";
	static final String fcst_06 = "<fcst_06>";
	static final String fcst_12 = "<fcst_12>";
	static final String fcst_18 = "<fcst_18>";
	static final String nextadv = "<nextadv>";
	static final String fcstrs = "<fcstrs>";
	static final String corr = "<corr>";
	static final String avcc = "<avcc>";
	//vac
	static final String fhr = "<fhr>";
	static final String flvl1 = "<flvl1>";
	static final String flvl2 = "<flvl2>";
	
	//ccfp
	static final String cover = "<cover>";
	static final String prob = "<prob>";
	static final String growth = "<growth>";
	static final String textlat = "<textlat>";
	static final String textlon = "<textlon>";
	static final String arrowlat = "<arrowlat>";
	static final String arrowlon = "<arrowlon>";
	static final String linetype = "<linetype>";
	static final String szarrow = "<szarrow>";
	static final String fillhi = "<fillhi>";
	static final String fillmed = "<fillmed>";
	static final String filllow = "<filllow>";
	static final String textLayout = "<textLayout>";
	//static final String  = "<>";
}

