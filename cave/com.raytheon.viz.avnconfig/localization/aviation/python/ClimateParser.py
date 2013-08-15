##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
#
#    Name:
#       %PM%
#       %PID%
#
#    Status:
#       %PS%
#    
#    History:
#       %PL%
#
#    Change Document History:
#       %PIRC%
#
import ClimateStructs
import datetime
import time


class Parser:

	
	def __init__(self):
		self.FLT_FILL = 9.99999986991e+14
		self.INT_FILL = 1073741823
		self.STR_FILL = '\0'

	def process_line(self, line):
		self.si = ClimateStructs.StationInfo()
		self.md = ClimateStructs.MandatoryData()
		self.ad = ClimateStructs.AdditionalData()
		self.parse_station_info(line)
		self.parse_mandatory_data(line)
		self.parse_additional_data(line[105:])
		ret_val = {'si': self.si.get_data(), 'md':self.md.get_data(), 'ad':self.ad.get_data()}
		del self.si, self.md, self.ad
		return ret_val

	def parse_station_info(self,line):
		#self.si.set_info('num_add',line[:4])
		self.si.set_info('station_id',int(line[4:10]))
		self.si.set_info('wban_id',int(line[10:15]))
		self.si.set_info('lat',float(line[28:34]) / 1000.0)
		self.si.set_info('lon',float(line[34:41]) / 1000.0)
		self.si.set_info('elevation',float(line[46:51]))
		self.si.set_info('call_id',line[51:56])
		self.si.set_info('year',int(line[15:19]))

	def parse_mandatory_data(self,line):
		self.md.set_info('date_time',int(self.__convert_char_time(line[15:27])))
		self.md.set_info('source',line[27:28])
		self.md.set_info('type',str(line[41:46]))
		self.md.set_info('wind_dir',float(line[60:63]),999)
		#skip qc code: line[63:64])
		self.md.set_info('wdir_type',line[64:65],'9')
		self.md.set_info('wind_spd',float(line[65:69]) / 10.0,999.9)
		#skip wind speed qc code: line[69:70])
		self.md.set_info('cig',float(line[70:75]),99999.0)
		#skip ceiling qc code: line[75:76])
		self.md.set_info('cig_tool',line[76:77],'9')
		self.md.set_info('cavok',line[77:78])
		self.md.set_info('vis',float(line[78:84]),99999)
		#skip vis qc code: line[84:85])
		self.md.set_info('vis_var_code',line[85:86],'9')
		#skip vis var qc code: line[86:87])
 		self.md.set_info('temp',float(line[87:92]) / 10.0,999.9)
		#skip temp qc code: line[92:93])
		self.md.set_info('dewt',float(line[93:98]) / 10.0,999.9)
		#skip dwpt qc code: line[98:99])
		self.md.set_info('pres',float(line[99:104]) / 10.0,9999.9)
		#skip slp qc code: line[104:105])

	def parse_additional_data(self,line):
		while len(line) > 0:
			value = line[:3].lower()
			
			if ((value == "rem") or (value == "eqd") or (value == "qnn")):
				line = ""
			else:
				try:
					result = getattr(self, 'parse_addtl_' + value[:2]) #found this on the net
					line = line[3:]
					line = result(line)
				except AttributeError:
					line = line[1:]
				except ValueError:
					line = line[1:]
				except IndexError:
					continue
		self.__assign_missing_values()

	def parse_addtl_aa(self,line):						#liquid precip occurence
		self.ad.set_info('prec_period',int(line[:2]))		#period quantity in hours
		prec_depth = int(line[2:6])						#depth dimension
		if (prec_depth == 99999):
			self.ad.set_info('prec_depth',self.FLT_FILL)	
		else:
			self.ad.set_info('prec_depth',(float(prec_depth) / 10.0))
		self.ad.set_info('prec_code',int(line[6:7]))		#condition code
		#skip precip qc code: line[7:8]
		return line[8:]

	def parse_addtl_ac(self,line):						#precip obs history id
		self.ad.set_info('dur_code',line[:1])		#precip ob history duration (hours)
		self.ad.set_info('char_code',line[1:2])		#precip ob history characteristic
		#skip poh qc code: line[2:3]
		return line[3:]

	def parse_addtl_ag(self,line):						#estimated precip id
		self.ad.set_info('disc_code',line[:1])			#discrepancy code
		self.ad.set_info('water_dep',int(line[1:4]))		#estimated water depth
		return line[4:]

	def parse_addtl_aj(self,line):						#snow depth id
		self.ad.set_info('snow_dim',int(line[:4]))			#depth of snow and ice on the ground
		self.ad.set_info('snow_code',line[4:5])		#condition code
		#skip sd qc code: line[5:6]
		liq_dim = int(line[6:12])						#equivalent water depth
		if (liq_dim == 999999):
			self.ad.set_info('liq_dim',self.FLT_FILL)	
		else:
			self.ad.set_info('liq_dim',float(liq_dim) / 10.0)
		self.ad.set_info('liq_code',line[12:13])		#equivalent water condition code
		#skip ewc qc code: line[13:14]
		return line[14:]

	def parse_addtl_al(self,line):						#snow accumulation occurrence id
		self.ad.set_info('snow_period',int(line[:2]))		#snow period quantity (hours)
		self.ad.set_info('snow_accu',int(line[2:5]))		#snow accumulation depth (cm)
		self.ad.set_info('accu_cond',line[5:6])		#snow accumulation condition
		#skip sa qc code: line[6:7]		
		return line[7:]

	def parse_addtl_aw(self,line):						#automated present weather observation
		self.ad.set_info('pres_wx_code',line[:2])			#present weather code
		#skip aac qc code: line[2:3]
		return line[3:]

	def parse_addtl_ay(self,line):						#manual past weather observation
		self.ad.set_info('pt_mwx_code',int(line[:1]))			#atmospheric condition code
		#skip mac qc code: line[1:2]
		self.ad.set_info('pt_mwx_pd',int(line[2:4]))				#period quantity (in hours)
		#skip pq qc code: line[4:5]
		return line[5:]
		
	def parse_addtl_az(self,line):						#automated past weather observation
		self.ad.set_info('pt_awx_code',line[:1])			#automated past condition code
		#skip aac qc code: line[1:2]
		self.ad.set_info('pt_awx_pd',int(line[2:4]))		#period quantity (in hours)
		#skip pq qc code: line[4:5]
		return line[5:]

	def parse_addtl_ed(self,line):						#runway visual range id
		vdr = int(line[:2])
		if vdr == 99:							#runway visual range angle	
			self.ad.set_info('rw_vis_dir',self.INT_FILL)
		else:
			self.ad.set_info('rw_vis_dir',(vdr * 10))
		self.ad.set_info('rw_code',line[2:3])				#runway designator code
		self.ad.set_info('rw_vis_dim',int(line[3:7]))			#runway visual range visibility dimension
		#skip rvrv qc code: line[7:8]
		return line[8:]
	
	def parse_addtl_ga(self,line):						#sky cover layer id
		self.ad.set_info('cov_code',int(line[:2]))				#coverage code
		#skip cc qc code: line[2:3]
		self.ad.set_info('bs_hi_dim',int(line[3:9]))				#base height dimension
		#skip bhd qc code: line[9:10]
		self.ad.set_info('cloud_code',line[10:12])			#cloud type code
		#skip ct qc code: line[12:13]
		return line[13:]

	def parse_addtl_gd(self,line):						#sky cover summation
		self.ad.set_info('cov_sum_st_code',int(line[:1]))			#coverage code 1
		self.ad.set_info('cov_sum_code',line[1:3])			#coverage code 2
		#skip cvg qc code: line[3:4]
		self.ad.set_info('cov_sum_st_dim',int(line[4:10]))			#height dimension
		#skip hgt qc code: line[10:11]
		self.ad.set_info('cov_sum_char_code',int(line[11:12]))		#characteristic code
		return line[12:]
		
	def parse_addtl_gf(self,line):						#sky condition observation
		self.ad.set_info('total_cov_code',line[:2])			#total coverage code
		self.ad.set_info('total_opa_code',line[2:4])			#total opaque coverage code
		#skip tcc qc code: line[4:5]
		self.ad.set_info('low_cov_code',line[5:7])			#total lowest cloud cover code
		#skip tlc qc code: line[7:8]
		self.ad.set_info('low_cld_gen_code',line[8:10])			#low cloud genus code
		#skip lcg qc code: line[10:11]
		self.ad.set_info('low_cld_dim',int(line[11:16]))			#lowest cloud base height dimension
		#skip lcbh qc code: line[16:17]
		self.ad.set_info('mid_cld_gen_code',line[17:19])		#mid cloud genus code
		#skip mcg qc code: line[19:20]
		self.ad.set_info('hi_cld_gen_code',line[20:22])			#high cloud genus code
		#skip hcg qc code: line[22:23]
		return line[23:]
	
	def parse_addtl_gj(self,line):
		self.ad.set_info('sun_dur',int(line[:4]))				#sunshine duration quantity (minutes)
		#skip sund qc code: line[4:5]
		return line[5:]
	
	def parse_addtl_hl(self,line):						#hail observation id
		hsz = int(line[:3])						#hail size
		if (hsz == 999):
			self.ad.set_info('hail_size',self.FLT_FILL)
		else:
			self.ad.set_info('hail_size',float(hsz) / 10.0)
		#skip hsz qc code: line[3:4]
		return line[4:]

	def parse_addtl_ia(self,line):						#ground surface obs id
		if (line[3:4].isalpha()):					#check if we've got ia1 or ia2 
			self.ad.set_info('g2s_code',line[:2])			#ground surface ob code
			#skip gsoc qc code: line[2:3]
			return line[3:]
		else:
			mntp = int(line[:3])					#min temp period quantity (hours)
			if (mntp == 999):
				self.ad.set_info('mint_period',self.FLT_FILL)
			else:
				self.ad.set_info('mint_period',float(mntp) / 10.0)
			mnt = int(line[3:8])					#minimum temperature
			if (mnt == 9999):
				self.ad.set_info('mint',self.FLT_FILL)
			else:
				self.ad.set_info('mint',float(mnt) / 10.0)
			#skip mnt qc code: line[8:9]
			return line[9:]
	
	def parse_addtl_ka(self,line):						#extreme air temp id
		pq = int(line[:3])								#period quantity (hours)
		if (pq == 999):
			self.ad.set_info('x_tp_period',self.FLT_FILL)
		else:
			self.ad.set_info('x_tp_period',float(pq) / 10.0)
		#self.ad.set_info('x_tp_code',line[3:4])				#temperature extreme type code (max, min, etc)
		temp = int(line[4:9])							#extreme temperature value
		if (temp == 99999):
			self.ad.set_info('x_tp',self.FLT_FILL)
		else:
			self.ad.set_info('x_tp',float(temp) / 10.0)
		#skip temp qc code: line[9:10]
		return line[10:]
	
	def parse_addtl_ma(self,line):									#atmospheric pressure
		alt = int(line[:5])
		if (alt == 99999):
			self.ad.set_info('altimeter',self.FLT_FILL)
		else:
			self.ad.set_info('altimeter',float(line[:5]) / 10.0)		#altimeter setting rate
		#skip asr qc code: line[5:6]
		spr = int(line[6:11])									#station pressure rate
		if (spr == 99999):
			self.ad.set_info('st_pres',self.FLT_FILL)
		else:
			self.ad.set_info('st_pres',float(spr) / 10.0)
		#skip spr qc code: line[11:12]
		return line[12:]
	
	def parse_addtl_md(self,line):						#pressure change id
		self.ad.set_info('pres_tr',int(line[:1]))			#tendency code
		#skip tc qc code: line[1:2]
		thq = int(line[2:5])							#three hour pressure change quantity
		if (thq == 999):
			self.ad.set_info('pres_chg_3h',self.FLT_FILL)
		else:
			self.ad.set_info('pres_chg_3h',float(thq) / 10.0)
		#skip thq qc code: line[5:6]
		tfhq = int(line[6:10])							#24 hour pressure change quantity
		if (tfhq == 999):
			self.ad.set_info('pres_chg_24h',self.FLT_FILL)
		else:
			self.ad.set_info('pres_chg_24h',float(tfhq) / 10.0)
		return line[11:]
	
	def parse_addtl_me(self,line):						#geopotential height level id
		self.ad.set_info('isobar_code',line[:1])			#height level code
		self.ad.set_info('isobar_dim',int(line[1:5]))			#height level in meters
		#skip ghlh qc code: line[5:6]
		return line[6:]
	
	def parse_addtl_mv(self,line):						#present weather in vicinity
		self.ad.set_info('wx_vic_code',line[:2])		#present atmo. cond. code
		#skip acc qc code: line[2:3]
		return line[3:]
	
	def parse_addtl_mw(self,line):						#manual occurence id
		self.ad.set_info('pres_wxm_code',line[:2])		#manual atmospheric condition code
		#skip mac qc code: line[2:3]
		return line[3:]
	
	def parse_addtl_oa(self,line):						#supplamentary wind observation
		self.ad.set_info('sup_wd_code',line[:1])			#supp. wind ob type code
		self.ad.set_info('sup_wd_prd',int(line[1:3]))			#period quantity (hours)
		sr = int(line[3:7])					#speed rate
		if (sr == 9999):
			self.ad.set_info('sup_wd_spd',self.FLT_FILL)
		else:
			self.ad.set_info('sup_wd_spd',float(sr) / 10.0)
		#skip sr qc code: line[7:8]
		return line[8:]
	
	def parse_addtl_oc(self,line):						#wind gust observation
		sr = int(line[:4])							#speed rate
		if (sr == 99999):
			self.ad.set_info('wd_gust',self.FLT_FILL)
		else:
			self.ad.set_info('wd_gust',float(sr) / 10.0)
		#skip sr qc code: line[4:5]
		return line[5:]
	
	def parse_addtl_sa(self,line):						#sea surface temp ob
		sst = int(line[:4])						#sst value
		if (sst == 999): #not 9999: it'll be +999 in file
			self.ad.set_info('sea_tp',self.FLT_FILL)
		else:
			self.ad.set_info('sea_tp',float(sst) / 10.0)
		#skip sst qc code: line[4:5]
		return line[5:]
	
	def parse_addtl_ua(self,line):						#wave measurement id
		self.ad.set_info('wv_me_code',line[:1])				#wave measurement method code
		self.ad.set_info('wv_prd_quat',line[1:3])			#wave period quantity (seconds)
		whd = int(line[3:6])						#wave height (meters)
		if (whd == 999):
			self.ad.set_info('wv_hi_dim',self.FLT_FILL)
		else:
			self.ad.set_info('wv_hi_dim',float(whd) / 10.0)
		#skip whd qc code: line[6:7]
		self.ad.set_info('sea_st_code',line[7:9])			#sea state code
		#skip ssc qc code: line[9:10]
		return line[10:]
	
	def parse_addtl_ug(self,line):						#primary swell id
		self.ad.set_info('pm_sw_prd',line[:2])				#primary swell period (seconds)
		psh = int(line[2:5])						#primary swell height (meters)
		if (psh == 999):
			self.ad.set_info('pm_sw_hi',self.FLT_FILL)
		else:
			self.ad.set_info('pm_sw_hi',float(psh) / 10.0)
		self.ad.set_info('pm_sw_dir',line[5:8])
		#skip psp qc code: sefl.line[8:9]
		return line[9:]
	
	def parse_addtl_wa(self,line):						#ice accretion id
		self.ad.set_info('ice_src',line[:1])				#ice accretion source code
		iat = int(line[1:4])						#ice accretion thickness (cm)
		if (iat == 999):
			self.ad.set_info('ice_dim',self.FLT_FILL)
		else:
			self.ad.set_info('ice_dim',float(iat) / 10.0)
		self.ad.set_info('ice_tend',line[4:5])				#ice accretion tendency code
		#skip ia qc code: line[5:6]
		return line[6:]
	
	def parse_addtl_wd(self,line):						#water surface ice observation
		self.ad.set_info('edg_br_code',line[:2])		#ice edge bearing code
		self.ad.set_info('uni_rate',line[2:5])			#ice concentration rate (percent)
		self.ad.set_info('non_uni_rate',line[5:7])		#non-uniform concentration code
		self.ad.set_info('ship_pos',line[7:8])			#ship relative position code
		self.ad.set_info('ship_pen_code',line[8:9])		#ship penetrability code
		self.ad.set_info('ice_trd',line[9:10])			#ice trend code
		self.ad.set_info('ice_dev',line[10:12])			#ice development code
		self.ad.set_info('gro_ber_bit',line[12:13])		#growler-bergy bit presence code
		self.ad.set_info('gro_ber_quant',line[13:16])		#growler-bergy bit quantity
		self.ad.set_info('icebg_quant',line[16:19])		#iceberg quantity
		#skip ice ob qc code: line[19:20]
		return line[20:]
	
	def parse_addtl_wg(self,line):						#ocean ice observation
		self.ad.set_info('ice_edge_code',line[:2])		#edge bearing code
		self.ad.set_info('edge_dis_dim',line[2:4])		#edge distance dimension (km)
		self.ad.set_info('edge_ori_code',line[4:6])		#edge orientation code
		self.ad.set_info('form_type_code',line[6:8])		#formation type code
		self.ad.set_info('navi_eff',line[8:10])		#navigation effect code
		#skip ob qc code: line[10:11]		
		return line[11:]
	
	def __convert_char_time(self, char_time):
		ob_year = int(char_time[:4])
		ob_month = int(char_time[4:6])
		ob_day = int(char_time[6:8])
		ob_hour = int(char_time[8:10])
		ob_minute = int(char_time[10:12])
		
		t = datetime.datetime(ob_year, ob_month, ob_day, ob_hour, ob_minute, 0, 0)

		return time.mktime(t.timetuple())

	def __assign_missing_values(self):
		self.ad.fill_missing('accu_cond',self.INT_FILL,4)
		self.ad.fill_missing('altimeter',self.FLT_FILL)
		self.ad.fill_missing('bs_hi_dim',self.INT_FILL,6)
		self.ad.fill_missing('char_code',self.STR_FILL)
		self.ad.fill_missing('cloud_code',self.INT_FILL,6)
		self.ad.fill_missing('cov_code',self.INT_FILL,6)
		self.ad.fill_missing('cov_sum_char_code',self.INT_FILL,6)
		self.ad.fill_missing('cov_sum_code',self.INT_FILL,6)
		self.ad.fill_missing('cov_sum_st_code',self.INT_FILL,6)
		self.ad.fill_missing('cov_sum_st_dim',self.INT_FILL,6)
		self.ad.fill_missing('disc_code',self.INT_FILL)
		self.ad.fill_missing('dur_code',self.INT_FILL)
		self.ad.fill_missing('g2s_code',self.INT_FILL)
		self.ad.fill_missing('hail_size',self.FLT_FILL)
		self.ad.fill_missing('hi_cld_gen_code',self.INT_FILL)
		self.ad.fill_missing('isobar_code',self.INT_FILL)
		self.ad.fill_missing('isobar_dim',self.INT_FILL)
		self.ad.fill_missing('liq_code',self.INT_FILL)
		self.ad.fill_missing('liq_dim',self.FLT_FILL)
		self.ad.fill_missing('low_cld_dim',self.INT_FILL)
		self.ad.fill_missing('low_cld_gen_code',self.INT_FILL)
		self.ad.fill_missing('low_cov_code',self.INT_FILL)
		self.ad.fill_missing('mid_cld_gen_code',self.INT_FILL)
		self.ad.fill_missing('mint',self.FLT_FILL)
		self.ad.fill_missing('mint_period',self.FLT_FILL)
		self.ad.fill_missing('prec_code',self.INT_FILL,4)
		self.ad.fill_missing('prec_depth',self.FLT_FILL,4)
		self.ad.fill_missing('prec_period',self.INT_FILL,4)
		self.ad.fill_missing('pres_chg_24h',self.FLT_FILL)
		self.ad.fill_missing('pres_chg_3h',self.FLT_FILL)
		self.ad.fill_missing('pres_tr',self.INT_FILL)
		self.ad.fill_missing('pres_wx_code',self.INT_FILL)
		self.ad.fill_missing('pres_wxm_code',self.INT_FILL,7)
		self.ad.fill_missing('pt_awx_code',self.INT_FILL,2)
		self.ad.fill_missing('pt_awx_pd',self.INT_FILL,2)
		self.ad.fill_missing('pt_mwx_code',self.INT_FILL,2)
		self.ad.fill_missing('pt_mwx_pd',self.INT_FILL,2)
		self.ad.fill_missing('rw_code',self.STR_FILL)
		self.ad.fill_missing('rw_vis_dim',self.INT_FILL)
		self.ad.fill_missing('rw_vis_dir',self.INT_FILL)
		self.ad.fill_missing('snow_accu',self.INT_FILL,4)
		self.ad.fill_missing('snow_code',self.INT_FILL)
		self.ad.fill_missing('snow_dim',self.INT_FILL)
		self.ad.fill_missing('snow_period',self.INT_FILL,4)
		self.ad.fill_missing('st_cld_tp_code',self.INT_FILL,6)
		self.ad.fill_missing('st_cld_tp_hi',self.INT_FILL,6)
		self.ad.fill_missing('st_cld_type',self.INT_FILL,6)
		self.ad.fill_missing('st_cov_code',self.INT_FILL,6)
		self.ad.fill_missing('st_pres',self.FLT_FILL)
		self.ad.fill_missing('sun_dur',self.INT_FILL)
		self.ad.fill_missing('sup_wd_code',self.INT_FILL,3)
		self.ad.fill_missing('sup_wd_prd',self.INT_FILL,3)
		self.ad.fill_missing('sup_wd_spd',self.FLT_FILL,3)
		self.ad.fill_missing('total_cov_code',self.INT_FILL)
		self.ad.fill_missing('total_opa_code',self.INT_FILL)
		self.ad.fill_missing('water_dep',self.INT_FILL)
		self.ad.fill_missing('wd_gust',self.FLT_FILL)
		self.ad.fill_missing('wx_vic_code',self.INT_FILL,7)
		self.ad.fill_missing('x_tp',self.FLT_FILL,2)
		self.ad.fill_missing('x_tp_code',self.INT_FILL)
		self.ad.fill_missing('x_tp_period',self.FLT_FILL,2)
		#self.ad.fill_missing('sea_tp',self.FLT_FILL)
		#self.ad.fill_missing('wv_me_code',self.STR_FILL)
		#self.ad.fill_missing('wv_prd_quat',self.FLT_FILL)
		#self.ad.fill_missing('wv_hi_dim',self.FLT_FILL)
		#self.ad.fill_missing('sea_st_code',self.INT_FILL)
		#self.ad.fill_missing('pm_sw_prd',self.FLT_FILL)
		#self.ad.fill_missing('pm_sw_hi',self.FLT_FILL)
		#self.ad.fill_missing('pm_sw_dir',self.FLT_FILL)
		#self.ad.fill_missing('ice_src',self.FLT_FILL)
		#self.ad.fill_missing('ice_dim',self.FLT_FILL)
		#self.ad.fill_missing('ice_tend',self.FLT_FILL)
		#self.ad.fill_missing('edg_br_code',self.INT_FILL)
		#self.ad.fill_missing('uni_rate',self.FLT_FILL)
		#self.ad.fill_missing('non_uni_rate',self.FLT_FILL)
		#self.ad.fill_missing('ship_pos',self.FLT_FILL)
		#self.ad.fill_missing('ship_pen_code',self.INT_FILL)
		#self.ad.fill_missing('ice_trd',self.FLT_FILL)
		#self.ad.fill_missing('ice_dev',self.FLT_FILL)
		#self.ad.fill_missing('gro_ber_bit',self.FLT_FILL)
		#self.ad.fill_missing('gro_ber_quant',self.FLT_FILL)
		#self.ad.fill_missing('icebg_quant',self.FLT_FILL)
		#self.ad.fill_missing('ice_edge_code',self.INT_FILL)
		#self.ad.fill_missing('edge_dis_dim',self.FLT_FILL)
		#self.ad.fill_missing('edge_ori_code',self.INT_FILL)
		#self.ad.fill_missing('form_type_code',self.INT_FILL)
		#self.ad.fill_missing('navi_eff',self.FLT_FILL)

