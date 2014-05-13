/**
 * process sea surface height anomaly
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	--------	-----------
 * 09/11					Chin J Chen	Initial coding from BufrSshaParser
 * May 14, 2014 2536        bclement    moved WMO Header to common, removed TimeTools usage
 * </pre>
 * 
 * @author Chin J. Chen
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.ssha.util;

import gov.noaa.nws.ncep.common.dataplugin.ssha.SshaRecord;
import gov.noaa.nws.ncep.edex.plugin.ssha.decoder.SshaSeparator;

import java.util.Calendar;
import java.util.List;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.BUFRDescriptor;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;

public class SshaParser {

	/**
	 * process BUFR SSHA.
	 * 
	 * @param sep
	 *            the BufrSsha separator
	 */
	public static SshaRecord processSsha(SshaSeparator sep,
			int subsetNum) {
		int year = -1;
		int month = -1;
		int day = -1;
		int hour = -1;	
		int min = -1;
		int sec = -1;
		Long longsec = -1l;
		SshaRecord sshaRec = null;
		BUFRDataDocument record = (BUFRDataDocument) sep.next();
		if (record != null) {
			List<IBUFRDataPacket> parameterList = record.getList();
			int numAcrs = 0;
			int numSccf = 0;
			int numTmbrst = 0;
			int numHmsl = 0;
			if (parameterList != null) {
				sshaRec = new SshaRecord();
				for (IBUFRDataPacket pp : parameterList) {
					int d = pp.getReferencingDescriptor().getDescriptor();
					if (d == BUFRDescriptor.createDescriptor(0, 1, 7)) {
						if (pp.getValue() != null) {
							sshaRec.setSaid((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 2, 19)) {
						if (pp.getValue() != null) {
							sshaRec.setSiid((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 1, 96)) {
						if (pp.getValue() != null) {
							String strRmNul = (String) pp.getValue();
							strRmNul = strRmNul.replace('\0',' ');
							sshaRec.setStaq(strRmNul);							
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 61)) {
						if (pp.getValue() != null) {						
							String strRmNul = (String) pp.getValue();
							strRmNul = strRmNul.replace('\0',' ');
							sshaRec.setSoftv(strRmNul);		
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 5, 44)) {
						if (pp.getValue() != null) {
							sshaRec.setSacyln((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 5, 40)) {
						if (pp.getValue() != null) {
							sshaRec.setOrbn((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 1, 30)) {
						if (pp.getValue() != null) {							
							String strRmNul = (String) pp.getValue();
							strRmNul = strRmNul.replace('\0',' ');
							sshaRec.setNumid(strRmNul);		
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 4, 1)) {
						year = ((Double) pp.getValue()).intValue();
					} else if (d == BUFRDescriptor.createDescriptor(0, 4, 2)) {
						month = ((Double) pp.getValue()).intValue();
					} else if (d == BUFRDescriptor.createDescriptor(0, 4, 3)) {
						day = ((Double) pp.getValue()).intValue();
					} else if (d == BUFRDescriptor.createDescriptor(0, 4, 4)) {
						hour = ((Double) pp.getValue()).intValue();
					} else if (d == BUFRDescriptor.createDescriptor(0, 4, 5)) {	
						min = ((Double) pp.getValue()).intValue();
					} else if (d == BUFRDescriptor.createDescriptor(0, 4, 7)) {
						if (pp.getValue() != null) {
							longsec = (Math.round((Double) pp.getValue()));
							sec = longsec.intValue();
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 5, 1)) {
						if (pp.getValue() != null) {
							sshaRec.setClath(((Double) pp.getValue()));
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 6, 1)) {
						if (pp.getValue() != null) {
							sshaRec.setClonh(((Double) pp.getValue()));
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 8, 29)) {
						if (pp.getValue() != null) {
							sshaRec.setRsst((Long) pp.getValue());
						}	
					} else if (d == BUFRDescriptor.createDescriptor(0, 8, 74)) {
						if (pp.getValue() != null) {
							sshaRec.setAetp((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 8, 77)) {
						if (pp.getValue() != null) {
							sshaRec.setDsst((Long) pp.getValue());
						}	
					} else if (d == BUFRDescriptor.createDescriptor(0, 40, 11)) {
						if (pp.getValue() != null) {
							sshaRec.setIntf((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 97)) {
						if (pp.getValue() != null) {
							sshaRec.setEeno((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 95)) {
						if (pp.getValue() != null) {
							sshaRec.setAsfl((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 98)) {
						if (pp.getValue() != null) {
							sshaRec.setAdqf((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 99)) {
						if (pp.getValue() != null) {
							sshaRec.setArqf((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 144)) {
						if (pp.getValue() != null) {
							sshaRec.setAlrf((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 96)) {
						if (pp.getValue() != null) {
							sshaRec.setRsfl((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 40, 12)) {
						if (pp.getValue() != null) {
							sshaRec.setRdqf((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 40, 13)) {
						if (pp.getValue() != null) {
							sshaRec.setRbif((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 169)) {
						if (pp.getValue() != null) {
							sshaRec.setIpin((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 40, 23)) {
						if (pp.getValue() != null) {
							sshaRec.setAasf((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 40, 24)) {
						if (pp.getValue() != null) {			
							sshaRec.setMmap((Long) pp.getValue());
						}	
					} else if (d == BUFRDescriptor.createDescriptor(0, 40, 25)) {
						if (pp.getValue() != null) {
							sshaRec.setIfdt((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 22, 151)) {
						if (pp.getValue() != null) {
							sshaRec.setKbor((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 22, 162)) {
						if (pp.getValue() != null) {
							sshaRec.setRkbor((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 22, 163)) {
						if (pp.getValue() != null) {
							sshaRec.setNvpk2((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 160)) {
						if (pp.getValue() != null) {
							sshaRec.setKbic((Double) pp.getValue());
						}	
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 133)) {
						if (pp.getValue() != null) {
							sshaRec.setSbck((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 22, 156)) {
						if (pp.getValue() != null) {
							sshaRec.setKbsw((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 22, 164)) {
						if (pp.getValue() != null) {
							sshaRec.setRksw((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 22, 165)) {
						if (pp.getValue() != null) {
							sshaRec.setNvksw((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 22, 166)) {
						if (pp.getValue() != null) {
							sshaRec.setKncs((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 137)) {
						if (pp.getValue() != null) {
							sshaRec.setKobc((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 138)) {
						if (pp.getValue() != null) {
							sshaRec.setSkobc((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 22, 167)) {
						if (pp.getValue() != null) {
							sshaRec.setNvpkb((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 139)) {
						if (pp.getValue() != null) {
							sshaRec.setKnic((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 118)) {
						numAcrs++;
						if (pp.getValue() != null) {
							if (numAcrs == 1) {
								sshaRec.setAcrs1((Double) pp.getValue());
							} else {
								sshaRec.setAcrs2((Double) pp.getValue());
							}
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 145)) {
						if (pp.getValue() != null) {
							sshaRec.setKagc((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 146)) {
						if (pp.getValue() != null) {
							sshaRec.setRkagc((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 147)) {
						if (pp.getValue() != null) {
							sshaRec.setNvkg((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 22, 168)) {
						if (pp.getValue() != null) {
							sshaRec.setCbor((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 22, 169)) {
						if (pp.getValue() != null) {
							sshaRec.setRcbor((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 22, 170)) {
						if (pp.getValue() != null) {
							sshaRec.setNvpc((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 161)) {
						if (pp.getValue() != null) {
							sshaRec.setCbic((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 162)) {
						if (pp.getValue() != null) {
							sshaRec.setSbcc((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 22, 171)) {
						if (pp.getValue() != null) {
							sshaRec.setCbsw((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 22, 172)) {
						if (pp.getValue() != null) {
							sshaRec.setRcsw((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 22, 173)) {
						if (pp.getValue() != null) {
							sshaRec.setNvcsw((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 22, 174)) {
						if (pp.getValue() != null) {
							sshaRec.setCncs((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 170)) {
						if (pp.getValue() != null) {
							sshaRec.setCcob((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 171)) {
						if (pp.getValue() != null) {
							sshaRec.setRccob((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 22, 175)) {
						if (pp.getValue() != null) {
							sshaRec.setNvpcb((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 172)) {
						if (pp.getValue() != null) {
							sshaRec.setCnia((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 173)) {
						if (pp.getValue() != null) {
							sshaRec.setCagc((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 174)) {
						if (pp.getValue() != null) {
							sshaRec.setRcagc((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 175)) {
						if (pp.getValue() != null) {
							sshaRec.setNvpca((Long) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 2, 153)) {
						numSccf++;
						if (pp.getValue() != null) {
							if (numSccf == 1) {
								sshaRec.setSccf1((Double) pp.getValue());
							} else if (numSccf == 2) {
								sshaRec.setSccf2((Double) pp.getValue());
							} else {
								sshaRec.setSccf3((Double) pp.getValue());
							}
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 12, 63)) {
						numTmbrst++;
						if (pp.getValue() != null) {
							if (numTmbrst == 1) {
								sshaRec.setTmbrst1((Double) pp.getValue());
							} else if (numTmbrst == 2) {
								sshaRec.setTmbrst2((Double) pp.getValue());
							} else {
								sshaRec.setTmbrst3((Double) pp.getValue());
							}
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 13, 90)) {
						if (pp.getValue() != null) {
							sshaRec.setRwvc((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 13, 91)) {
						if (pp.getValue() != null) {
							sshaRec.setRlqc((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 7, 2)) {
						numHmsl++;
						if (pp.getValue() != null) {
							if (numHmsl == 1) {								
								sshaRec.setHmsl1((Double) pp.getValue());
							} else {
								sshaRec.setHmsl2((Double) pp.getValue());
							}
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 11, 97)) {
						if (pp.getValue() != null) {
							sshaRec.setWspa((Double) pp.getValue());
						}	
					} else if (d == BUFRDescriptor.createDescriptor(0, 11, 98)) {
						if (pp.getValue() != null) {
							sshaRec.setWspr((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 11, 95)) {
						if (pp.getValue() != null) {
							sshaRec.setUmwv((Double) pp.getValue());
						}	
					} else if (d == BUFRDescriptor.createDescriptor(0, 11, 96)) {
						if (pp.getValue() != null) {
							sshaRec.setVwmv((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 10, 96)) {
						if (pp.getValue() != null) {
							sshaRec.setMdyt((Double) pp.getValue());
						}	
					} else if (d == BUFRDescriptor.createDescriptor(0, 10, 81)) {
						if (pp.getValue() != null) {
							sshaRec.setAlre((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 10, 82)) {
						if (pp.getValue() != null) {
							sshaRec.setIalr((Double) pp.getValue());
						}						
					} else if (d == BUFRDescriptor.createDescriptor(0, 10, 83)) {
						if (pp.getValue() != null) {
							sshaRec.setOnap((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 10, 101)) {
						if (pp.getValue() != null) {
							sshaRec.setSonaw((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 132)) {
						if (pp.getValue() != null) {
							sshaRec.setIcmk((Double) pp.getValue());
						}						
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 163)) {
						if (pp.getValue() != null) {
							sshaRec.setAick((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 126)) {
						if (pp.getValue() != null) {
							sshaRec.setMdtc((Double) pp.getValue());
						}	
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 128)) {
						if (pp.getValue() != null) {
							sshaRec.setMwtc((Double) pp.getValue());
						}	
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 164)) {
						if (pp.getValue() != null) {
							sshaRec.setRwtc((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 10, 85)) {
						if (pp.getValue() != null) {
							sshaRec.setMssh((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 10, 97)) {
						if (pp.getValue() != null) {
							sshaRec.setMsha((Double) pp.getValue());
						}	
					} else if (d == BUFRDescriptor.createDescriptor(0, 10, 86)) {
						if (pp.getValue() != null) {
							sshaRec.setGeodh((Double) pp.getValue());
						}	
					} else if (d == BUFRDescriptor.createDescriptor(0, 10, 87)) {
						if (pp.getValue() != null) {
							sshaRec.setOdle((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 10, 92)) {
						if (pp.getValue() != null) {
							sshaRec.setSeth((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 10, 88)) {
						if (pp.getValue() != null) {
							sshaRec.setTgoth1((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 10, 89)) {
						if (pp.getValue() != null) {
							sshaRec.setTgoth2((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 10, 98)) {
						if (pp.getValue() != null) {
							sshaRec.setLths1((Double) pp.getValue());
						}	
					} else if (d == BUFRDescriptor.createDescriptor(0, 10, 99)) {
						if (pp.getValue() != null) {
							sshaRec.setLths2((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 10, 90)) {
						if (pp.getValue() != null) {
							sshaRec.setLpth((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 10, 100)) {
						if (pp.getValue() != null) {
							sshaRec.setNlth((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 10, 93)) {
						if (pp.getValue() != null) {
							sshaRec.setGpth((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 127)) {
						if (pp.getValue() != null) {
							sshaRec.setIbco((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 40, 14)) {
						if (pp.getValue() != null) {
							sshaRec.setHfstc((Double) pp.getValue());
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 10, 102)) {
						if (pp.getValue() != null) {
							sshaRec.setSsha((Double) pp.getValue());
						}
					}
				}

				/*
				 * Create time stamp.
				 */
				if ((year > 0) && (month > 0) && (day > 0) && (hour >= 0)) {
                    Calendar baseTime = TimeUtil.newGmtCalendar(year, month,
							day);
					baseTime.set(Calendar.HOUR_OF_DAY, hour);
					baseTime.set(Calendar.MINUTE, min);
					baseTime.set(Calendar.SECOND, sec) ;
                    Calendar obstime = (Calendar) baseTime.clone();
					sshaRec.setObsTime(obstime);
					DataTime dt = new DataTime(obstime);
					sshaRec.setDataTime(dt);
				}
			} else {
				System.out.println("Info: There is no data in ssha bulletin.");
			}
		}
		return sshaRec;
	}

}
