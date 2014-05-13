/**
 * Parser for significant wave height data
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	--------	-----------
 * 04/21/10     208         F. J. Yen   Initial coding to11dr3
 * May 14, 2014 2536        bclement    removed TimeTools usage
 * </pre>
 * 
 * @author F. J. Yen
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.sgwh.util;

import static com.raytheon.uf.edex.decodertools.bufr.packets.DataPacketTypes.RepSubList;
import gov.noaa.nws.ncep.common.dataplugin.sgwh.SgwhRecord;
import gov.noaa.nws.ncep.edex.plugin.sgwh.decoder.SgwhSeparator;

import java.util.Calendar;
import java.util.List;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.BUFRDescriptor;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

public class SgwhParser {

	/**
	 * process BUFR SGWH.
	 * 
	 * @param sep
	 *            the BufrSgwh separator
	 */
	@SuppressWarnings("unchecked")
	public static SgwhRecord processSgwh(SgwhSeparator sep,
			int subsetNum) {
		int year = -1;
		int month = -1;
		int day = -1;
		int hour = -1;
		int min = -1;
		int sec = -1;
		Long longsec = -1l;
		boolean fostFlag = false;

		SgwhRecord sgwhRec = null;
		BUFRDataDocument record = (BUFRDataDocument) sep.next();
		if (record != null) {
			List<IBUFRDataPacket> parameterList = record.getList();
			if (parameterList != null) {
				sgwhRec = new SgwhRecord();
				int repGroup = 1;
				int ssinNum = 1;
				int afsiNum = 1;
				for (IBUFRDataPacket pp : parameterList) {
					int d = pp.getReferencingDescriptor().getDescriptor();
					if (d == BUFRDescriptor.createDescriptor(0, 1, 7)) {
						Long said = (Long) pp.getValue();
						sgwhRec.setSaid(said);
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 60)) {
						Long swid = (Long) pp.getValue();
						sgwhRec.setSwid(swid);
					} else if (d == BUFRDescriptor.createDescriptor(0, 1, 33)) {
						Long ogce = (Long) pp.getValue();
						sgwhRec.setOgce(ogce);
					} else if (d == BUFRDescriptor.createDescriptor(0, 2, 48)) {
						Long ssin = (Long) pp.getValue();
						if (ssinNum == 1) {
							sgwhRec.setSsin1(ssin);
						} else if (ssinNum == 2) {
							sgwhRec.setSsin2(ssin);
						} else {
							System.out.println("   BUFRSGWH WARNING:  More than 2 SSIN");
						}
						ssinNum++;
					} else if (d == BUFRDescriptor.createDescriptor(0, 5, 40)) {
						Long orbn = (Long) pp.getValue();
						sgwhRec.setOrbn(orbn);
					} else if (d == BUFRDescriptor.createDescriptor(0, 7, 1)) {
						// reference is -400
						Double selv = (Double) pp.getValue();
						sgwhRec.setSelv(selv);
					} else if (d == BUFRDescriptor.createDescriptor(0, 7, 5)) {
						Double hinc = (Double) pp.getValue();
						sgwhRec.setHinc(hinc);
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
						longsec = (Math.round((Double) pp.getValue()));
						sec = longsec.intValue();
					} else if (d == BUFRDescriptor.createDescriptor(0, 5, 1)) {
						sgwhRec.setClath(((Double) pp.getValue()));
					} else if (d == BUFRDescriptor.createDescriptor(0, 6, 1)) {
						sgwhRec.setClonh(((Double) pp.getValue()));
					} else if (d == BUFRDescriptor.createDescriptor(0, 8, 29)) {
						Long rsst = (Long) pp.getValue();
						sgwhRec.setRsst(rsst);
					} else if (d == BUFRDescriptor.createDescriptor(0, 8, 74)) {
						Long aetp = (Long) pp.getValue();
						sgwhRec.setAetp(aetp);
					} else if (d == BUFRDescriptor.createDescriptor(0, 8, 12)) {
						Long lsql = (Long) pp.getValue();
						sgwhRec.setLsql(lsql);
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 95)) {
						Long asfl = (Long) pp.getValue();
						sgwhRec.setAsfl(asfl);
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 96)) {
						Long rsfl = (Long) pp.getValue();
						sgwhRec.setRsfl(rsfl);
					} else if (d == BUFRDescriptor.createDescriptor(0, 25, 97)) {
						Long eeno = (Long) pp.getValue();
						sgwhRec.setEeno(eeno);
					} else if (d == BUFRDescriptor.createDescriptor(0, 31, 21)) {
						Long afsi = (Long) pp.getValue();
						if (afsiNum == 1) {						
							sgwhRec.setAfssgwh(afsi);
						} else if (afsiNum == 2) {
							sgwhRec.setAfssona(afsi);
						}
						afsiNum++;					
						
					} else if (d == BUFRDescriptor.createDescriptor(0, 22, 70)) {
						// scale is 2
						Double sgwh = (Double) pp.getValue();
						if (fostFlag) {
							fostFlag = false;
							sgwhRec.setSgwhstd(sgwh);
						} else {
							sgwhRec.setSgwh(sgwh);
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 128)) {
						Long nvpp = (Long) pp.getValue();
						sgwhRec.setNvpp(nvpp);
					} else if (d == BUFRDescriptor.createDescriptor(0, 8, 23)) {
						fostFlag = true;
						Long fost = (Long) pp.getValue();
						sgwhRec.setFostsgwh(fost);
					} else if (d == BUFRDescriptor.createDescriptor(0, 2, 173)) {
						Double sona = (Double) pp.getValue();
						sgwhRec.setSona(sona);
					} else if (d == BUFRDescriptor.createDescriptor(0, 13, 90)) {
						Double rwvc = (Double) pp.getValue();
						sgwhRec.setRwvc(rwvc);
					} else if (d == BUFRDescriptor.createDescriptor(0, 13, 91)) {
						Double rlqc = (Double) pp.getValue();
						sgwhRec.setRlqc(rlqc);
					} else if (d == 2) {
						// Replication
						int repIndex = 1;
						if ((pp instanceof BUFRSublistPacket)
								&& (RepSubList.getPacketType().equals(pp
										.getUnits()))) {
							List<IBUFRDataPacket> subList = (List<IBUFRDataPacket>) pp
									.getValue();

							for (IBUFRDataPacket pList : subList) {
								List<IBUFRDataPacket> sList = (List<IBUFRDataPacket>) pList
										.getValue();
								switch (repGroup) {
								case 1:
									processSubListGrp1(sList, sgwhRec, repIndex);
									break;
								case 2:
									processSubListGrp2 (sList, sgwhRec, repIndex);
									break;
								case 3: processSubListGrp3 (sList, sgwhRec, repIndex);
								    break;
								default:
									System.out
											.println("BUFRSGWH WARNING:  Unexpected new replication ignored");
								}
								repIndex++;
							}
						}
						repGroup++;
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
					Calendar obstime = TimeTools.copy(baseTime);
					sgwhRec.setObsTime(obstime);
					DataTime dt = new DataTime(obstime);
					sgwhRec.setDataTime(dt);
				}
			} else {
				System.out.println(" There is no data in bulletin ");
			}
		}
		return sgwhRec;
	}

	/**
	 * Processes a single replication of data from the parameter sublist for the
	 * first replication group that was decoded.
	 * 
	 * @param singleRepList
	 *            A sublist containing one replication of data.
	 * @param sgwhRec
	 * 			  The sgwh record
	 * @param repIndex
	 *            The current replication index
	 */
	private static void processSubListGrp1(List<IBUFRDataPacket> singleRepList,
			SgwhRecord sgwhRec, int repIndex) {

		boolean fostRFlag = false;
		Long afsg = null;
		if (singleRepList != null) {

			if (singleRepList.get(0).getValue() != null) {
				IBUFRDataPacket sp = singleRepList.get(0);
				int descr = sp.getReferencingDescriptor().getDescriptor();
				if (descr == BUFRDescriptor.createDescriptor(0, 8, 76)) {
					// Type of Band
					Long tobd = (Long) sp.getValue();
					switch (repIndex) {
					case 1:
						sgwhRec.setTobdg1r1(tobd);
						break;
					case 2:
						sgwhRec.setTobdg1r2(tobd);
						break;
					default:
						System.out
								.println("BUFRSGWH WARNING:  Too many replications for Group 1 ");
					}
				}

				sp = singleRepList.get(1);
				descr = sp.getReferencingDescriptor().getDescriptor();
				if (descr == BUFRDescriptor.createDescriptor(0, 31, 21)) {
					// Associated Field Significance:  Set afsg to the code table value.  (If it
					// is 1, it means the additional 1-bit indicator of quality.  Following how
					// Tamdar is processing this, these additional bits will be ignored for now.)
					afsg = (Long) sp.getValue();
					switch (repIndex) {
					case 1:
						sgwhRec.setAfsbkstg1r1(afsg);
						break;
					case 2:
						sgwhRec.setAfsbkstg1r2(afsg);
						break;
					default:
						System.out
								.println("BUFRSGWH WARNING:  Too many replications for Group 1 ");
					}
				}

				fostRFlag = false;
				for (int ir = 2; ir < 5; ir++) {
					sp = singleRepList.get(ir);
					descr = sp.getReferencingDescriptor().getDescriptor();
					if (descr == BUFRDescriptor.createDescriptor(0, 21, 62)) {
						// Backscatter
						Double bkst = (Double) sp.getValue();
						if (bkst == null) bkst = -9999.d;
						switch (repIndex) {
						case 1:
							if (fostRFlag) {
								fostRFlag = false;
								sgwhRec.setBkststdg1r1(bkst);
							} else {
								sgwhRec.setBkstg1r1(bkst);
							}
							break;
						case 2:
							if (fostRFlag) {
								fostRFlag = false;
								sgwhRec.setBkststdg1r2(bkst);
							} else {
								sgwhRec.setBkstg1r2(bkst);
							}
							break;
						default:
							System.out
									.println("BUFRSGWH WARNING:  Too many replications for Group 1 ");
						}
					} else if (descr == BUFRDescriptor.createDescriptor(0, 8,
							23)) {
						if ((Long) sp.getValue() != 10) {
							System.out
							.println("BUFRSGWH WARNING: FOST for BKST is not the expected standard deviation");
						} else {
							fostRFlag = true;
							Long fost = (Long) sp.getValue();
							switch (repIndex) {	
							case 1:
								sgwhRec.setFostbkstg1r1(fost);
								break;
							case 2:
								sgwhRec.setFostbkstg1r2(fost);
								break;
							default:
								System.out
								.println("BUFRSGWH WARNING:  Too many replications for Group 1 ");
							}
						}	

				} else {
						System.out
								.println("BUFRSGWH WARNING:  unexpected descriptor = "
										+ descr);
					}
				}

				sp = singleRepList.get(5);
				descr = sp.getReferencingDescriptor().getDescriptor();
				if (descr == BUFRDescriptor.createDescriptor(0, 31, 21)) {
					// Associated Field Significance:  Set afsg to the code table value.  (If it
					// is 1, it means the additional 1-bit indicator of quality.  Following how
					// Tamdar is processing this, these additional bits will be ignored for now.)
					afsg = (Long) sp.getValue();
					switch (repIndex) {
					case 1:
						sgwhRec.setAfsselvg1r1(afsg);
						break;
					case 2:
						sgwhRec.setAfsselvg1r2(afsg);
						break;
					default:
						System.out
								.println("BUFRSGWH WARNING:  Too many replications for Group 1 ");
					}
				}

				fostRFlag = false;
				for (int ir = 6; ir < 10; ir++) {
					sp = singleRepList.get(ir);
					descr = sp.getReferencingDescriptor().getDescriptor();
					if (descr == BUFRDescriptor.createDescriptor(0, 7, 1)) {
						// Height of station
						Double selv = (Double) sp.getValue();
						switch (repIndex) {
						case 1:
							if (fostRFlag) {
								fostRFlag = false;
								sgwhRec.setSelvstdg1r1(selv);
							} else {
								sgwhRec.setSelvg1r1(selv);
							}
							break;
						case 2:
							if (fostRFlag) {
								fostRFlag = false;
								sgwhRec.setSelvstdg1r2(selv);
							} else {
								sgwhRec.setSelvg1r2(selv);
							}
							break;
						default:
							System.out
									.println("BUFRSGWH WARNING:  Too many replications for Group 1 ");
						}
					} else if (descr == BUFRDescriptor.createDescriptor(0, 8,
							23)) {
						if ((Long) sp.getValue() != 10) {
							System.out
									.println("BUFRSGWH WARNING: FOST for SELV is not the expected standard deviation");
						} else {
							fostRFlag = true;
							Long fost = (Long) sp.getValue();							
							switch (repIndex) {	
							case 1:
								sgwhRec.setFostselvg1r1(fost);
								break;
							case 2:
								sgwhRec.setFostselvg1r2(fost);
								break;
							default:
								System.out
								.println("BUFRSGWH WARNING:  Too many replications for Group 1 ");
							}							
						}
					} else if (descr == BUFRDescriptor
							.createDescriptor(0, 7, 5)) {
						// Height increment
						Double hinc = (Double) sp.getValue();
						switch (repIndex) {
						case 1:
							sgwhRec.setHincg1r1(hinc);
							break;
						case 2:
							sgwhRec.setHincg1r2(hinc);
							break;
						default:
							System.out
									.println("BUFRSGWH WARNING:  Too many replications for Group 1 ");
						}

					} else {
						System.out
								.println("BUFRSGWH WARNING:  unexpected descriptor = "
										+ descr);
					}
				}

				sp = singleRepList.get(10);
				descr = sp.getReferencingDescriptor().getDescriptor();
				if (descr == BUFRDescriptor.createDescriptor(0, 21, 128)) {
					// Number of valid points per second used to derive previous parameters
					Long nvpp = (Long) sp.getValue();
					switch (repIndex) {
					case 1:
						sgwhRec.setNvppg1r1(nvpp);

						break;
					case 2:
						sgwhRec.setNvppg1r2(nvpp);

						break;
					default:
						System.out
								.println("BUFRSGWH WARNING:  Too many replications for Group 2");
					}
				}
			}
		}
	}
	/**
	 * Processes a single replication of data from the parameter sublist for the
	 * second replication group that was decoded.
	 * 
	 * @param singleRepList
	 *            A sublist containing one replication of data.
	 * @param sgwhRec
	 * 			  The sgwh record
	 * @param repIndex
	 *            The current replication index
	 */
	private static void processSubListGrp2(List<IBUFRDataPacket> singleRepList,
			SgwhRecord sgwhRec, int repIndex) {
		Long afsg = null;
		if (singleRepList != null) {

			if (singleRepList.get(0).getValue() != null) {
				IBUFRDataPacket sp = singleRepList.get(0);
				int descr = sp.getReferencingDescriptor().getDescriptor();
				if (descr == BUFRDescriptor.createDescriptor(0, 2, 121)) {
					// Mean Frequency
					Double mefr = (Double) sp.getValue();
					switch (repIndex) {
					case 1:
						sgwhRec.setMefrg2r1(mefr);
						break;
					case 2:
						sgwhRec.setMefrg2r2(mefr);
						break;
					case 3:
						sgwhRec.setMefrg2r3(mefr);
						break;
					default:
						System.out
								.println("BUFRSGWH WARNING:  Too many replications for Group 2 ");
					}
				}

				sp = singleRepList.get(1);
				descr = sp.getReferencingDescriptor().getDescriptor();
				if (descr == BUFRDescriptor.createDescriptor(0, 31, 21)) {
					// Associated Field Significance:  Set afsg to the code table value.  (If it
					// is 1, it means the additional 1-bit indicator of quality.  Following how
					// Tamdar is processing this, these additional bits will be ignored for now.)
					afsg = (Long) sp.getValue();
					switch (repIndex) {
					case 1:
						sgwhRec.setAfstmbrg2r1(afsg);
						break;
					case 2:
						sgwhRec.setAfstmbrg2r2(afsg);;
						break;
					case 3:
						sgwhRec.setAfstmbrg2r3(afsg);;
						break;
					default:
						System.out
								.println("BUFRSGWH WARNING:  Too many replications for Group 2 ");
					}
				}

				sp = singleRepList.get(2);
				descr = sp.getReferencingDescriptor().getDescriptor();
				if (descr == BUFRDescriptor.createDescriptor(0, 12, 163)) {
					// Brightness Temperature
					Double tmbr = (Double) sp.getValue();
					switch (repIndex) {
					case 1:
						sgwhRec.setTmbrg2r1(tmbr);
						break;
					case 2:
						sgwhRec.setTmbrg2r2(tmbr);
						break;
					case 3:
						sgwhRec.setTmbrg2r3(tmbr);
						break;
					default:
						System.out
						.println("BUFRSGWH WARNING:  Too many replications for Group 2 ");
					}
				}				
			}
		}
	}
	/**
	 * Processes a single replication of data from the parameter sublist for the
	 * third replication group that was decoded.
	 * 
	 * @param singleRepList
	 *            A sublist containing one replication of data.
	 * @param sgwhRec
	 * 			  The sgwh record
	 * @param repIndex
	 *            The current replication index
	 */
	private static void processSubListGrp3(List<IBUFRDataPacket> singleRepList,
			SgwhRecord sgwhRec, int repIndex) {

		if (singleRepList != null) {

			if (singleRepList.get(0).getValue() != null) {
				IBUFRDataPacket sp = singleRepList.get(0);
				int descr = sp.getReferencingDescriptor().getDescriptor();
				if (descr == BUFRDescriptor.createDescriptor(0, 2, 23)) {
					// Satellite-derived wind computation method
					Long swcm = (Long) sp.getValue();
					switch (repIndex) {
					case 1:
						sgwhRec.setSwcmg3r1(swcm);
						break;
					case 2:
						sgwhRec.setSwcmg3r2(swcm);
						break;
					default:
						System.out
								.println("BUFRSGWH WARNING:  Too many replications for Group 3 ");
					}
				}
				
				sp = singleRepList.get(1);
				descr = sp.getReferencingDescriptor().getDescriptor();
				if (descr == BUFRDescriptor.createDescriptor(0, 11, 12)) {
					// Wind speed at 10 m
					Double ws10 = (Double) sp.getValue();
					switch (repIndex) {
					case 1:
						sgwhRec.setWs10g3r1(ws10);
						break;
					case 2:
						sgwhRec.setWs10g3r2(ws10);
						break;
					default:
						System.out
								.println("BUFRSGWH WARNING:  Too many replications for Group 3");
					}
				}			
			}
		}
	}
}
