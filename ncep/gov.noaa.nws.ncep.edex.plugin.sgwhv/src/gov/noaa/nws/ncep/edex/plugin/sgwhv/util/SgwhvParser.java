/**
 * 
 * Parser for significant wave height data
 * 
 * SOFTWARE HISTORSgwhvParserY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	--------	-----------
 * 08/23/11					Chin J Chen	Initial coding from BufrSgwhvParser
 * May 14, 2014 2536        bclement    removed TimeTools usage
 * </pre>
 * 
 * @author Chin J. Chen
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.sgwhv.util;

import gov.noaa.nws.ncep.common.dataplugin.sgwhv.SgwhvRecord;
import gov.noaa.nws.ncep.edex.plugin.sgwhv.decoder.SgwhvSeparator;

import java.util.Calendar;
import java.util.List;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.BUFRDescriptor;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;

public class SgwhvParser {

	/**
	 * process BUFR SGWHV.
	 * 
	 * @param sep
	 *            the BufrSgwhv separator
	 */
	public static SgwhvRecord processSgwhv(SgwhvSeparator sep,
			int subsetNum) {
		int year = -1;
		int month = -1;
		int day = -1;
		int hour = -1;	
		int min = -1;
		int sec = -1;
		SgwhvRecord sgwhvRec = null;
		BUFRDataDocument record = (BUFRDataDocument) sep.next();
		if (record != null) {
			List<IBUFRDataPacket> parameterList = record.getList();
			if (parameterList != null) {
				sgwhvRec = new SgwhvRecord();
				for (IBUFRDataPacket pp : parameterList) {
					int d = pp.getReferencingDescriptor().getDescriptor();
					if (d == BUFRDescriptor.createDescriptor(0, 1, 7)) {
						if (pp.getValue() != null) {

							Long satelliteId = (Long) pp.getValue();
							sgwhvRec.setSatelliteId(satelliteId);
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
					} else if (d == BUFRDescriptor.createDescriptor(0, 4, 6)) {
						if (pp.getValue() != null) {
							sec = ((Double) pp.getValue()).intValue();
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 5, 2)) {
						if (pp.getValue() != null) {
							sgwhvRec.setLat(((Double) pp.getValue()));
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 6, 2)) {
						if (pp.getValue() != null) {
							sgwhvRec.setLon(((Double) pp.getValue()));
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 11, 12)) {
						if (pp.getValue() != null) {
							Double wspd10 = (Double) pp.getValue();
							sgwhvRec.setWspd10(wspd10);
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 22, 21)) {
						if (pp.getValue() != null) {
							Double htwaves = (Double) pp.getValue();
							sgwhvRec.setHtwaves(htwaves);
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 22, 26)) {
						if (pp.getValue() != null) {
							Double sgwhSd = (Double) pp.getValue();
							sgwhvRec.setSgwhSd(sgwhSd);
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 10, 33)) {
						if (pp.getValue() != null) {
							Double altitude = (Double) pp.getValue();
							sgwhvRec.setAltitude(altitude);
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 71)) {
						if (pp.getValue() != null) {
							Long peak = (Long) pp.getValue();
							sgwhvRec.setPeak(peak);
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 77)) {
						if (pp.getValue() != null) {
						    Double altCorrI = (Double) pp.getValue();
						    sgwhvRec.setAltCorrI(altCorrI);
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 78)) {
						if (pp.getValue() != null) {
							Double altCorrD = (Double) pp.getValue();
							sgwhvRec.setAltCorrD(altCorrD);
						}						
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 79)) {
						if (pp.getValue() != null) {
							Double altCorrW = (Double) pp.getValue();
							sgwhvRec.setAltCorrW(altCorrW);
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 82)) {
						if (pp.getValue() != null) {
							Double loopCorr = (Double) pp.getValue();
							sgwhvRec.setLoopCorr(loopCorr);
						}
					} else if (d == BUFRDescriptor.createDescriptor(0, 21, 62)) {
						if (pp.getValue() != null) {
							Double bkst = (Double) pp.getValue();
							sgwhvRec.setBkst(bkst);
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
					sgwhvRec.setObsTime(obstime);
					DataTime dt = new DataTime(obstime);
					sgwhvRec.setDataTime(dt);
				}
			} else {
				System.out.println(" There is no data in bulletin ");
			}
		}
		return sgwhvRec;
	}

}
