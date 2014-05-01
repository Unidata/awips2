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
package com.raytheon.rcm.config;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Scanner;

import javax.xml.bind.JAXBException;

import com.raytheon.rcm.config.awips1.Awips1RpsListUtil;
import com.raytheon.rcm.request.Request;
import com.raytheon.rcm.request.RpsList;
import com.raytheon.rcm.request.RpsXml;



public class Util {

	public static RadarType getRadarType(RadarConfig rc) {
		int id = rc.getNexradID();
		if (id >= 3000 && id <= 3045) // TODO: 3045 is the current max, but...
			return RadarType.TDWR;
		else if (id >= 4000 && id < 4050) // TODO: may be 4050
			return RadarType.ASR;
		else if (id >= 4100 && id < 5000)
			return RadarType.ARSR;
		else
			return RadarType.WSR;
	}
	
	public static RpsList parseRpsListData(byte[] data, int opModeHint, int vcpHint)
			throws IOException, JAXBException {
		ByteArrayInputStream bis = new ByteArrayInputStream(data);
		RpsList rpsList;
		if (new String(data, 0, 8).equals("RPS List")) {
			Request[] reqs = null;
			Scanner s = new Scanner(bis);
			reqs = Awips1RpsListUtil.parse(s);
			rpsList = new RpsList(opModeHint, vcpHint, reqs);
		} else {
			rpsList = (RpsList) RpsXml.getUnmarshaller().unmarshal(bis);
		}
		return rpsList;
	}

}
