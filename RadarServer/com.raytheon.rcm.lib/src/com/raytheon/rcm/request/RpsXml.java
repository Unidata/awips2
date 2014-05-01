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
package com.raytheon.rcm.request;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

public class RpsXml {
	private static JAXBContext context;
	private static Unmarshaller u;
	private static Marshaller m;
	
	static {
		try {
			context = JAXBContext.newInstance(RpsList.class);
			u = context.createUnmarshaller();
			m = context.createMarshaller();
			m.setProperty("jaxb.formatted.output", true);
		} catch (JAXBException e) {
			// TODO: Log.errorf("%s", e);
		}
	}
	
	public static Unmarshaller getUnmarshaller() {
		return u;
	}

	public static Marshaller getMarshaller() {
		return m;
	}

}
