/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2010 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 6825 Pine Street, Suite 340
 * Mail Stop B8
 * Omaha, NE 68106
 * 402.291.0100
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wcs.format;

import java.io.OutputStream;

import org.apache.commons.codec.binary.Base64;

import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.edex.wcs.WcsCoverageThriftReponse;
import com.raytheon.uf.edex.wcs.reg.Coverage;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class ThriftFormatter implements WcsDataFormatter {

	protected String id = "application/x-thrift";

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wcs.format.WcsReturnFormat#getIdentifier()
	 */
	@Override
	public String getIdentifier() {
		return id;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wcs.format.WcsReturnFormat#format(com.raytheon.uf
	 * .common.datastorage.records.IDataRecord, java.io.OutputStream, boolean)
	 */
	@Override
	public void format(Coverage coverage, OutputStream out, boolean base64)
			throws Exception {
		WcsCoverageThriftReponse thriftable = new WcsCoverageThriftReponse();
		thriftable.setDataRecord(coverage.getDataRecord());
		thriftable.setCrsString(coverage.getCrs().getName().toString());
		thriftable.setTimeString(coverage.getTime());
		thriftable.setEnvelopeString(coverage.getEnvelope().toString());

		byte[] data = SerializationUtil.transformToThrift(thriftable);
		if (base64) {
			Base64 b64 = new Base64();
			data = b64.encode(data);
		}
		out.write(data);
		out.flush();
	}

}
