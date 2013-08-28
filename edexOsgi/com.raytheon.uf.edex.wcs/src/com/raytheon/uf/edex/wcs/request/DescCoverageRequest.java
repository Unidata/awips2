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
 * Apr 22, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wcs.request;

import java.util.List;

import net.opengis.wcs.v_1_1_2.DescribeCoverage;

public class DescCoverageRequest extends WcsRequest{

	protected String outputformat = "text/xml; subtype=gml/3.1.1";
	
	protected String[] identifiers;
	
	public DescCoverageRequest() {
		super(Type.DescribeCoverage);
	}

    public DescCoverageRequest(DescribeCoverage req) {
        super(Type.DescribeCoverage);
        this.request = req;
        List<String> ids = req.getIdentifier();
        identifiers = ids.toArray(new String[ids.size()]);
    }

	public String getOutputformat() {
		return outputformat;
	}

	public void setOutputformat(String outputformat) {
		this.outputformat = outputformat;
	}

	public String[] getIdentifiers() {
		return identifiers;
	}

	public void setIdentifiers(String[] identifiers) {
		this.identifiers = identifiers;
	}

}
