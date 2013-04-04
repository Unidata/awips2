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
 * May 5, 2011            bclement     Initial creation
 *
 */

package com.raytheon.uf.edex.wcs.reg;

import java.util.List;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.wcs.WcsException;
import com.vividsolutions.jts.geom.Envelope;

public interface WcsSource {

	public List<CoverageDescription> listCoverages();

	public CoverageDescription describeCoverage(String identifier)
			throws WcsException;

	public Coverage getCoverage(String identifier, DataTime time,
			CoordinateReferenceSystem crs, Envelope bbox,
			List<RangeField> rangeFields) throws WcsException;

	public String getKey();

	/**
	 * Get a quick true or false answer to the question
	 * "Does this WcsSource have a coverage description for this id"
	 * <p>
	 * Implementations should maintain a cache of valid identifiers so that this
	 * lookup is performed quickly. A for-loop is not a recommended solution.
	 * 
	 * @param identifier
	 * @return true if the wcs source has a coverage description for the given
	 *         identifier, false otherwise
	 */
	public boolean hasCoverageDescription(String identifier);
}
