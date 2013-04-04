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
 * Apr 26, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs.gml;

import java.util.Arrays;
import java.util.List;

import net.opengis.gml.v_3_1_1.CoordType;
import net.opengis.gml.v_3_1_1.CoordinatesType;
import net.opengis.gml.v_3_1_1.DirectPositionType;
import net.opengis.gml.v_3_1_1.EnvelopeType;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class EnvelopeConverter {

	public Envelope convert(EnvelopeType env) throws Exception {
		DirectPositionType lower = env.getLowerCorner();
		DirectPositionType upper = env.getUpperCorner();
		if (lower == null || upper == null) {
			return handleDeprecated(env);
		}
		return translate(lower, upper);
	}

	protected Envelope translate(DirectPositionType lower,
			DirectPositionType upper) throws Exception {
		List<Double> lowers = lower.getValue();
		List<Double> uppers = upper.getValue();
		if (lowers == null || uppers == null || lowers.size() != 2
				|| uppers.size() != 2) {
			throw new Exception("Unsupported envelope format");
		}
		Coordinate l = new Coordinate(lowers.get(0), lowers.get(1));
		Coordinate u = new Coordinate(uppers.get(0), uppers.get(1));
		return new Envelope(l, u);
	}

	protected Envelope handleDeprecated(EnvelopeType env) throws Exception {
		List<CoordType> coord = env.getCoord();
		List<DirectPositionType> pos = env.getPos();
		CoordinatesType coordinates = env.getCoordinates();
		DirectPositionType lower;
		DirectPositionType upper;
		if (coord != null && coord.size() == 2) {
			CoordType c0 = coord.get(0);
			CoordType c1 = coord.get(1);
			lower = createPos(c0);
			upper = createPos(c1);
		} else if (pos != null && pos.size() == 2) {
			lower = pos.get(0);
			upper = pos.get(1);
		} else if (coordinates != null) {
			String value = coordinates.getValue();
			String[] parts = value.trim().split("\\s");
			lower = createPos(parts[0]);
			upper = createPos(parts[1]);
		} else {
			throw new Exception("Unsupported envelope format");
		}
		return translate(lower, upper);
	}

	/**
	 * @param string
	 * @return
	 */
	private DirectPositionType createPos(String string) {
		return createPos(string.split(","));
	}

	protected DirectPositionType createPos(String[] doubles) {
		Double[] rval = new Double[doubles.length];
		for (int i = 0; i < doubles.length; ++i) {
			rval[i] = Double.parseDouble(doubles[i]);
		}
		return createPos(rval);
	}

	protected DirectPositionType createPos(CoordType coord) throws Exception {
		Double[] rval;
		if (coord.isSetZ()) {
			rval = new Double[] { coord.getX().doubleValue(),
					coord.getY().doubleValue(), coord.getZ().doubleValue() };
		} else if (coord.isSetY()) {
			rval = new Double[] { coord.getX().doubleValue(),
					coord.getY().doubleValue() };
		} else {
			rval = new Double[] { coord.getX().doubleValue() };
		}
		return createPos(rval);
	}

	protected DirectPositionType createPos(Double... coords) {
		DirectPositionType rval = new DirectPositionType();
		rval.setValue(Arrays.asList(coords));
		return rval;
	}

}
