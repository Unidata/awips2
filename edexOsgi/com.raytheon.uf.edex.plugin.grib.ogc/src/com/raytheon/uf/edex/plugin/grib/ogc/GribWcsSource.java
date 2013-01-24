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
 * Jun 30, 2011            jelkins     Initial creation
 *
 */
package com.raytheon.uf.edex.plugin.grib.ogc;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.ogc.common.db.SimpleDimension;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;
import com.raytheon.uf.edex.plugin.grib.ogc.GribRecordFinder;
import com.raytheon.uf.edex.wcs.WcsException;
import com.raytheon.uf.edex.wcs.WcsException.Code;
import com.raytheon.uf.edex.wcs.reg.CoverageTransform;
import com.raytheon.uf.edex.wcs.reg.DefaultWcsSource;
import com.raytheon.uf.edex.wcs.reg.RangeAxis;
import com.raytheon.uf.edex.wcs.reg.RangeField;


/**
 * TODO Add Description
 * 
 * @author jelkins
 * @version 1.0
 */
public class GribWcsSource extends DefaultWcsSource {

	private CoverageTransform _cTransform;

	/**
	 * @param props
	 * @param layerTable
	 */
	public GribWcsSource(PluginProperties props, LayerTransformer transformer) {
		super(props, transformer);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wcs.reg.DefaultWcsSource#getCoverageTransform()
	 */
	@Override
	protected CoverageTransform getCoverageTransform() {
		if (_cTransform == null) {
			_cTransform = new CoverageTransform(transformer.getKey()) {

				@Override
				protected List<RangeField> getRangeFields(SimpleLayer layer) {
					Set<? extends SimpleDimension> dims = layer.getDimensions();
					List<RangeField> rval = new ArrayList<RangeField>(
							dims.size());
					for (SimpleDimension dim : dims) {
						rval.add(convert(dim));
					}
					return rval;
				}

				protected RangeField convert(SimpleDimension dim) {
					String name = dim.getName();
					Set<String> fromVals = dim.getValues();
					String units = dim.getUnits();
					RangeField rf = new RangeField(name, null);
					List<RangeAxis> axis = new ArrayList<RangeAxis>(1);
					if ( units == null ){
						// use dim name as axis label
						units = name;
					}
					axis.add(new RangeAxis(units, fromVals));
					rf.setAxis(axis);
					return rf;
				}
			};
		}
		return _cTransform;
	}

	@Override
	protected PluginDataObject getRecord(String identifier, DataTime time,
			List<RangeField> rangeFields) throws WcsException {
		List<GribRecord> res;
		try {
			Map<String, String> dimensions = parseRange(rangeFields);
			Date d = (time == null ? null : time.getRefTime());
			res = GribRecordFinder.find(transformer, transformer.getKey(),
					identifier, d, dimensions);
		} catch (OgcException e) {
			WcsException err = new WcsException(e);
			if (err.getCode().equals(Code.InternalServerError)) {
				log.error("Problem getting grib layer: " + identifier);
			}
			throw err;
		}
		if (res.isEmpty()) {
			throw new WcsException(Code.LayerNotDefined,
					"No layer matching all specified dimensions found");
		}
		if (res.size() > 1) {
			throw new WcsException(Code.InternalServerError,
					"Too many matches for criteria");
		}
		return res.get(0);
	}

	/**
	 * @param rangeFields
	 * @return
	 */
	private Map<String, String> parseRange(List<RangeField> fields) {
		if (fields == null) {
			return new HashMap<String, String>(0);
		}
		Map<String, String> rval = new HashMap<String, String>(fields.size());
		for (RangeField rf : fields) {
			String key = rf.getIdentifier().toLowerCase();
			if (rf.getAxis() == null) {
				continue;
			}
			for (RangeAxis ra : rf.getAxis()) {
				Set<String> keys = ra.getKeys();
				if (keys != null && !keys.isEmpty()) {
					// default to use the first value found
					// TODO should throw and error if multiple range values are
					// provided
					rval.put(key, keys.iterator().next());
					break;
				}
			}
		}
		return rval;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wcs.reg.DefaultWcsSource#getNullPadValue()
	 */
	@Override
	protected Object getNullPadValue() {
		return new Byte((byte) 0);
	}

}
