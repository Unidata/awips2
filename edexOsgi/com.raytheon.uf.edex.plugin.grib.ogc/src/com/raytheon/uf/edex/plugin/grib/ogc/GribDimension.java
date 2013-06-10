package com.raytheon.uf.edex.plugin.grib.ogc;

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

import java.util.HashMap;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.persistence.ElementCollection;
import javax.persistence.FetchType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.ogc.common.db.SimpleDimension;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;
import com.raytheon.uf.edex.ogc.common.time.ForecastTimeUtil;

/**
 * 
 * Grib Dimension
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/22/2013   1746       dhladky      Modified from a class written by Brian Clements
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */


@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GribDimension extends SimpleDimension {

	public static final String REFTIME_DIM = "REFTIME";

	public static final String FORECAST_OFFSET_DIM = "FORECAST_OFFSET";

	public static final String LEVEL1_DIM = "LEVEL1";

	public static final String LEVEL2_DIM = "LEVEL2";

	public static final String PERTURB_DIM = "PERTURBATION_NUM";

	public static final String ENSEMBLE_DIM = "ENSEMBLE_TYPE";

	public static final String VERSION_DIM = "VERSION";

	private transient DataTime defaultTime = null;

	protected transient Log log = LogFactory.getLog(this.getClass());

	@XmlElement
	@DynamicSerializeElement
	@ElementCollection(fetch = FetchType.EAGER)
	protected Set<String> values;

	public GribDimension() {
	}

	public GribDimension(String name, String units) {
		this.name = name;
		this.units = units;
		this.values = new TreeSet<String>();
	}

	public void setValues(Set<String> values) {
		this.values = values;
	}

	@Override
	public Set<String> getValues() {
		return values;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.ogc.common.db.SimpleDimension#getDefaultValue()
	 */
	@Override
    public String getDefaultValue(SimpleLayer<? extends SimpleDimension> layer) {
		String rval = null;
		if (GribDimension.REFTIME_DIM.equals(name)) {
			DataTime time = getDefaultTime(layer);
			rval = LayerTransformer.format(time.getRefTime());
		} else if (GribDimension.FORECAST_OFFSET_DIM.equals(name)) {
			DataTime time = getDefaultTime(layer);
			rval = time.getFcstTime() + "S";
		} else if (GribDimension.LEVEL1_DIM.equals(name)) {
			rval = getDouble(true);
		} else if (GribDimension.LEVEL2_DIM.equals(name)) {
			rval = getDouble(true);
		} else if (GribDimension.PERTURB_DIM.equals(name)) {
			rval = getInt(true);
		} else if (GribDimension.ENSEMBLE_DIM.equals(name)) {
			rval = getInt(true);
		} else if (GribDimension.VERSION_DIM.equals(name)) {
			rval = getInt(true);
		}
		return rval;
	}

    protected DataTime getDefaultTime(
            SimpleLayer<? extends SimpleDimension> layer) {
		if (defaultTime == null){
			try {
				SortedSet<DataTime> times = new ForecastTimeUtil()
						.getDataTimes(layer, layer.getDefaultTime(),
								new HashMap<String, String>(0));
				defaultTime = times.last();
			} catch (OgcException e) {
				log.error("Problem getting default times", e);
				return new DataTime(layer.getDefaultTime());
			}
		}
		return defaultTime;
	}


}
