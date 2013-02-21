package com.raytheon.uf.edex.plugin.grib.ogc;

import java.util.HashMap;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.persistence.ElementCollection;
import javax.persistence.Embeddable;
import javax.persistence.Entity;
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
import com.raytheon.uf.edex.plugin.grib.ogc.GribDimension;

@Entity
@Embeddable
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
	public String getDefaultValue(SimpleLayer layer) {
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

	protected DataTime getDefaultTime(SimpleLayer layer) {
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
