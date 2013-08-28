/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */

package com.raytheon.uf.edex.plugin.grib.ogc;

import java.util.Collection;
import java.util.HashMap;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.lang.StringUtils;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.ogc.common.db.SimpleDimension;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;
import com.raytheon.uf.edex.ogc.common.level.LevelDimUtil;
import com.raytheon.uf.edex.ogc.common.time.ForecastTimeUtil;

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GribDimension extends SimpleDimension {

    private static final long serialVersionUID = 1232298678216203380L;

    public static final String REFTIME_DIM = "REFTIME";

	public static final String FORECAST_OFFSET_DIM = "FORECAST_OFFSET";

    public static final String PARAM_DIM = "PARAMETER";

    private transient DataTime defaultTime = null;

    protected transient IUFStatusHandler log = UFStatus.getHandler(this
            .getClass());

    @XmlElement
    @DynamicSerializeElement
    protected Set<String> values;

    public GribDimension() {
    }

    public GribDimension(String name, String units) {
        this.name = name;
        this.units = units;
        this.values = new TreeSet<String>();
    }

    /**
     * @param otherDim
     */
    public GribDimension(GribDimension other) {
        super(other);
        this.values = new TreeSet<String>(other.values);
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
    public String getDefaultValue(SimpleLayer<?> layer) {
		String rval = null;
		if (GribDimension.REFTIME_DIM.equals(name)) {
			DataTime time = getDefaultTime(layer);
			rval = LayerTransformer.format(time.getRefTime());
		} else if (GribDimension.FORECAST_OFFSET_DIM.equals(name)) {
			DataTime time = getDefaultTime(layer);
			rval = time.getFcstTime() + "S";
        } else if (name.startsWith(LevelDimUtil.LEVEL_DIM_PREFIX)) {
            rval = getLevel(true);
        } else {
            rval = getString(true);
        }
        return rval;
    }
    
    private String getLevel(boolean lowest) {
        TreeMap<Double, String> sorted = new TreeMap<Double, String>();
        if (this.getValues().isEmpty()) {
            return null;
        }
        for (String val : this.getValues()) {
            String level1 = StringUtils.split(val, '_')[0];
            sorted.put(Double.parseDouble(level1), val);
        }
        Double key = lowest ? sorted.firstKey() : sorted.lastKey();
        return sorted.get(key);
    }

    protected DataTime getDefaultTime(SimpleLayer<?> layer) {
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

    /**
     * Copy contents of from into to
     * 
     * @param to
     * @param from
     */
    public static void copy(Collection<GribDimension> to,
            Collection<GribDimension> from) {
        for (GribDimension d : from) {
            to.add(new GribDimension(d));
        }
    }

}
