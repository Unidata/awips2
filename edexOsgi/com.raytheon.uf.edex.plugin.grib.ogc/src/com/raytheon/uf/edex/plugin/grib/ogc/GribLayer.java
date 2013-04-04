package com.raytheon.uf.edex.plugin.grib.ogc;

import java.util.Date;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.persistence.ElementCollection;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Sort;
import org.hibernate.annotations.SortType;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.ogc.common.db.SimpleDimension;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;
import com.raytheon.uf.edex.plugin.grib.ogc.GribDimension;

@Entity
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@Table(name = "griblayers")
public class GribLayer extends SimpleLayer {

	@XmlElement
	@DynamicSerializeElement
	@Sort(type = SortType.NATURAL)
	@ElementCollection(fetch = FetchType.EAGER)
	protected SortedSet<Date> times;

	@XmlElement
	@DynamicSerializeElement
	@Embedded
	@ElementCollection(fetch = FetchType.EAGER)
    protected Set<GribDimension> dimensions;

	public GribLayer() {
        this(new TreeSet<Date>(), new TreeSet<GribDimension>());
	}

	public GribLayer(TreeSet<Date> times, Set<GribDimension> dimensions) {
		this.times = times;
		this.dimensions = dimensions;
	}

	public void setTimes(TreeSet<Date> times) {
		this.times = times;
	}

    public void setDimensions(Set<GribDimension> dimensions) {
		this.dimensions = dimensions;
	}

	@Override
    public Set<? extends SimpleDimension> getDimensions() {
		return dimensions;
	}

	@Override
	public SortedSet<Date> getTimes() {
		return times;
	}

}
