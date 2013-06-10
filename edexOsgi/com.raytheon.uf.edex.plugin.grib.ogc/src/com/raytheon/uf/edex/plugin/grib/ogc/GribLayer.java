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

import java.util.Date;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;
/**
 * 
 * GribLayer
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/01/2013   1746       dhladky      Initial creation
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */


@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GribLayer extends SimpleLayer<GribDimension> {

	@XmlElement
	@DynamicSerializeElement
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
    public Set<GribDimension> getDimensions() {
		return dimensions;
	}

	@Override
	public SortedSet<Date> getTimes() {
		return times;
	}

}
