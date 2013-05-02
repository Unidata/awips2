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
package com.raytheon.uf.common.dataplugin.cwa;

import java.util.Calendar;

import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 1, 2010            jsanchez     Initial creation
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013       1857 bgonzale    Added SequenceGenerator annotation.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "cwaseq")
@Table(name = "cwa", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "cwa",
		indexes = {
				@Index(name = "cwa_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class CWARecord extends PersistablePluginDataObject implements
		IPointData, IPersistable {

	private static final long serialVersionUID = 1L;

	@Embedded
	@DynamicSerializeElement
	private PointDataView pointDataView;

	// Text of the WMO header
	@Transient
	@XmlElement
	@DynamicSerializeElement
	private String wmoHeader = "";

	@Transient
	@XmlElement
	@DynamicSerializeElement
	private CWADimension dimension;

	@DataURI(position = 1, embedded = true)
	@XmlElement
	@DynamicSerializeElement
	private String eventId;

	@Transient
	@XmlElement
	@DynamicSerializeElement
	private Coordinate[] coordinates;

	@Transient
	@XmlElement
	@DynamicSerializeElement
	private String text;

	public CWADimension getDimension() {
		return dimension;
	}

	public void setDimension(CWADimension dimension) {
		this.dimension = dimension;
	}

	public String getWmoHeader() {
		return wmoHeader;
	}

	public void setWmoHeader(String wmoHeader) {
		this.wmoHeader = wmoHeader;
	}

	public Coordinate[] getCoordinates() {
		return coordinates;
	}

	public void setCoordinates(Coordinate[] coordinates) {
		this.coordinates = coordinates;
	}

	public String getEventId() {
		return eventId;
	}

	public void setEventId(String eventId) {
		this.eventId = eventId;
	}

	/**
	 * Set the data uri for this observation.
	 * 
	 * @param dataURI
	 */
	@Override
	public void setDataURI(String dataURI) {
		super.setDataURI(dataURI);
		identifier = dataURI;
	}

	@Override
	public IDecoderGettable getDecoderGettable() {
		return null;
	}

	public String getText() {
		return text;
	}

	public void setText(String text) {
		this.text = text;
	}

	@Override
	public PointDataView getPointDataView() {
		return pointDataView;
	}

	@Override
	public void setPointDataView(PointDataView pointDataView) {
		this.pointDataView = pointDataView;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		Calendar c = getDataTime().getRefTimeAsCalendar();
		if (c != null) {
			sb.append(String.format("CWA:%1$tY%1$tm%1$td%1$tH%1$tM",
					getDataTime().getRefTimeAsCalendar()));
		} else {
			sb.append("CWA:YYYYMMDDHHmm");
		}
		return sb.toString();
	}
}
