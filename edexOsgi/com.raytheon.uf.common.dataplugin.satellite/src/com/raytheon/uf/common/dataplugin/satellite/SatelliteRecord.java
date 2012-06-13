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

package com.raytheon.uf.common.dataplugin.satellite;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.ServerSpecificPersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Record implementation for satellite plugin.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 02/14/07     139         bphillip    Initial Creation    
 * 20070914            379  jkorman     Added populateDataStore() and
 *                                      getPersistenceTime() from new IPersistable
 * 20071129            472  jkorman     Added IDecoderGettable interface.
 * 20081106           1515  jkorman     Changed units length from 16 to 26
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@Table(name = "satellite", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SatelliteRecord extends ServerSpecificPersistablePluginDataObject
		implements ISpatialEnabled {

	private static final long serialVersionUID = 1L;

	/**
	 * The source of the data - NESDIS
	 */
	@Column(length = 31)
	@DataURI(position = 1)
	@XmlAttribute
	@DynamicSerializeElement
	private String source;

	/** The creating entity. See table 4.5 of GINI satellite ICD */
	@Column(length = 63)
	@DataURI(position = 2)
	@XmlAttribute
	@DynamicSerializeElement
	private String creatingEntity;

	/** The sector ID. See table 4.6 of the GINI satellite ICD */
	@Column(length = 63)
	@DataURI(position = 3)
	@XmlAttribute
	@DynamicSerializeElement
	private String sectorID;

	/** The physical Element. See table 4.7 of the GINI satellite ICD */
	@Column(length = 63)
	@DataURI(position = 4)
	@XmlAttribute
	@DynamicSerializeElement
	private String physicalElement;

	/**
	 * Number of logical records in the product. See tables 4.9, 4.11, 4.12,
	 * 4.13, 4.14, 4.16 of the GINI satellite ICD
	 */
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private Integer numRecords;

	/**
	 * Size of logical records in bytes for product. See tables 4.9, 4.11, 4.12,
	 * 4.13, 4.14, 4.16 of the GINI satellite ICD
	 */
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private Integer sizeRecords;

	/** The latitude directly beneath the satellite */
	@Column
	@DynamicSerializeElement
	private Float satSubPointLat;

	/** The longitude directly beneath the satellite */
	@Column
	@DynamicSerializeElement
	private Float satSubPointLon;

	/** The upper right hand latitude */
	@Column
	@DynamicSerializeElement
	private Float upperRightLat;

	/** The upper right hand longitude */
	@Column
	@DynamicSerializeElement
	private Float upperRightLon;

	/** Height of the satellite in km */
	@Column
	@DynamicSerializeElement
	private Integer satHeight;

	/** Units of the satellite data * */
	@Column(length = 26)
	@XmlAttribute
	@DynamicSerializeElement
	private String units;

	@ManyToOne
	@PrimaryKeyJoinColumn
	@XmlElement
	@DynamicSerializeElement
	private SatMapCoverage coverage;

	@Override
	public SatMapCoverage getSpatialObject() {
		return coverage;
	}

	public SatMapCoverage getCoverage() {
		return coverage;
	}

	public void setCoverage(SatMapCoverage coverage) {
		this.coverage = coverage;
	}

	public Float getSatSubPointLat() {
		return satSubPointLat;
	}

	public void setSatSubPointLat(Float satSubPointLat) {
		this.satSubPointLat = satSubPointLat;
	}

	public Float getSatSubPointLon() {
		return satSubPointLon;
	}

	public void setSatSubPointLon(Float satSubPointLon) {
		this.satSubPointLon = satSubPointLon;
	}

	public Float getUpperRightLat() {
		return upperRightLat;
	}

	public void setUpperRightLat(Float upperRightLat) {
		this.upperRightLat = upperRightLat;
	}

	public Float getUpperRightLon() {
		return upperRightLon;
	}

	public void setUpperRightLon(Float upperRightLon) {
		this.upperRightLon = upperRightLon;
	}

	/**
	 * No-arg constructor.
	 */
	public SatelliteRecord() {

	}

	/**
	 * Constructs a satellite record from a dataURI
	 * 
	 * @param uri
	 *            The dataURI
	 * @param tableDef
	 *            The table definition associated with this class
	 */
	public SatelliteRecord(String uri) {
		super(uri);
	}

	public Integer getNumRecords() {
		return numRecords;
	}

	public void setNumRecords(Integer numRecords) {
		this.numRecords = numRecords;
	}

	public Integer getSizeRecords() {
		return sizeRecords;
	}

	public void setSizeRecords(Integer sizeRecords) {
		this.sizeRecords = sizeRecords;
	}

	public Integer getSatHeight() {
		return satHeight;
	}

	public void setSatHeight(Integer satHeight) {
		this.satHeight = satHeight;
	}

	/**
	 * @return the units
	 */
	public String getUnits() {
		return units;
	}

	/**
	 * @param units
	 *            the units to set
	 */
	public void setUnits(String units) {
		this.units = units;
	}

	/**
	 * Get the IDecoderGettable reference for this record.
	 * 
	 * @return The IDecoderGettable reference for this record. Null for this
	 *         class.
	 */
	@Override
	public IDecoderGettable getDecoderGettable() {
		return null;
	}

	public String getSource() {
		return source;
	}

	public void setSource(String source) {
		this.source = source;
	}

	public String getCreatingEntity() {
		return creatingEntity;
	}

	public void setCreatingEntity(String creatingEntity) {
		this.creatingEntity = creatingEntity;
	}

	public String getSectorID() {
		return sectorID;
	}

	public void setSectorID(String sectorID) {
		this.sectorID = sectorID;
	}

	public String getPhysicalElement() {
		return physicalElement;
	}

	public void setPhysicalElement(String physicalElement) {
		this.physicalElement = physicalElement;
	}

}
