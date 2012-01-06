/**
 * 
 * WcpSevrln
 * 
 * This class represents the SEVR Line for a WCP record.  This contains the setters
 * and getters for the child table.
 * 
 * <pre>
 *      
 * SOFTWARE HISTORY
 *      
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12Dec2008    37          F. J. Yen   Initial
 * 17Apr2009	37			F. J. Yen	Refactored for TO10
 * 17May2010	37			F. J. Yen	Refactored to dataplugin for migration to to11dr11
 *       
 * </pre>
 * 
 * @author Fee Jing Yen, SIB
 * @version 1
 */

package gov.noaa.nws.ncep.common.dataplugin.wcp;

import java.io.Serializable;
import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;
import java.util.Iterator;

import gov.noaa.nws.ncep.common.dataplugin.wcp.WcpRecord;
import gov.noaa.nws.ncep.common.dataplugin.wcp.WcpLatlons;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@Entity
@Table(name="wcp_sevrln")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
/*
 * 8888888888888888 public class WcpSevrln extends PersistableDataObject
 * implements ISerializableObject { 888
 */
public class WcpSevrln implements Serializable, ISerializableObject {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue
	private Integer recordId = null;

	/** An identifier used to link this to its parent WcpRecord */
	/** The WcpSevrln record this object belongs to */
	@ManyToOne
	@JoinColumn(name="parentID", nullable=false)
	private WcpRecord parentID;

	//@DataURI(position=1)
	@Column(length = 8)
	@XmlElement
	@DynamicSerializeElement
	private String watchNumber;


	//@DataURI(position=2)
	@Column(length = 8)
	@XmlElement
	@DynamicSerializeElement
	private String eventType;

	@Column
	@XmlElement
	@DynamicSerializeElement
	private Calendar startTime;

	@Column
	@XmlElement
	@DynamicSerializeElement
	private Calendar endTime;

	@Column
	@XmlElement
	@DynamicSerializeElement
	private Integer numPnts;

	@Column(length = 272)
	@XmlElement
	@DynamicSerializeElement
	private String sevrLines;

	@DynamicSerializeElement
	@XmlElement
	@OneToMany(cascade = CascadeType.ALL, mappedBy = "parentID", fetch = FetchType.EAGER)
	@OnDelete(action = OnDeleteAction.CASCADE)
	private Set<WcpLatlons> wcpLatLon = new HashSet<WcpLatlons>();

	/**
	 * No-Arg Constructor.
	 */
	public WcpSevrln() {
		this.sevrLines = null;
		this.numPnts = 0;
	}

	/**
	 * @return the serialVersionUID
	 */
	public static long getSerialVersionUID() {
		return serialVersionUID;
	}

	/**
	 * @return the parentID
	 */
	public WcpRecord getParentID() {
		return parentID;
	}

	/**
	 * @param parentID
	 *            the parentID to set
	 */
	public void setParentID(WcpRecord parentID) {
		this.parentID = parentID;

		if (this.getWcpLatLon() != null && this.getWcpLatLon().size() > 0) {
			for (Iterator<WcpLatlons> iter = this.getWcpLatLon().iterator();
			        iter.hasNext();) {
				WcpLatlons cond = iter.next();
				cond.setParentID(this);
			}
		}
	}

	public String getWatchNumber() {
		return watchNumber;
	}

	public void setWatchNumber(String watchNumber) {
		this.watchNumber = watchNumber;
	}

	public Calendar getStartTime() {
		return startTime;
	}

	public void setStartTime(Calendar startTime) {
		this.startTime = startTime;
	}

	public String getEventType() {
		return eventType;
	}

	public void setEventType(String eventType) {
		this.eventType = eventType;
	}

	public Calendar getEndTime() {
		return endTime;
	}

	public void setEndTime(Calendar endTime) {
		this.endTime = endTime;
	}

	public Integer getNumPnts() {
		return numPnts;
	}

	public void setNumPnts(Integer numPnts) {
		this.numPnts = numPnts;
	}

	public String getSevrLines() {
		return sevrLines;
	}

	public void setSevrLines(String sevrLines) {
		this.sevrLines = sevrLines;
	}

	/**
	 * Get the record id.
	 * @return the recordId.  If not set, returns null
	 */
	public Integer getRecordId() {
		return recordId;
	}
	
	/**
	 * Set the record id.
	 * @param recordId
	 * 			The recordId.
	 */
	@SuppressWarnings("unused")
	private void setRecordId(Integer recordId) {
		this.recordId = recordId;
	}
	
	/**
	 * @return the set of wcpLatlon
	 */
	public Set<WcpLatlons> getWcpLatLon() {
		return wcpLatLon;
	}

	/**
	 * @param wcpLatlons
	 *            - the set of wcpLatlons to set
	 */
	public void setWcpLatLon(Set<WcpLatlons> wcpLatlons) {
		this.wcpLatLon = wcpLatlons;
	}

	/*
	 * Add wcpLatlons to set
	 */
	public void addWcpLatLon(WcpLatlons platlon) {
		wcpLatLon.add(platlon);
	}

}
