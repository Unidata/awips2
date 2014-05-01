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
 * 09/2011      		    Chin Chen   changed to improve purge performance and
 * 										removed xml serialization as well
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
import gov.noaa.nws.ncep.common.dataplugin.wcp.WcpLatlons;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@Entity
@Table(name="wcp_sevrln")
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

	

	//@DataURI(position=1)
	@Column(length = 8)
	@DynamicSerializeElement
	private String watchNumber;


	//@DataURI(position=2)
	@Column(length = 8)
	@DynamicSerializeElement
	private String eventType;

	@Column
	@DynamicSerializeElement
	private Calendar startTime;

	@Column
	@DynamicSerializeElement
	private Calendar endTime;

	@Column
	@DynamicSerializeElement
	private Integer numPnts;

	@Column(length = 272)
	@DynamicSerializeElement
	private String sevrLines;

	@DynamicSerializeElement
	@OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
	@JoinColumn(name = "parentID", nullable = false)
    @Index(name = "wcpLatLon_parentid_idex")
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
