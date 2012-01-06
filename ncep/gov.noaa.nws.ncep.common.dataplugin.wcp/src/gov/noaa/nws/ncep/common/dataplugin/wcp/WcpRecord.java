/*
 * 
 * WcpRecord
 * 
 * This class performs the mapping to the database tables for the Watch Corner Point
 * (WCP) Decoder Plug-In
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer		Description
 * ------------	----------- --------------	-----------------------------------
 * 02Dec2008	37			F. J. Yen		Initial creation
 * 03Apr2009	37			F. J. Yen		Refactored for TO10
 * *
 * This code has been develped by the SIB for use in the AWIPS2 system.
 * </pre>
 * 
 * @author F. J. Yen, SIB
 * @version 1
 
 */

package gov.noaa.nws.ncep.common.dataplugin.wcp;

import java.util.Calendar;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import gov.noaa.nws.ncep.common.dataplugin.wcp.WcpSevrln;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * WcpRecord is the Data Access component for WCP Watch Corner Point data.
 * This contains getters and setters for the main parent table wcp.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12Dec2008         37    F. J. Yen    Initial Coding.
 * 17Apr2009		 37	   F. J. Yen	Refactored for TO10
 * 24Aug2009		 37	   F. J. Yen	Refactored for TO11
 * 17May2010		 37	   F. J. Yen	Refactored to dataplugin for migration to to11dr11
 * </pre>
 * 
 * @author F. J. Yen, SIB
 * @version 1.0
 */
@Entity
@Table(name = "wcp", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class WcpRecord extends PluginDataObject{
	
	private static final long serialVersionUID = 1L;
	
	/** Report type */
	@Column(length=32)
	@XmlElement
	@DynamicSerializeElement
	private String reportType;
	
	//@DataURI(position = 1)
	@Column
    @DynamicSerializeElement
	@XmlElement
	private Calendar issueTime;

	//@DataURI(position = 2)
	@DataURI(position = 1)
	@Column(length = 8)
	@DynamicSerializeElement
	@XmlElement
	private String designatorBBB;
	
	@Column(length = 2500)
	@DynamicSerializeElement
	@XmlElement
	private String bullMessage;

	/** WcpSevrln */
	@XmlElement
    @DynamicSerializeElement
    @OneToMany(cascade = CascadeType.ALL, mappedBy = "parentID", fetch = FetchType.EAGER)
	@OnDelete(action = OnDeleteAction.CASCADE)
	private Set<WcpSevrln> wcpSevrLn = new HashSet<WcpSevrln>();

	/**
     * Default Constructor
     */
    public WcpRecord() {
    }

    /**
     * Constructs a wcp record from a dataURI
     * 
     * @param uri
     *            The dataURI
     */
    public WcpRecord(String uri) {
        super(uri);
    }
  
    @Override
    public IDecoderGettable getDecoderGettable() {
        // TODO Auto-generated method stub
        return null;
    }
    
    public String getReportType() {
    	return reportType;
    }
    public void setReportType(String reportType) {
    	this.reportType = reportType;
    }
    
	public Calendar getIssueTime(){
		return issueTime;
	}
	public void setIssueTime(Calendar issueTime){
		this.issueTime=issueTime;
	}
	
	public String getDesignatorBBB(){
		return designatorBBB;
	}
	public void setDesignatorBBB(String designatorBBB){
		this.designatorBBB=designatorBBB;
	}
	
	public String getBullMessage(){
		return bullMessage;
	}
	public void setBullMessage(String bullMessage){
		this.bullMessage=bullMessage;
	}

	/**
	 * return the set of wcpSevrLn
	 */
	public Set<WcpSevrln> getWcpSevrLn() {
		return wcpSevrLn;
	}

	/**
	 * @param wcpSevrln the wcpSevrln to set
	 */
	public void setWcpSevrLn(Set<WcpSevrln> wcpSevrlin) {
		this.wcpSevrLn = wcpSevrlin;
	}

	/*
	 * 	Add  wcpSevrln to set
	 */
	public void addWcpSevrLn(WcpSevrln psevrln) {
		wcpSevrLn.add(psevrln);
		psevrln.setParentID(this);
	}

    public void setIdentifier(Object dataURI){
		 this.identifier = dataURI;      
	     if(this.getWcpSevrLn() != null && this.getWcpSevrLn().size() > 0)
	     {
	         for (Iterator<WcpSevrln> iter = this.getWcpSevrLn().iterator(); iter.hasNext();) {
	            WcpSevrln ws = iter.next();
	            ws.setParentID(this);
	         }
	     }
	      
    }
   
}
