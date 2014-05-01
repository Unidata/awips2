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
package com.raytheon.rcm.config;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;

/** 
 * Describes how to connect to an RPG.
 *  
 * <p>
 * Currently only supports TCP connections.
 * <p>
 * The linkAddress property is a String of the form 
 * <em>host</em>:<em>port</em>.
 *  */
@XmlType(name="link",propOrder={})
@XmlAccessorType(XmlAccessType.PROPERTY)
public class LinkResource implements Cloneable {
	
	protected boolean dedicated;
	
	/* TODO: maxRpsListSize as an optional value?  The max size seems to be
	 * determined by the PUP ID, but AWIPS 1 makes it part of the link resource
	 * record.
	 */
	protected LinkType linkType;
	protected String comment;
	
	protected String linkAddress;
	
	protected int linkIndex;
	protected String tcmPassword;
	
	// For dedicated links
	protected int maxRpsListSize = -1;
	
	// For dial links
	protected String userPassword;
	protected String portPassword;
	
	// TODO: overrideDisconnect ... or does it go in RadarConfig?

	public LinkType getLinkType() {
		return linkType;
	}
	public void setLinkType(LinkType linkType) {
		this.linkType = linkType;
	}
	public String getComment() {
		return comment;
	}
	public void setComment(String comment) {
		this.comment = comment;
	}
	public int getLinkIndex() {
		return linkIndex;
	}
	public void setLinkIndex(int linkIndex) {
		this.linkIndex = linkIndex;
	}
	public String getTcmPassword() {
		return tcmPassword;
	}
	public void setTcmPassword(String tcmPassword) {
		this.tcmPassword = tcmPassword;
	}
	public String getLinkAddress() {
		return linkAddress;
	}
	public void setLinkAddress(String linkAddress) {
		this.linkAddress = linkAddress;
	}
	public int getMaxRpsListSize() {
		return maxRpsListSize;
	}
	public void setMaxRpsListSize(int maxRpsListSize) {
		this.maxRpsListSize = maxRpsListSize;
	}
	public String getUserPassword() {
		return userPassword;
	}
	public void setUserPassword(String userPassword) {
		this.userPassword = userPassword;
	}
	public String getPortPassword() {
		return portPassword;
	}
	public void setPortPassword(String portPassword) {
		this.portPassword = portPassword;
	}
	public boolean isDedicated() {
		return dedicated;
	}
	public void setDedicated(boolean isDedicated) {
		this.dedicated = isDedicated;
	}
	
	public LinkResource clone() {
		try {
			return (LinkResource) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new UnsupportedOperationException(e);
		}
	}
}
