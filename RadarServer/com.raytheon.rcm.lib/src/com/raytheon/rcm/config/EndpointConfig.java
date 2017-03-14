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

public class EndpointConfig implements Cloneable {
	private static final String DEFAULT_TOPIC = "radarserver.dropbox";

	private String archiveRoot;
	private String connectionURL;
	private Boolean prefixPathWithRadar;
	private String topic = DEFAULT_TOPIC;
	private Integer backlogLimitPerRadar;

	/**
	 * Provides complete control over RadarServer ActiveMQ broker. Takes
	 * precedence over radarServerBrokerHost
	 */
	private String radarServerBrokerURL;
	/** Simplified option for setting up ActiveMQ broker. */
	private String radarServerBrokerHost;

	public String getArchiveRoot() {
		return archiveRoot;
	}

	public void setArchiveRoot(String archiveRoot) {
		this.archiveRoot = archiveRoot;
	}

	public String getConnectionURL() {
		return connectionURL;
	}

	public void setConnectionURL(String connectionURL) {
		this.connectionURL = connectionURL;
	}

	public Boolean getPrefixPathWithRadar() {
		return prefixPathWithRadar;
	}

	public void setPrefixPathWithRadar(Boolean prefixPathWithRadar) {
		this.prefixPathWithRadar = prefixPathWithRadar;
	}

	public String getTopic() {
		return topic;
	}

	public void setTopic(String topic) {
		this.topic = topic;
	}

	public Integer getBacklogLimitPerRadar() {
		return backlogLimitPerRadar;
	}

	public void setBacklogLimitPerRadar(Integer backlogLimit) {
		this.backlogLimitPerRadar = backlogLimit;
	}

	public String getRadarServerBrokerURL() {
		return radarServerBrokerURL;
	}

	public void setRadarServerBrokerURL(String radarServerBrokerURL) {
		this.radarServerBrokerURL = radarServerBrokerURL;
	}

	public String getRadarServerBrokerHost() {
		return radarServerBrokerHost;
	}

	public void setRadarServerBrokerHost(String radarServerBrokerHost) {
		this.radarServerBrokerHost = radarServerBrokerHost;
	}

}
