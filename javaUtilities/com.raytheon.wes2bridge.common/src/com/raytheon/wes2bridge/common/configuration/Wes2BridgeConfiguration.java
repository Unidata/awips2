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
package com.raytheon.wes2bridge.common.configuration;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.XMLConfiguration;

/**
 * Reads the edex-environment XML configuration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012 1490       bkowal      Added httpdPypiesPort and
 *                                     pypiesLoggingPort as configurable
 *                                     parameters.
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

/*
 * Example: 
 * <Wes2BridgeCase> 
 * 		<name>String</name>
 * 		<dataArchiveRoot>String</dataArchiveRoot> 
 * 		<databasePort>int</databasePort>
 * 		<edexHttpPort>int</edexHttpPort> 
 * 		<jmsPort>int</jmsPort>
 * 		<webPort>int</webPort> 
 * 		<confidentialPort>int</confidentialPort>
 * 		<httpdPypiesPort>int</httpdPypiesPort>
 * 		<pypiesLoggingPort>int</pypiesLoggingPort> 
 * </Wes2BridgeCase>
 */
public class Wes2BridgeConfiguration {
	private static class XML_SCHEMA {
		public static final String XML_TEST_CASE_NAME = "name";
		public static final String XML_DATA_ARCHIVE_ROOT = "dataArchiveRoot";
		public static final String XML_DATABASE_PORT = "databasePort";
		public static final String XML_EDEX_HTTP_PORT = "edexHttpPort";
		public static final String XML_JMS_PORT = "jmsPort";
		public static final String XML_WEB_PORT = "webPort";
		public static final String XML_CONFIDENTIAL_PORT = "confidentialPort";
		public static final String XML_HTTPD_PYPIES_PORT = "httpdPypiesPort";
		public static final String XML_PYPIES_LOGGING_PORT = "pypiesLoggingPort";
	}

	private static final String __SPACE = " ";

	private String configurationFile = null;

	private String testCaseName = null;
	private String dataArchiveRoot = null;
	private int databasePort = -1;
	private int edexHttpPort = -1;
	private int jmsPort = -1;
	private int webPort = -1;
	private int confidentialPort = -1;
	private int httpdPypiesPort = -1;
	private int pypiesLoggingPort = -1;

	/**
	 * 
	 */
	public Wes2BridgeConfiguration(String configurationFile) {
		this.configurationFile = configurationFile;
	}

	public void init() throws ConfigurationException {
		XMLConfiguration xmlConfiguration = new XMLConfiguration(
				this.configurationFile);

		this.testCaseName = xmlConfiguration
				.getString(XML_SCHEMA.XML_TEST_CASE_NAME);
		/*
		 * Currently spaces are not allowed in the name of a test case.
		 */
		if (this.testCaseName.contains(__SPACE)) {
			ConfigurationException exception = new ConfigurationException(
					"The Edex Environment name cannot contain spaces.");
			exception.fillInStackTrace();

			throw exception;
		}
		this.dataArchiveRoot = xmlConfiguration
				.getString(XML_SCHEMA.XML_DATA_ARCHIVE_ROOT);
		this.databasePort = xmlConfiguration
				.getInt(XML_SCHEMA.XML_DATABASE_PORT);
		this.edexHttpPort = xmlConfiguration
				.getInt(XML_SCHEMA.XML_EDEX_HTTP_PORT);
		this.jmsPort = xmlConfiguration.getInt(XML_SCHEMA.XML_JMS_PORT);
		this.webPort = xmlConfiguration.getInt(XML_SCHEMA.XML_WEB_PORT);
		this.confidentialPort = xmlConfiguration
				.getInt(XML_SCHEMA.XML_CONFIDENTIAL_PORT);
		this.httpdPypiesPort = xmlConfiguration
				.getInt(XML_SCHEMA.XML_HTTPD_PYPIES_PORT);
		this.pypiesLoggingPort = xmlConfiguration
				.getInt(XML_SCHEMA.XML_PYPIES_LOGGING_PORT);
	}

	public String getTestCaseName() {
		return testCaseName;
	}

	public void setTestCaseName(String testCaseName) {
		this.testCaseName = testCaseName;
	}

	public String getDataArchiveRoot() {
		return dataArchiveRoot;
	}

	public void setDataArchiveRoot(String dataArchiveRoot) {
		this.dataArchiveRoot = dataArchiveRoot;
	}

	public int getDatabasePort() {
		return databasePort;
	}

	public void setDatabasePort(int databasePort) {
		this.databasePort = databasePort;
	}

	public int getEdexHttpPort() {
		return edexHttpPort;
	}

	public void setEdexHttpPort(int edexHttpPort) {
		this.edexHttpPort = edexHttpPort;
	}

	public int getJmsPort() {
		return jmsPort;
	}

	public void setJmsPort(int jmsPort) {
		this.jmsPort = jmsPort;
	}

	public int getWebPort() {
		return webPort;
	}

	public void setWebPort(int webPort) {
		this.webPort = webPort;
	}

	public int getConfidentialPort() {
		return confidentialPort;
	}

	public void setConfidentialPort(int confidentialPort) {
		this.confidentialPort = confidentialPort;
	}

	public int getHttpdPypiesPort() {
		return httpdPypiesPort;
	}

	public void setHttpdPypiesPort(int httpdPypiesPort) {
		this.httpdPypiesPort = httpdPypiesPort;
	}

	public int getPypiesLoggingPort() {
		return pypiesLoggingPort;
	}

	public void setPypiesLoggingPort(int pypiesLoggingPort) {
		this.pypiesLoggingPort = pypiesLoggingPort;
	}
}