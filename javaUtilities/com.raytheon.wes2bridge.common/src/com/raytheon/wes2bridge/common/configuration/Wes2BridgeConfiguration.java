/**
 * 
 */
package com.raytheon.wes2bridge.common.configuration;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.XMLConfiguration;

/**
 * @author bkowal
 *
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
 * 		<confidentialPort></confidentialPort>
 * </Wes2BridgeCase>
 */
public class Wes2BridgeConfiguration 
{
	private String configurationFile = null;
	
	private String testCaseName = null;
	private String dataArchiveRoot = null;
	private int databasePort = -1;
	private int edexHttpPort = -1;
	private int jmsPort = -1;
	private int webPort = -1;
	private int confidentialPort = -1;
	
	/**
	 * 
	 */
	public Wes2BridgeConfiguration(String configurationFile) 
	{
		this.configurationFile = configurationFile;
		
		this.testCaseName = null;
		this.dataArchiveRoot = null;
		this.databasePort = -1;
		this.edexHttpPort = -1;
		this.jmsPort = -1;
		this.webPort = -1;
		this.confidentialPort = -1;
	}
	
	public void init()
	throws ConfigurationException
	{
		XMLConfiguration xmlConfiguration =
			new XMLConfiguration(this.configurationFile);
		
		this.testCaseName = 
			xmlConfiguration.getString("name");
		/*
		 * Currently spaces are not allowed in the
		 * name of a test case.
		 */
		if (this.testCaseName.contains(" "))
		{
			ConfigurationException exception =
				new ConfigurationException(
				"The Edex Environment name cannot contain spaces.");
			exception.fillInStackTrace();
			
			throw exception;
		}
		this.dataArchiveRoot =
			xmlConfiguration.getString("dataArchiveRoot");
		this.databasePort =
			xmlConfiguration.getInt("databasePort");
		this.edexHttpPort =
			xmlConfiguration.getInt("edexHttpPort");
		this.jmsPort =
			xmlConfiguration.getInt("jmsPort");
		this.webPort =
			xmlConfiguration.getInt("webPort");
		this.confidentialPort =
			xmlConfiguration.getInt("confidentialPort");
	}

	public String getTestCaseName() 
	{
		return testCaseName;
	}

	public void setTestCaseName(String testCaseName) 
	{
		this.testCaseName = testCaseName;
	}

	public String getDataArchiveRoot() 
	{
		return dataArchiveRoot;
	}

	public void setDataArchiveRoot(String dataArchiveRoot) 
	{
		this.dataArchiveRoot = dataArchiveRoot;
	}

	public int getDatabasePort() 
	{
		return databasePort;
	}

	public void setDatabasePort(int databasePort) 
	{
		this.databasePort = databasePort;
	}

	public int getEdexHttpPort() 
	{
		return edexHttpPort;
	}

	public void setEdexHttpPort(int edexHttpPort) 
	{
		this.edexHttpPort = edexHttpPort;
	}

	public int getJmsPort() 
	{
		return jmsPort;
	}

	public void setJmsPort(int jmsPort) 
	{
		this.jmsPort = jmsPort;
	}

	public int getWebPort() 
	{
		return webPort;
	}

	public void setWebPort(int webPort) 
	{
		this.webPort = webPort;
	}

	public int getConfidentialPort() 
	{
		return confidentialPort;
	}

	public void setConfidentialPort(int confidentialPort) 
	{
		this.confidentialPort = confidentialPort;
	}
}