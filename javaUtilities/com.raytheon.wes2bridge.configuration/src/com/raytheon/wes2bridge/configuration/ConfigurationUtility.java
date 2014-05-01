/**
 * 
 */
package com.raytheon.wes2bridge.configuration;

import org.apache.commons.configuration.ConfigurationException;

import com.raytheon.wes2bridge.common.configuration.Wes2BridgeConfiguration;

/**
 * @author bkowal
 *
 */
public class ConfigurationUtility 
{
	private static final String FIELD_NAME = "-name";
	private static final String FIELD_ARCHIVE = "-archiveRoot";
	private static final String FIELD_DBPORT = "-databasePort";
	private static final String FIELD_HTTPPORT = "-httpPort";
	private static final String FIELD_JMSPORT = "-jmsPort";
	private static final String FIELD_WEBPORT =
		"-webPort";
	private static final String FIELD_CONFPORT =
		"-confidentialPort";
	
	/**
	 * @param args
	 */
	public static void main(String[] args) 
	{
		if (args.length != 2)
		{
			System.out.println("Error: both a configuration file and a field must be specified.");
			System.exit(-1);
		}
		
		Wes2BridgeConfiguration configuration = 
			new Wes2BridgeConfiguration(args[0]);
		try
		{
			configuration.init();
		}
		catch (ConfigurationException e1)
		{
			e1.printStackTrace();
			System.exit(-1);
		}
		
		final String field = args[1];
		if (field.equals(FIELD_NAME))
		{
			System.out.print(configuration.getTestCaseName());
		}
		else if (field.equals(FIELD_ARCHIVE))
		{
			System.out.print(configuration.getDataArchiveRoot());
		}
		else if (field.equals(FIELD_DBPORT))
		{
			System.out.print(configuration.getDatabasePort());
		}
		else if (field.equals(FIELD_HTTPPORT))
		{
			System.out.print(configuration.getEdexHttpPort());
		}
		else if (field.equals(FIELD_JMSPORT))
		{
			System.out.print(configuration.getJmsPort());
		}
		else if (field.equals(FIELD_WEBPORT))
		{
			System.out.println(configuration.getWebPort());
		}
		else if (field.equals(FIELD_CONFPORT))
		{
			System.out.println(configuration.getConfidentialPort());
		}
		System.exit(0);
	}
}