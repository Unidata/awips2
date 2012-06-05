/**
 * 
 */
package com.raytheon.wes2bridge.manager;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.FileWriter;
import java.io.BufferedWriter;
import java.io.IOException;

import org.apache.commons.configuration.ConfigurationException;

import com.raytheon.wes2bridge.common.configuration.Wes2BridgeConfiguration;

/**
 * @author bkowal
 *
 * This java-based utility is used to update a wes2bridge environment.
 * This utility is invoked by the wes2bridge management script (bash)
 * after the management script spawns a new edex, database, qpid.
 * Only "base" files are updated based on the configuration file. The
 * new wes2bridge environment will still need to be localized.
 */
public class Wes2BridgeManager 
{
	private static final String AWIPSII =
		"/awips2";
	private static final String AWIPSII_WES2BRIDGE_SCRIPTS =
		AWIPSII + "/edex-environment/scripts";
	private static final String WES2BRIDGE_DIRECTORY =
		"/usr/local/edex-environment";
	private Wes2BridgeConfiguration configuration = null;
	private String wes2BridgeScripts = null;
	
	/**
	 * 
	 */
	public Wes2BridgeManager() 
	{
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) 
	{		
		if (args.length != 1)
		{
			System.out.println("ERROR: The configuration file has not been specified.");
			System.exit(-1);
		}
		
		Wes2BridgeManager manager = new Wes2BridgeManager();
		try
		{
			manager.init(args[0]);
		}
		catch (Exception e1)
		{
			e1.printStackTrace();
			System.exit(-1);
		}
		
		try
		{
			manager.reconfigureEdex();
		}
		catch (Exception e1)
		{
			e1.printStackTrace();
			System.exit(-1);
		}
		
		try
		{
			manager.reconfigurePostgreSQL();
		}
		catch (Exception e1)
		{
			e1.printStackTrace();
			System.exit(-1);
		}
		
		try
		{
			manager.reconfigureQPID();
		}
		catch (Exception e1)
		{
			e1.printStackTrace();
			System.exit(-1);
		}
		
		System.exit(0);
	}
	
	public void init(String arg1)
	throws ConfigurationException
	{
		configuration = new Wes2BridgeConfiguration(arg1);
		configuration.init();
		this.wes2BridgeScripts = WES2BRIDGE_DIRECTORY + "/" +
			configuration.getTestCaseName() + "/" +
			"edex-environment";
	}
	
	/*
	 * Updates setup.env and wrapper.conf.
	 */
	public void reconfigureEdex()
	throws FileNotFoundException, IOException
	{
		final String srcEdexDirectory =
			AWIPSII + "/" +
			"edex";
		final String edexDirectory =
			WES2BRIDGE_DIRECTORY + "/" + 
			this.configuration.getTestCaseName() + "/" +
			"edex";
		
		this.updateEdexSetup(srcEdexDirectory, edexDirectory);
		this.updateEdexWrapper(srcEdexDirectory, edexDirectory);
		this.updateEdexCamel(edexDirectory);
	}
	
	private void updateEdexSetup(String srcEdexDirectory, 
	String edexDirectory)
	throws FileNotFoundException, IOException
	{
		String srcsetup_env = srcEdexDirectory + "/bin/setup.env";
		String setup_env = edexDirectory + "/bin/setup.env";
		
		File srcFile = new File(srcsetup_env);
		File destFile = new File(setup_env);
		BufferedReader br = 
			new BufferedReader(new FileReader(srcFile));
		BufferedWriter bw = 
			new BufferedWriter(new FileWriter(destFile));
		
		final String line1 = "export DATA_ARCHIVE_ROOT=";
		final String line2 = "export DB_PORT=";
		final String line3 = "export BROKER_ADDR=";
		final String line4 = "export HTTP_PORT=";
		final String line5 = "export JMS_SERVER=";
		final String line6 = "export SHARE_DIR=";
		
		String line = "";
		while ((line = br.readLine()) != null)
		{
			if (line.startsWith(line1))
			{
				line = line1 + this.configuration.getDataArchiveRoot();
			}
			else if (line.startsWith(line2))
			{
				line = line2 + this.configuration.getDatabasePort();
			}
			else if (line.startsWith(line3))
			{
				line = line3 + "localhost:" +
					this.configuration.getJmsPort();
			}
			else if (line.startsWith(line4))
			{
				line = line4 + this.configuration.getEdexHttpPort();
			}
			else if (line.startsWith(line5))
			{
				line = line5 + "tcp://localhost:" +
					this.configuration.getJmsPort();
			}
			else if (line.startsWith(line6))
			{
				line = line6 + edexDirectory + "/data/share";
			}
			
			bw.write(line + "\n");
		}
		br.close();
		bw.close();
	}
	
	/* Disable JMX. */
	private void updateEdexWrapper(String srcEdexDirectory,
	String edexDirectory)
	throws FileNotFoundException, IOException
	{
		String srcwrapper_conf = srcEdexDirectory + 
			"/bin/linux-x86-32/wrapper.conf";
		String wrapper_conf = edexDirectory +
			"/bin/linux-x86-32/wrapper.conf";
		
		File srcFile = new File(srcwrapper_conf);
		File destFile = new File(wrapper_conf);
		BufferedReader br = 
			new BufferedReader(new FileReader(srcFile));
		BufferedWriter bw = 
			new BufferedWriter(new FileWriter(destFile));
		
		/*
		 * We want to replace at least one of the jmx jvm arguments
		 * with the wes2bridge.instance argument.
		 */
		boolean wes2BridgeInstanceAdded = false;
		
		/*
		 * Disable JMX Remote and add a new wes2bridge.instance
		 * JVM argument so that it will be possible to determine
		 * which edex instance belongs to which test case.
		 */
		/*
		 * This may apply to multiple jvm arguments including:
		 * 	1) -Dcom.sun.management.jmxremote.port
		 *  2) -Dcom.sun.management.jmxremote.authenticate
		 *  3) -Dcom.sun.management.jmxremote.ssl
		 */
		final String line1 = "-Dcom.sun.management.jmxremote";
		/* Set the web port; used by uengine spring. */
		final String line2 = "-Dweb.port";
		/* Set the confidential port; used by uengine spring. */
		final String line3 = "-Dconfidential.port";
		
		String line = "";
		while ((line = br.readLine()) != null)
		{
			if (line.contains(line1))
			{
				line = this.getJVMArgumentName(line);
				if (wes2BridgeInstanceAdded == false)
				{
					line += "-Dwes2bridge.instance=" + 
						this.configuration.getTestCaseName();
					wes2BridgeInstanceAdded = true;
				}
			}
			else if (line.contains(line2))
			{
				line = this.getJVMArgumentName(line);
				line += line2 + "=" +
					this.configuration.getWebPort();
			}
			else if (line.contains(line3))
			{
				line = this.getJVMArgumentName(line);
				line += line3 + "=" +
					this.configuration.getConfidentialPort();
			}
			
			bw.write(line + "\n");
		}
		br.close();
		bw.close();
	}
	
	private String getJVMArgumentName(String jvmArgument)
	{
		if (jvmArgument == null)
		{
			System.out.println("ERROR: Invalid wrapper.conf file.");
			System.exit(-1);
		}
		
		String[] splitJVMArg = jvmArgument.split("=");
		if (splitJVMArg.length <= 0)
		{
			System.out.println("ERROR: Invalid wrapper.conf file.");
			System.exit(-1);			
		}
		
		return splitJVMArg[0] + "=";
	}
	
	private void updateEdexCamel(String edexDirectory)
	throws FileNotFoundException, IOException
	{
		final String srcedex_camel = AWIPSII_WES2BRIDGE_SCRIPTS + "/" +
			"edex_camel";
		final String edex_camel = this.wes2BridgeScripts + "/edex_camel";
		
		File srcFile = new File(srcedex_camel);
		File destFile = new File(edex_camel);
		BufferedReader br = 
			new BufferedReader(new FileReader(srcFile));
		BufferedWriter bw = 
			new BufferedWriter(new FileWriter(destFile));
		
		final String line1 = "EDEX_INSTALL=";
		final String line2 = "export DATA_ARCHIVE_ROOT=";
		final String line3 = 
			"CAMELPROCESS=`ps -ef | grep \"edex.dev.mode\"|grep -c \"edex.run.mode=${1} \" `";
		
		String line = "";
		while ((line = br.readLine()) != null)
		{
			if (line.trim().startsWith(line1))
			{
				line = line1 + edexDirectory;
			}
			else if (line.trim().startsWith(line2))
			{
				line = line2 +
					this.configuration.getDataArchiveRoot();
			}
			else if (line.trim().startsWith(line3))
			{
				line = "CAMELPROCESS=`ps -ef | " +
					"grep \"wes2bridge.instance=" + 
					this.configuration.getTestCaseName() + "\" | " + 
					"grep -c \"edex.run.mode=${1} \" `";
			}
			
			bw.write(line + "\n");
		}
		br.close();
		bw.close();
	}
	
	public void reconfigurePostgreSQL()
	throws FileNotFoundException, IOException
	{
		final String postgresqlRootDirectory =
			WES2BRIDGE_DIRECTORY + "/" + 
			this.configuration.getTestCaseName();
		
		this.updateEdexPostgres(postgresqlRootDirectory);
	}
	
	private void updateEdexPostgres(String postgresqlRootDirectory)
	throws FileNotFoundException, IOException
	{
		final String srcedex_postgres = AWIPSII_WES2BRIDGE_SCRIPTS + "/" +
			"edex_postgres";
		final String edex_postgres = this.wes2BridgeScripts + 
			"/edex_postgres";
		
		File srcFile = new File(srcedex_postgres);
		File destFile = new File(edex_postgres);
		BufferedReader br = 
			new BufferedReader(new FileReader(srcFile));
		BufferedWriter bw = 
			new BufferedWriter(new FileWriter(destFile));
		
		final String line1 = "POSTGRESQL_INSTALL_ROOT=";
		final String line2 = "PGPORT=";
		
		String line = "";
		while ((line = br.readLine()) != null)
		{
			if (line.startsWith(line1))
			{
				line = line1 + postgresqlRootDirectory;
			}
			else if (line.startsWith(line2))
			{
				line = line2 +
					this.configuration.getDatabasePort();
			}
			
			bw.write(line + "\n");
		}
		br.close();
		bw.close();
	}
	
	public void reconfigureQPID()
	throws FileNotFoundException, IOException
	{
		final String srcQpidDirectory =
			AWIPSII + "/" +
			"qpid";
		final String qpidDirectory =
			WES2BRIDGE_DIRECTORY + "/" + 
			this.configuration.getTestCaseName() + "/" +
			"qpid";
		
		this.updateQpidConf(srcQpidDirectory, qpidDirectory);
		this.updateQPIDD(qpidDirectory);
		this.updateQueueCreatorSH(qpidDirectory);
	}
	
	/* Updates qpidd.conf */
	private void updateQpidConf(String srcQpidDirectory,
	String qpidDirectory)
	throws FileNotFoundException, IOException
	{
		String srcqpidd_conf = srcQpidDirectory +
			"/etc/qpidd.conf";
		String qpidd_conf = qpidDirectory + "/etc/qpidd.conf";
	
		File srcFile = new File(srcqpidd_conf);
		File destFile = new File(qpidd_conf);
		BufferedReader br = 
			new BufferedReader(new FileReader(srcFile));
		BufferedWriter bw = 
			new BufferedWriter(new FileWriter(destFile));
	
		final String line1 = "data-dir=";
		final String line2 = "store-dir=";
		final String line3 = "pid-dir=";
		/*
		 * add the port to qpidd.conf
		 */
		final String line4 = "auth=no";
	
		String line = "";
		while ((line = br.readLine()) != null)
		{			
			if (line.startsWith(line1))
			{
				line = line1 + qpidDirectory + "/data";
			}
			else if (line.startsWith(line2))
			{
				line = line2 + qpidDirectory + "/messageStore";
			}
			else if (line.startsWith(line3))
			{
				line = line3 + qpidDirectory + "/var/lock";
			}
			else if (line.startsWith(line4))
			{
				line = line4 + "\nport=" +
					this.configuration.getJmsPort();
			}
		
			bw.write(line + "\n");
		}
		br.close();
		bw.close();
	}
	
	private void updateQPIDD(String qpidDirectory)
	throws FileNotFoundException, IOException
	{
		final String srcqpidd = AWIPSII_WES2BRIDGE_SCRIPTS + "/" +
			"qpidd";
		final String qpidd = this.wes2BridgeScripts + "/qpidd";
		
		File srcFile = new File(srcqpidd);
		File destFile = new File(qpidd);
		BufferedReader br = 
			new BufferedReader(new FileReader(srcFile));
		BufferedWriter bw = 
			new BufferedWriter(new FileWriter(destFile));
		
		final String line1 = "QPID_HOME=";
		
		String line = "";
		while ((line = br.readLine()) != null)
		{
			if (line.startsWith(line1))
			{
				line = line1 + qpidDirectory;
			}
			
			bw.write(line + "\n");
		}
		br.close();
		bw.close();
	}
	
	private void updateQueueCreatorSH(String qpidDirectory)
	throws FileNotFoundException, IOException
	{
		final String srcqueue = AWIPSII_WES2BRIDGE_SCRIPTS + "/" +
		"queueCreator.sh";
		final String queue = qpidDirectory + "/sbin/queueCreator.sh";	
		
		File srcFile = new File(srcqueue);
		File destFile = new File(queue);
		BufferedReader br = 
			new BufferedReader(new FileReader(srcFile));
		BufferedWriter bw = 
			new BufferedWriter(new FileWriter(destFile));
		
		final String line1 = "port=";
		
		String line = "";
		while ((line = br.readLine()) != null)
		{
			if (line.startsWith(line1))
			{
				line = line1 + this.configuration.getJmsPort();
			}
			
			bw.write(line + "\n");
		}
		br.close();
		bw.close();
	}
}