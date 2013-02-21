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
package com.raytheon.wes2bridge.manager;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.FileWriter;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.lang.StringUtils;

import com.raytheon.wes2bridge.common.configuration.Wes2BridgeConfiguration;

/**
 * This java-based utility is used to update a wes2bridge environment. This
 * utility is invoked by the wes2bridge management script (bash) after the
 * management script spawns a new edex, database, qpid, pypies. Only "base"
 * files are updated based on the configuration file. The new wes2bridge
 * environment will still need to be localized.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012 1490       bkowal      Pypies is now added to each
 *                                     edex-environment instance
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */
public class Wes2BridgeManager {
	private static final String AWIPSII = "/awips2";
	private static final String AWIPSII_WES2BRIDGE_SCRIPTS = AWIPSII
			+ "/edex-environment/scripts";
	private static final String WES2BRIDGE_DIRECTORY = "/usr/local/edex-environment";
	private static final int GROUP_INDEX_ONE = 1;
	private static final int GROUP_INDEX_TWO = 2;
	private static final int EXIT_FAILURE = -1;
	private static final int EXIT_SUCCESS = 0;
	private static final String DEFAULT_HDF5_DIRECTORY = "/edex/data/hdf5";
	private Wes2BridgeConfiguration configuration = null;
	private String wes2BridgeScripts = null;

	/**
	 * 
	 */
	public Wes2BridgeManager() {
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		if (args.length != 1) {
			System.out
					.println("ERROR: The configuration file has not been specified.");
			System.exit(EXIT_FAILURE);
		}

		Wes2BridgeManager manager = new Wes2BridgeManager();
		try {
			manager.init(args[0]);
		} catch (Exception e1) {
			e1.printStackTrace();
			System.exit(EXIT_FAILURE);
		}

		try {
			manager.reconfigureEdex();
		} catch (Exception e1) {
			e1.printStackTrace();
			System.exit(EXIT_FAILURE);
		}

		try {
			manager.reconfigurePostgreSQL();
		} catch (Exception e1) {
			e1.printStackTrace();
			System.exit(EXIT_FAILURE);
		}

		try {
			manager.reconfigureQPID();
		} catch (Exception e1) {
			e1.printStackTrace();
			System.exit(EXIT_FAILURE);
		}

		try {
			manager.reconfigurePypies();
		} catch (Exception e1) {
			e1.printStackTrace();
			System.exit(EXIT_FAILURE);
		}

		System.exit(EXIT_SUCCESS);
	}

	public void init(String arg1) throws ConfigurationException {
		configuration = new Wes2BridgeConfiguration(arg1);
		configuration.init();
		this.wes2BridgeScripts = WES2BRIDGE_DIRECTORY + "/"
				+ configuration.getTestCaseName() + "/" + "edex-environment";
	}

	/*
	 * Updates setup.env and wrapper.conf.
	 */
	public void reconfigureEdex() throws FileNotFoundException, IOException {
		final String srcEdexDirectory = AWIPSII + "/" + "edex";
		final String edexDirectory = WES2BRIDGE_DIRECTORY + "/"
				+ this.configuration.getTestCaseName() + "/" + "edex";

		this.updateEdexSetup(srcEdexDirectory, edexDirectory);
		this.updateEdexWrapper(srcEdexDirectory, edexDirectory);
		this.updateEdexCamel(edexDirectory);
	}

	private void updateEdexSetup(String srcEdexDirectory, String edexDirectory)
			throws FileNotFoundException, IOException, IllegalStateException {
		String srcsetup_env = srcEdexDirectory + "/bin/setup.env";
		String setup_env = edexDirectory + "/bin/setup.env";

		BufferedReader br = this.getBufferedReader(srcsetup_env);
		BufferedWriter bw = this.getBufferedWriter(setup_env);

		final String line1 = "export DATA_ARCHIVE_ROOT=";
		final String line2 = "export DB_PORT=";
		final String line3 = "export BROKER_ADDR=";
		final String line4 = "export HTTP_PORT=";
		final String line5 = "export JMS_SERVER=";
		final String line6 = "export SHARE_DIR=";
		final String pypiesServerPattern = "(export PYPIES_SERVER=http://.+:)[1-9][0-9]+";
		final Pattern pattern7 = Pattern.compile(pypiesServerPattern);

		String line = StringUtils.EMPTY;
		while ((line = br.readLine()) != null) {
			Matcher matcher = pattern7.matcher(line);

			if (line.startsWith(line1)) {
				line = line1 + this.configuration.getDataArchiveRoot();
			} else if (line.startsWith(line2)) {
				line = line2 + this.configuration.getDatabasePort();
			} else if (line.startsWith(line3)) {
				line = line3 + "localhost:" + this.configuration.getJmsPort();
			} else if (line.startsWith(line4)) {
				line = line4 + this.configuration.getEdexHttpPort();
			} else if (line.startsWith(line5)) {
				line = line5 + "tcp://localhost:"
						+ this.configuration.getJmsPort();
			} else if (line.startsWith(line6)) {
				line = line6 + edexDirectory + "/data/share";
			} else if (matcher.matches()) {
				line = matcher.group(GROUP_INDEX_ONE)
						+ this.configuration.getHttpdPypiesPort();
			}

			bw.write(line + "\n");
		}
		br.close();
		bw.close();
	}

	/* Disable JMX. */
	private void updateEdexWrapper(String srcEdexDirectory, String edexDirectory)
			throws FileNotFoundException, IOException {
		String srcwrapper_conf = srcEdexDirectory + "/bin/wrapper.conf";
		String wrapper_conf = edexDirectory + "/bin/wrapper.conf";

		BufferedReader br = this.getBufferedReader(srcwrapper_conf);
		BufferedWriter bw = this.getBufferedWriter(wrapper_conf);

		/*
		 * We want to replace at least one of the jmx jvm arguments with the
		 * wes2bridge.instance argument.
		 */
		boolean wes2BridgeInstanceAdded = false;

		/*
		 * Disable JMX Remote and add a new wes2bridge.instance JVM argument so
		 * that it will be possible to determine which edex instance belongs to
		 * which test case.
		 */
		/*
		 * This may apply to multiple jvm arguments including: 1)
		 * -Dcom.sun.management.jmxremote.port 2)
		 * -Dcom.sun.management.jmxremote.authenticate 3)
		 * -Dcom.sun.management.jmxremote.ssl
		 */
		final String line1 = "-Dcom.sun.management.jmxremote";
		/* Set the web port; used by uengine spring. */
		final String line2 = "-Dweb.port";
		/* Set the confidential port; used by uengine spring. */
		final String line3 = "-Dconfidential.port";

		String line = StringUtils.EMPTY;
		while ((line = br.readLine()) != null) {
			if (line.contains(line1)) {
				line = this.getJVMArgumentName(line);
				if (wes2BridgeInstanceAdded == false) {
					line += "-Dwes2bridge.instance="
							+ this.configuration.getTestCaseName();
					wes2BridgeInstanceAdded = true;
				}
			} else if (line.contains(line2)) {
				line = this.getJVMArgumentName(line);
				line += line2 + "=" + this.configuration.getWebPort();
			} else if (line.contains(line3)) {
				line = this.getJVMArgumentName(line);
				line += line3 + "=" + this.configuration.getConfidentialPort();
			}

			bw.write(line + "\n");
		}
		br.close();
		bw.close();
	}

	private String getJVMArgumentName(String jvmArgument) {
		if (jvmArgument == null) {
			System.out.println("ERROR: Invalid wrapper.conf file.");
			System.exit(EXIT_FAILURE);
		}

		String[] splitJVMArg = jvmArgument.split("=");
		if (splitJVMArg.length <= 0) {
			System.out.println("ERROR: Invalid wrapper.conf file.");
			System.exit(EXIT_FAILURE);
		}

		return splitJVMArg[0] + "=";
	}

	private void updateEdexCamel(String edexDirectory)
			throws FileNotFoundException, IOException {
		final String srcedex_camel = AWIPSII_WES2BRIDGE_SCRIPTS + "/"
				+ "edex_camel";
		final String edex_camel = this.wes2BridgeScripts + "/edex_camel";

		BufferedReader br = this.getBufferedReader(srcedex_camel);
		BufferedWriter bw = this.getBufferedWriter(edex_camel);

		final String line1 = "EDEX_INSTALL=";
		final String line2 = "export DATA_ARCHIVE_ROOT=";
		final String line3 = "CAMELPROCESS=`ps -ef | grep \"edex.dev.mode\"|grep -c \"edex.run.mode=${1} \" `";

		String line = StringUtils.EMPTY;
		while ((line = br.readLine()) != null) {
			if (line.trim().startsWith(line1)) {
				line = line1 + edexDirectory;
			} else if (line.trim().startsWith(line2)) {
				line = line2 + this.configuration.getDataArchiveRoot();
			} else if (line.trim().startsWith(line3)) {
				line = "CAMELPROCESS=`ps -ef | "
						+ "grep \"wes2bridge.instance="
						+ this.configuration.getTestCaseName() + "\" | "
						+ "grep -c \"edex.run.mode=${1} \" `";
			}

			bw.write(line + "\n");
		}
		br.close();
		bw.close();
	}

	public void reconfigurePostgreSQL() throws FileNotFoundException,
			IOException {
		final String postgresqlRootDirectory = WES2BRIDGE_DIRECTORY + "/"
				+ this.configuration.getTestCaseName();

		this.updateEdexPostgres(postgresqlRootDirectory);
	}

	private void updateEdexPostgres(String postgresqlRootDirectory)
			throws FileNotFoundException, IOException {
		final String srcedex_postgres = AWIPSII_WES2BRIDGE_SCRIPTS + "/"
				+ "edex_postgres";
		final String edex_postgres = this.wes2BridgeScripts + "/edex_postgres";

		BufferedReader br = this.getBufferedReader(srcedex_postgres);
		BufferedWriter bw = this.getBufferedWriter(edex_postgres);

		final String line1 = "POSTGRESQL_INSTALL_ROOT=";
		final String line2 = "PGPORT=";

		String line = StringUtils.EMPTY;
		while ((line = br.readLine()) != null) {
			if (line.startsWith(line1)) {
				line = line1 + postgresqlRootDirectory;
			} else if (line.startsWith(line2)) {
				line = line2 + this.configuration.getDatabasePort();
			}

			bw.write(line + "\n");
		}
		br.close();
		bw.close();
	}

	public void reconfigureQPID() throws FileNotFoundException, IOException {
		final String srcQpidDirectory = AWIPSII + "/" + "qpid";
		final String qpidDirectory = WES2BRIDGE_DIRECTORY + "/"
				+ this.configuration.getTestCaseName() + "/" + "qpid";

		this.updateQpidConf(srcQpidDirectory, qpidDirectory);
		this.updateQPIDD(qpidDirectory);
		this.updateQueueCreatorSH(qpidDirectory);
	}

	/* Updates qpidd.conf */
	private void updateQpidConf(String srcQpidDirectory, String qpidDirectory)
			throws FileNotFoundException, IOException {
		String srcqpidd_conf = srcQpidDirectory + "/etc/qpidd.conf";
		String qpidd_conf = qpidDirectory + "/etc/qpidd.conf";

		BufferedReader br = this.getBufferedReader(srcqpidd_conf);
		BufferedWriter bw = this.getBufferedWriter(qpidd_conf);

		final String line1 = "data-dir=";
		final String line2 = "store-dir=";
		final String line3 = "pid-dir=";
		/*
		 * add the port to qpidd.conf
		 */
		final String line4 = "auth=no";

		String line = StringUtils.EMPTY;
		while ((line = br.readLine()) != null) {
			if (line.startsWith(line1)) {
				line = line1 + qpidDirectory + "/data";
			} else if (line.startsWith(line2)) {
				line = line2 + qpidDirectory + "/messageStore";
			} else if (line.startsWith(line3)) {
				line = line3 + qpidDirectory + "/var/lock";
			} else if (line.startsWith(line4)) {
				line = line4 + "\nport=" + this.configuration.getJmsPort();
			}

			bw.write(line + "\n");
		}
		br.close();
		bw.close();
	}

	private void updateQPIDD(String qpidDirectory)
			throws FileNotFoundException, IOException {
		final String srcqpidd = AWIPSII_WES2BRIDGE_SCRIPTS + "/" + "qpidd";
		final String qpidd = this.wes2BridgeScripts + "/qpidd";

		BufferedReader br = this.getBufferedReader(srcqpidd);
		BufferedWriter bw = this.getBufferedWriter(qpidd);

		final String line1 = "QPID_HOME=";

		String line = StringUtils.EMPTY;
		while ((line = br.readLine()) != null) {
			if (line.startsWith(line1)) {
				line = line1 + qpidDirectory;
			}

			bw.write(line + "\n");
		}
		br.close();
		bw.close();
	}

	private void updateQueueCreatorSH(String qpidDirectory)
			throws FileNotFoundException, IOException {
		final String srcqueue = AWIPSII_WES2BRIDGE_SCRIPTS + "/"
				+ "queueCreator.sh";
		final String queue = qpidDirectory + "/sbin/queueCreator.sh";

		BufferedReader br = this.getBufferedReader(srcqueue);
		BufferedWriter bw = this.getBufferedWriter(queue);

		final String line1 = "port=";

		String line = StringUtils.EMPTY;
		while ((line = br.readLine()) != null) {
			if (line.startsWith(line1)) {
				line = line1 + this.configuration.getJmsPort();
			}

			bw.write(line + "\n");
		}
		br.close();
		bw.close();
	}

	/*
	 * This method will: 1) update pypies.cfg 2) update httpd.conf
	 */
	public void reconfigurePypies() throws FileNotFoundException, IOException {
		final String srcPypiesDirectory = AWIPSII + File.separator + "pypies";
		final String pypiesDirectory = WES2BRIDGE_DIRECTORY + File.separator
				+ this.configuration.getTestCaseName() + File.separator
				+ "pypies";

		final String srcHttpdPypiesDirectory = AWIPSII + File.separator
				+ "httpd_pypies";
		final String httpdPypiesDirectory = WES2BRIDGE_DIRECTORY
				+ File.separator + this.configuration.getTestCaseName()
				+ File.separator + "httpd_pypies";

		this.updatePypiesCfg(srcPypiesDirectory, pypiesDirectory);
		this.updateHttpdConf(srcHttpdPypiesDirectory, httpdPypiesDirectory);
		this.updateHttpdPypies(httpdPypiesDirectory, pypiesDirectory);
	}

	private void updatePypiesCfg(String srcPypiesDirectory,
			String pypiesDirectory) throws FileNotFoundException, IOException,
			IllegalArgumentException {
		final String pypiesCfgPathSuffix = File.separator + "conf"
				+ File.separator + "pypies.cfg";
		final String srcpypiescfg = srcPypiesDirectory + pypiesCfgPathSuffix;
		final String pypiescfg = pypiesDirectory + pypiesCfgPathSuffix;

		// use the default location for the hdf5 root
		final String hdf5DirectoryLocation = WES2BRIDGE_DIRECTORY
				+ File.separator + this.configuration.getTestCaseName()
				+ DEFAULT_HDF5_DIRECTORY;
		final String logFileDirectoryLocation = pypiesDirectory
				+ File.separator + "logs";

		BufferedReader br = this.getBufferedReader(srcpypiescfg);
		BufferedWriter bw = this.getBufferedWriter(pypiescfg);

		final String hdf5DirPattern = "(hdf5dir=).+";
		final String logFileDirPattern = "(logFileDir=).+";
		final String loggingPortPattern = "(logging_port=)[1-9][0-9]+";
		final Pattern pattern1 = Pattern.compile(hdf5DirPattern);
		final Pattern pattern2 = Pattern.compile(logFileDirPattern);
		final Pattern pattern3 = Pattern.compile(loggingPortPattern);

		String line = StringUtils.EMPTY;
		while ((line = br.readLine()) != null) {
			Matcher matcher1 = pattern1.matcher(line);
			Matcher matcher2 = pattern2.matcher(line);
			Matcher matcher3 = pattern3.matcher(line);

			if (matcher1.matches()) {
				line = matcher1.group(GROUP_INDEX_ONE);
				line += hdf5DirectoryLocation;
			} else if (matcher2.matches()) {
				line = matcher2.group(GROUP_INDEX_ONE);
				line += logFileDirectoryLocation;
			} else if (matcher3.matches()) {
				line = matcher3.group(GROUP_INDEX_ONE);
				line += this.configuration.getPypiesLoggingPort();
			}

			bw.write(line + "\n");
		}

		br.close();
		bw.close();
	}

	private void updateHttpdConf(String srcHttpdPypiesDirectory,
			String httpdPypiesDirectory) throws FileNotFoundException,
			IOException {
		final String httpdConfPathSuffix = File.separator + "etc"
				+ File.separator + "httpd" + File.separator + "conf"
				+ File.separator + "httpd.conf";
		final String srcHttpdConf = srcHttpdPypiesDirectory
				+ httpdConfPathSuffix;
		final String httpdConf = httpdPypiesDirectory + httpdConfPathSuffix;
		final String serverRoot = httpdPypiesDirectory + File.separator + "etc"
				+ File.separator + "httpd";

		BufferedReader br = this.getBufferedReader(srcHttpdConf);
		BufferedWriter bw = this.getBufferedWriter(httpdConf);

		final String listenPattern = "(Listen )[1-9][0-9]+";
		final String serverRootPattern = "(ServerRoot \").+(\")";
		final Pattern pattern1 = Pattern.compile(listenPattern);
		final Pattern pattern2 = Pattern.compile(serverRootPattern);

		String line = StringUtils.EMPTY;
		while ((line = br.readLine()) != null) {
			Matcher matcher1 = pattern1.matcher(line);
			Matcher matcher2 = pattern2.matcher(line);
			if (matcher1.matches()) {
				line = matcher1.group(GROUP_INDEX_ONE);
				line += this.configuration.getHttpdPypiesPort();
			} else if (matcher2.matches()) {
				line = matcher2.group(GROUP_INDEX_ONE);
				line += serverRoot;
				line += matcher2.group(GROUP_INDEX_TWO);
			}

			bw.write(line + "\n");
		}

		br.close();
		bw.close();
	}

	private void updateHttpdPypies(String httpdPypiesDirectory,
			String pypiesDirectory) throws IOException, FileNotFoundException {
		final String srchttpd_pypies = AWIPSII_WES2BRIDGE_SCRIPTS + "/"
				+ "httpd-pypies";
		final String httpd_pypies = this.wes2BridgeScripts + "/httpd-pypies";

		BufferedReader br = this.getBufferedReader(srchttpd_pypies);
		BufferedWriter bw = this.getBufferedWriter(httpd_pypies);

		final String httpdPypiesInstallPattern = "(HTTPD_PYPIES_INSTALL=).+";
		final String loggingCommandPattern = "( *nohup su awips -c \"\\$loggingCmd > /tmp/pypiesLoggingService)(.log 2>&1\" > /dev/null &)";
		final String pypiesConfigurationPattern = "(export PYPIES_CFG=).+";
		final Pattern pattern1 = Pattern.compile(httpdPypiesInstallPattern);
		final Pattern pattern2 = Pattern.compile(loggingCommandPattern);
		final Pattern pattern3 = Pattern.compile(pypiesConfigurationPattern);

		String line = StringUtils.EMPTY;
		while ((line = br.readLine()) != null) {
			Matcher matcher1 = pattern1.matcher(line);
			Matcher matcher2 = pattern2.matcher(line);
			Matcher matcher3 = pattern3.matcher(line);

			if (matcher1.matches()) {
				line = matcher1.group(GROUP_INDEX_ONE);
				line += httpdPypiesDirectory;
			} else if (matcher2.matches()) {
				line = matcher2.group(GROUP_INDEX_ONE);
				line += this.configuration.getTestCaseName();
				line += matcher2.group(GROUP_INDEX_TWO);
			} else if (matcher3.matches()) {
				line = matcher3.group(GROUP_INDEX_ONE) + pypiesDirectory
						+ File.separator + "conf" + File.separator
						+ "pypies.cfg";
			}

			bw.write(line + "\n");
		}

		br.close();
		bw.close();
	}

	/*
	 * The following functions and usage of the following functions would no
	 * longer be necessary with Apache Commons IOUtils.
	 */
	private BufferedReader getBufferedReader(String file)
			throws FileNotFoundException {
		return new BufferedReader(new FileReader(this.getFile(file)));
	}

	private BufferedWriter getBufferedWriter(String file) throws IOException {
		return new BufferedWriter(new FileWriter(this.getFile(file)));
	}

	private File getFile(String file) {
		return new File(file);
	}
}