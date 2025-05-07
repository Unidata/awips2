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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBException;

import org.apache.commons.lang3.StringUtils;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.raytheon.wes2bridge.common.configuration.Wes2BridgeCase;
import com.raytheon.wes2bridge.configuration.jaxb.Wes2BridgeJaxbManager;

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
 * Apr 18, 2013 1899       bkowal      Updates qpid 0.18 configuration now.
 * July 2, 2013 2133       bkowal      Updates for yajsw-wrapped qpid
 * Dec 11, 2013 2182       bkowal      Update the postgresql port in
 *                                     postgresql.conf instead of the
 *                                     postgresql startup scripts
 * Aug 14, 2014 3521       bkowal      Updated to use Wes2BridgeCase. Eliminated
 *                                     configuration that is no longer used and
 *                                     updated EDEX re-configuration.
 * Apr 15, 2015 4392       dlovely     Updates the new qpid json configuration now
 * Apr 20, 2015 4392       dlovely     Removed un-used JMX port configuration
 * Nov 12, 2015 5121       bkowal      Write Java, Python, and PSQL locations to
 *                                     setup.env to override the default locations.
 * Mar 07, 2016 5067       bkowal      Update to use fasterxml jackson.
 * Jul 06, 2016 5734       bkowal      Update edex_camel pid lookup match text.
 * Sep 13, 2018 DR20592    mporricelli Modified updateEdexSetup; updated postgres
 *                                     directory path; created
 *                                     updateEdexrequestScript,updateEdexRegistryScript
 * Oct 04, 2018 DR20592    smoorthy    added functionality for separate registry port in
 *                                     updateEdexRegistryScript
 * Apr 15, 2019 21201      smoorthy    added functionality for separate web registry port
 *                                     and fix for modification of new edex_postgres script
 * Mar 15, 2021 8375       randerso    Added code to force DATASTORE_PROIVDER=pypies
 *                                     Major code cleanup
 * Jun 01, 2021 8475       dgilling    Have reconfigureQPID use initialConfig.json instead
 *                                     of config.json.
 * Dec 19, 2022 23367      zalberts    Added --oldest flag to camel_pid pgrep to prevent it
 *                                     from returning multiple pids
 * Sep 12, 2024 2030328    smoorthy    Adjust for systemd files replacing initd startup scripts.
 *
 * </pre>
 *
 * @author bkowal
 */
public class Wes2BridgeManager {
    private static final Path AWIPSII = Paths.get("/awips2");

    private static final Path AWIPSII_WES2BRIDGE_SCRIPTS = AWIPSII
            .resolve(Paths.get("edex-environment", "scripts"));

    private static final Path WES2BRIDGE_DIRECTORY = Paths.get("/usr", "local",
            "edex-environment");

    private static final int GROUP_INDEX_ONE = 1;

    private static final int GROUP_INDEX_TWO = 2;

    private static final int EXIT_FAILURE = -1;

    private static final int EXIT_SUCCESS = 0;

    private static final Path DEFAULT_HDF5_DIRECTORY = Paths.get("edex", "data",
            "hdf5");

    private Wes2BridgeCase wes2BridgeCase;

    private Path wes2BridgeScripts = null;

    private static final TypeReference<Map<String, Object>> MAP_TYPE_REFERENCE = new TypeReference<Map<String, Object>>() {
    };

    private static final String QPID_NAME = "name";

    private static final String QPID_AMQP = "AMQP";

    private static final String QPID_HTTP = "HTTP";

    private static final String QPID_PORT = "port";

    private static final String QPID_PORTS = "ports";

    private Wes2BridgeManager() {
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println(
                    "ERROR: The configuration file has not been specified.");
            System.exit(EXIT_FAILURE);
        }

        Wes2BridgeManager manager = new Wes2BridgeManager();
        try {
            manager.init(args[0]);
            manager.reconfigureEdex();
            manager.reconfigurePostgreSQL();
            manager.reconfigureQPID();
            manager.reconfigurePypies();
        } catch (Exception e1) {
            e1.printStackTrace();
            System.exit(EXIT_FAILURE);
        }

        System.exit(EXIT_SUCCESS);
    }

    private void init(String arg1) throws JAXBException {
        this.wes2BridgeCase = Wes2BridgeJaxbManager
                .toWes2BridgeCase(new File(arg1));

        this.wes2BridgeScripts = WES2BRIDGE_DIRECTORY.resolve(
                Paths.get(this.wes2BridgeCase.getName(), "edex-environment"));
    }

    /*
     * Updates setup.env and wrapper.conf.
     */
    private void reconfigureEdex() throws FileNotFoundException, IOException {
        Path srcEdexDirectory = AWIPSII.resolve("edex");
        Path edexDirectory = WES2BRIDGE_DIRECTORY
                .resolve(Paths.get(this.wes2BridgeCase.getName(), "edex"));

        this.updateEdexSetup(srcEdexDirectory, edexDirectory);
        this.updateEdexWrapper(srcEdexDirectory, edexDirectory);
        this.updateEdexCamel(edexDirectory);
        this.updateEdexrequestScript(srcEdexDirectory, edexDirectory);
        this.updateEdexRegistryScript(srcEdexDirectory, edexDirectory);
        this.updateRegistryProperties(srcEdexDirectory, edexDirectory);
    }

    private void updateEdexSetup(Path srcEdexDirectory, Path edexDirectory)
            throws FileNotFoundException, IOException, IllegalStateException {
        Path srcsetup_env = srcEdexDirectory
                .resolve(Paths.get("bin", "setup.env"));
        Path setup_env = edexDirectory.resolve(Paths.get("bin", "setup.env"));

        try (BufferedReader br = Files.newBufferedReader(srcsetup_env);
                BufferedWriter bw = Files.newBufferedWriter(setup_env)) {

            final String line1 = "export DATA_ARCHIVE_ROOT=";
            final String line2 = "export DB_PORT=";
            final String line3 = "export BROKER_PORT=";
            final String line4 = "export HTTP_PORT=";
            final String line5 = "export BROKER_HTTP=";
            final String line6 = "export SHARE_DIR=";
            final String line7 = "export PYPIES_PORT=";
            final String line8 = "export DATASTORE_PROVIDER=";

            String line = StringUtils.EMPTY;
            while ((line = br.readLine()) != null) {

                if (line.startsWith(line1)) {
                    line = line1 + this.wes2BridgeCase.getDataArchiveRoot();
                } else if (line.startsWith(line2)) {
                    line = line2 + this.wes2BridgeCase.getDatabasePort();
                } else if (line.startsWith(line3)) {
                    line = line3 + this.wes2BridgeCase.getJmsPort();
                } else if (line.startsWith(line4)) {
                    line = line4 + this.wes2BridgeCase.getEdexHttpPort();
                } else if (line.startsWith(line5)) {
                    line = line5 + this.wes2BridgeCase.getQpidHttpPort();
                } else if (line.startsWith(line6)) {
                    line = line6
                            + edexDirectory.resolve(Paths.get("data", "share"));
                } else if (line.startsWith(line7)) {
                    line = line7 + this.wes2BridgeCase.getHttpdPypiesPort();
                } else if (line.startsWith(line8)) {
                    line = line8 + "pypies";
                }

                bw.write(line + "\n");
            }

            /*
             * Need to overwrite the Java, Python, and PSQL locations using
             * setup.env ever since edex_camel was updated to use a login shell
             * to run the EDEX start.sh script.
             */
            bw.write("export JAVA_INSTALL=/awips2/java\n");
            bw.write("export PYTHON_INSTALL=/awips2/python\n");
            bw.write("export PSQL_INSTALL=/awips2/psql\n");
        }
    }

    /*
     * Updates request.sh
     */
    private void updateEdexrequestScript(Path srcEdexDirectory,
            Path edexDirectory)
            throws FileNotFoundException, IOException, IllegalStateException {

        Path srcrequest_script = srcEdexDirectory
                .resolve(Paths.get("etc", "request.sh"));
        Path request_script = edexDirectory
                .resolve(Paths.get("etc", "request.sh"));

        try (BufferedReader br = Files.newBufferedReader(srcrequest_script);
                BufferedWriter bw = Files.newBufferedWriter(request_script)) {

            final String line1 = "export HTTP_PORT=";
            String line = StringUtils.EMPTY;
            while ((line = br.readLine()) != null) {
                if (line.startsWith(line1)) {
                    line = line1 + this.wes2BridgeCase.getEdexHttpPort();
                }
                bw.write(line + "\n");
            }
        }
    }

    /*
     * Updates registry.sh
     */
    private void updateEdexRegistryScript(Path srcEdexDirectory,
            Path edexDirectory)
            throws FileNotFoundException, IOException, IllegalStateException {

        Path srcregistry_script = srcEdexDirectory
                .resolve(Paths.get("etc", "registry.sh"));
        Path registry_script = edexDirectory
                .resolve(Paths.get("etc", "registry.sh"));

        try (BufferedReader br = Files.newBufferedReader(srcregistry_script);
                BufferedWriter bw = Files.newBufferedWriter(registry_script)) {

            final String line1 = "export HTTP_PORT=";
            String line = StringUtils.EMPTY;
            while ((line = br.readLine()) != null) {
                if (line.startsWith(line1)) {
                    line = line1 + this.wes2BridgeCase.getHttpRegistryPort();
                }
                bw.write(line + "\n");
            }
        }
    }

    /*
     * Updates com.raytheon.uf.edex.registry.ebxml.properties
     */
    private void updateRegistryProperties(Path srcEdexDirectory,
            Path edexDirectory)
            throws FileNotFoundException, IOException, IllegalStateException {

        Path srcregistryProperties = srcEdexDirectory.resolve(Paths.get("conf",
                "resources", "com.raytheon.uf.edex.registry.ebxml.properties"));
        Path registryProperties = edexDirectory.resolve(Paths.get("conf",
                "resources", "com.raytheon.uf.edex.registry.ebxml.properties"));

        try (BufferedReader br = Files.newBufferedReader(srcregistryProperties);
                BufferedWriter bw = Files
                        .newBufferedWriter(registryProperties)) {
            final String line1 = "ebxml.registry.webserver.port=";
            String line = StringUtils.EMPTY;
            while ((line = br.readLine()) != null) {
                if (line.startsWith(line1)) {
                    line = line1 + this.wes2BridgeCase.getWebRegistryPort();
                }
                bw.write(line + "\n");
            }
        }
    }

    /* Disable JMX. */
    private void updateEdexWrapper(Path srcEdexDirectory, Path edexDirectory)
            throws FileNotFoundException, IOException {

        Path srcwrapper_conf = srcEdexDirectory
                .resolve(Paths.get("conf", "wrapper.conf"));
        Path wrapper_conf = edexDirectory
                .resolve(Paths.get("conf", "wrapper.conf"));

        try (BufferedReader br = Files.newBufferedReader(srcwrapper_conf);
                BufferedWriter bw = Files.newBufferedWriter(wrapper_conf)) {

            /*
             * Add a new wes2bridge.instance JVM argument so that it will be
             * possible to determine which edex instance belongs to which test
             * case.
             */

            int javaAdditionalMax = 0;

            final String line1 = "wrapper.jvm.parameter.order.2=-Daw.site.identifier";

            final String javaAdditionalPatternRegex = "wrapper\\.java\\.additional\\.([0-9]+)=.+";
            final Pattern javaAdditionalPattern = Pattern
                    .compile(javaAdditionalPatternRegex);

            String line = StringUtils.EMPTY;
            while ((line = br.readLine()) != null) {
                Matcher matcher = javaAdditionalPattern.matcher(line);
                if (matcher.matches()) {
                    /* Guaranteed to be numeric based on the regex */
                    int javaAdditional = Integer.parseInt(matcher.group(1));
                    if (javaAdditional > javaAdditionalMax) {
                        javaAdditionalMax = javaAdditional;
                    }
                }

                if (line.equals(line1)) {
                    bw.write(line + "\n");
                    /*
                     * Ensure that the wes2bridge test name will be third in the
                     * list of jvm arguments.
                     */
                    line = "wrapper.jvm.parameter.order.3=-Dwes2bridge.instance";
                }

                bw.write(line + "\n");
            }

            /*
             * add the additional JVM argument.
             */
            ++javaAdditionalMax;
            String jvmArg = "wrapper.java.additional." + javaAdditionalMax
                    + "=-Dwes2bridge.instance=" + this.wes2BridgeCase.getName();
            bw.write(jvmArg);
        }
    }

    private void updateEdexCamel(Path edexDirectory)
            throws FileNotFoundException, IOException {

        final String envName = this.wes2BridgeCase.getName();
        final String envHome = WES2BRIDGE_DIRECTORY.resolve(envName).toString();

        Path srcedex_camel = AWIPSII_WES2BRIDGE_SCRIPTS.resolve("edex_camel@.service");
        Path edex_camel = this.wes2BridgeScripts.resolve("edex_camel_" + envName.toLowerCase() + "@.service");

        Path srcedex_watchdog = AWIPSII_WES2BRIDGE_SCRIPTS.resolve("edex_camel_watchdog.sh");
        Path edex_watchdog = this.wes2BridgeScripts.resolve("edex_camel_" + envName.toLowerCase() +"_watchdog.sh");

        Path srcedex_target = AWIPSII_WES2BRIDGE_SCRIPTS.resolve("edex_camel.target");
        Path edex_target = this.wes2BridgeScripts.resolve("edex_camel_" + envName.toLowerCase() + ".target");


        try (BufferedReader br = Files.newBufferedReader(srcedex_camel);
                BufferedWriter bw = Files.newBufferedWriter(edex_camel)) {

            String line = StringUtils.EMPTY;
            while ((line = br.readLine()) != null) {
                if (line.contains("EDEX_INSTALL") || line.contains("POSTGRESQL_INSTALL") 
                        || line.contains("ExecStart")) {
                    line = line.replace("/awips2", envHome);
                }
                if (line.contains("PATH")){
                    line = line.replace("/awips2/GFESuite/bin", envHome + "/GFESuite/bin");
                    line = line.replace("/awips2/postgresql/bin", envHome + "/postgresql/bin");
                }
                if (line.contains("PartOf") || line.contains("Before") || line.contains("WantedBy")) {
                    line = line.replace("edex_camel", "edex_camel_" + envName.toLowerCase());
                }
                bw.write(line + "\n");
            }
        }

        try (BufferedReader br = Files.newBufferedReader(srcedex_watchdog);
                BufferedWriter bw = Files.newBufferedWriter(edex_watchdog)) {
            String line = StringUtils.EMPTY;
            while ((line = br.readLine()) != null) {
                if (line.contains("/awips2/edex")){
                    line = line.replace("/awips2", envHome);
                }

                if (line.contains("edexServiceList")){
                    line = "SERVICES=( 'request' 'ingest' 'ingestGrib' 'ingestDat' 'registry')";
                }
                if (line.contains("edex_camel")) {
                    line = line.replace("edex_camel", "edex_camel_" + envName.toLowerCase());
                }
                bw.write(line + "\n");
            }
        }

        try (OutputStream os = Files.newOutputStream(edex_target)) {
            Files.copy(srcedex_target, os);
        }
    }

    private void reconfigurePostgreSQL()
            throws FileNotFoundException, IOException {
        Path postgresqlRootDirectory = WES2BRIDGE_DIRECTORY
                .resolve(this.wes2BridgeCase.getName());
        Path srcDataDirectory = AWIPSII.resolve(Paths.get("database", "data"));

        this.updateEdexPostgres(postgresqlRootDirectory);
        this.updatePostgresqlConf(srcDataDirectory);
    }

    private void updateEdexPostgres(Path postgresqlRootDirectory)
            throws FileNotFoundException, IOException {

        final String envName = this.wes2BridgeCase.getName();
        final String envHome = WES2BRIDGE_DIRECTORY.resolve(envName).toString();

        Path srcedex_postgres = AWIPSII_WES2BRIDGE_SCRIPTS
                .resolve("30-postgresql-setup.conf");
        Path edex_postgres = this.wes2BridgeScripts.resolve("postgresql@awips_" 
                + envName.toLowerCase() + ".service.d").resolve("30-postgresql-setup.conf");

        Path srcpostgres_watchdog = AWIPSII_WES2BRIDGE_SCRIPTS
                .resolve("postgres_watchdog.sh");
        Path postgres_watchdog = this.wes2BridgeScripts.resolve("postgres_" + envName.toLowerCase() + "_watchdog.sh");


        Files.createDirectories(edex_postgres.getParent());

        try (BufferedReader br = Files.newBufferedReader(srcedex_postgres);
                BufferedWriter bw = Files.newBufferedWriter(edex_postgres)) {

            String line = StringUtils.EMPTY;
            while ((line = br.readLine()) != null) {
                line = line.replace("/awips2", envHome);
                bw.write(line + "\n");
            }
        }

        try (BufferedReader br = Files.newBufferedReader(srcpostgres_watchdog);
                BufferedWriter bw = Files.newBufferedWriter(postgres_watchdog)) {
            String line = StringUtils.EMPTY;
            while ((line = br.readLine()) != null) {
                if (line.contains("systemd_action")) {
                    line = line.replace("postgresql@awips", "postgresql@awips_" + envName.toLowerCase());
                }
                bw.write(line + "\n");
            }

        }
    }

    private void updatePostgresqlConf(Path srcDataDirectory)
            throws FileNotFoundException, IOException {
        final String postgresqlConf = "postgresql.conf";
        Path srcPostgresqlConf = srcDataDirectory.resolve(postgresqlConf);
        Path destPostgresqlConf = WES2BRIDGE_DIRECTORY
                .resolve(Paths.get(this.wes2BridgeCase.getName(), "database",
                        "data", postgresqlConf));

        final String regex1 = "^(port = )([0-9]+)(.+)";
        final Pattern pattern1 = Pattern.compile(regex1);

        try (BufferedReader br = Files.newBufferedReader(srcPostgresqlConf);
                BufferedWriter bw = Files
                        .newBufferedWriter(destPostgresqlConf)) {

            String line = StringUtils.EMPTY;
            // only used once - clearing it will not be necessary
            StringBuilder stringBuilder = new StringBuilder();
            while ((line = br.readLine()) != null) {
                Matcher matcher = pattern1.matcher(line);
                if (matcher.matches()) {
                    stringBuilder.append(matcher.group(1));
                    stringBuilder.append(this.wes2BridgeCase.getDatabasePort());

                    line = stringBuilder.toString();
                }
                bw.write(line + "\n");
            }
        }
    }

    private void reconfigureQPID() throws FileNotFoundException, IOException {
        final Path srcQpidDirectory = AWIPSII.resolve("qpid");
        final Path qpidDirectory = WES2BRIDGE_DIRECTORY
                .resolve(Paths.get(this.wes2BridgeCase.getName(), "qpid"));

        this.updateQpidConfigJSON(srcQpidDirectory, qpidDirectory);
        this.updateQPIDD(qpidDirectory);
    }

    /* Updates qpid config.json */
    private void updateQpidConfigJSON(Path srcQpidDirectory, Path qpidDirectory)
            throws FileNotFoundException, IOException {

        Path srcconfig_json = srcQpidDirectory.resolve("initialConfig.json");
        Path config_json = qpidDirectory.resolve("initialConfig.json");

        try (InputStream is = Files.newInputStream(srcconfig_json);
                BufferedWriter bw = Files.newBufferedWriter(config_json)) {

            ObjectMapper mapper = new ObjectMapper();
            mapper.configure(JsonParser.Feature.ALLOW_COMMENTS, true);
            Map<String, Object> attributesMap = mapper.readValue(is,
                    MAP_TYPE_REFERENCE);

            @SuppressWarnings("unchecked")
            List<Map<String, Object>> ports = (List<Map<String, Object>>) attributesMap
                    .get(QPID_PORTS);

            for (Map<String, Object> port : ports) {
                String name = (String) port.get(QPID_NAME);
                if (QPID_AMQP.equals(name)) {
                    port.put(QPID_PORT, this.wes2BridgeCase.getJmsPort());
                } else if (QPID_HTTP.equals(name)) {
                    port.put(QPID_PORT, this.wes2BridgeCase.getQpidHttpPort());
                }
            }

            /*
             * Write the updated configuration file to its destination.
             */
            mapper.writerWithDefaultPrettyPrinter().writeValue(bw,
                    attributesMap);
        }
    }

    private void updateQPIDD(Path qpidDirectory)
            throws FileNotFoundException, IOException {
        String qpidd_env_name = "qpidd_" +this.wes2BridgeCase.getName().toLowerCase();
        String qpidd_env_name_svc =  qpidd_env_name +".service";
        String qpid_watchdog_env_name = "qpid_" + 
            this.wes2BridgeCase.getName().toLowerCase() + "_watchdog.sh";

        Path srcqpidd = AWIPSII_WES2BRIDGE_SCRIPTS.resolve("qpidd.service");
        Path qpidd = this.wes2BridgeScripts.resolve(qpidd_env_name_svc);

        Path srcQpidWatchdog = AWIPSII_WES2BRIDGE_SCRIPTS.resolve("qpid_watchdog.sh");
        Path qpidWatchdog = this.wes2BridgeScripts.resolve(qpid_watchdog_env_name);

        try (BufferedReader br = Files.newBufferedReader(srcqpidd);
                BufferedWriter bw = Files.newBufferedWriter(qpidd)) {

            final String line1 = "ExecStart=";
            final String line2 = "remove_watchdog_bypass";
            final String line3 = "bypass_watchdog";

            String line = StringUtils.EMPTY;
            while ((line = br.readLine()) != null) {
                if (line.startsWith(line1)) {
                    line = line1 + qpidDirectory + "/bin/qpid-wrapper -noConsole";
                }
                if (line.contains(line2) || line.contains(line3)) {
                    line = line.replace("qpidd", qpidd_env_name);
                }
                bw.write(line + "\n");
            }
        }

        try (BufferedReader br = Files.newBufferedReader(srcQpidWatchdog);
                BufferedWriter bw = Files.newBufferedWriter(qpidWatchdog)) {

            final String line1 = "systemd_action";

            String line = StringUtils.EMPTY;
            while ((line = br.readLine()) != null) {
                if (line.contains(line1)) {
                    line = line.replace("qpidd", qpidd_env_name);
                }
                bw.write(line + "\n");
            }
        }
    }

    /*
     * This method will: 1) update pypies.cfg 2) update httpd.conf 3) update httpd_pypies_env 
     * 4) update httpd-pypies.service and httpd-pypies-logging.service
     */
    private void reconfigurePypies() throws FileNotFoundException, IOException {
        Path srcPypiesDirectory = AWIPSII.resolve("pypies");
        Path pypiesDirectory = WES2BRIDGE_DIRECTORY
                .resolve(Paths.get(this.wes2BridgeCase.getName(), "pypies"));

        Path srcHttpdPypiesDirectory = AWIPSII.resolve("httpd_pypies");
        Path httpdPypiesDirectory = WES2BRIDGE_DIRECTORY.resolve(
                Paths.get(this.wes2BridgeCase.getName(), "httpd_pypies"));

        this.updatePypiesCfg(srcPypiesDirectory, pypiesDirectory);
        this.updateHttpdConf(srcHttpdPypiesDirectory, httpdPypiesDirectory);
        this.updatePypiesEnv(srcHttpdPypiesDirectory, httpdPypiesDirectory);
        this.updateHttpdPypies(httpdPypiesDirectory, pypiesDirectory);
    }

    private void updatePypiesCfg(Path srcPypiesDirectory, Path pypiesDirectory)
            throws FileNotFoundException, IOException,
            IllegalArgumentException {
        final Path pypiesCfgPathSuffix = Paths.get("conf", "pypies.cfg");
        final Path srcpypiescfg = srcPypiesDirectory
                .resolve(pypiesCfgPathSuffix);
        final Path pypiescfg = pypiesDirectory.resolve(pypiesCfgPathSuffix);

        // use the default location for the hdf5 root
        final Path hdf5DirectoryLocation = WES2BRIDGE_DIRECTORY
                .resolve(this.wes2BridgeCase.getName())
                .resolve(DEFAULT_HDF5_DIRECTORY);
        final Path logFileDirectoryLocation = pypiesDirectory.resolve("logs");

        try (BufferedReader br = Files.newBufferedReader(srcpypiescfg);
                BufferedWriter bw = Files.newBufferedWriter(pypiescfg)) {

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
                    line += this.wes2BridgeCase.getPypiesLoggingPort();
                }

                bw.write(line + "\n");
            }
        }
    }

    private void updateHttpdConf(Path srcHttpdPypiesDirectory,
            Path httpdPypiesDirectory)
            throws FileNotFoundException, IOException {
        final Path httpdConfPathSuffix = Paths.get("etc", "httpd", "conf",
                "httpd.conf");
        final Path srcHttpdConf = srcHttpdPypiesDirectory
                .resolve(httpdConfPathSuffix);
        final Path httpdConf = httpdPypiesDirectory
                .resolve(httpdConfPathSuffix);
        final Path serverRoot = httpdPypiesDirectory
                .resolve(Paths.get("etc", "httpd"));

        final String envName = this.wes2BridgeCase.getName();
        final String envPidLoc = "/run/" + envName; 
        final String envHome = WES2BRIDGE_DIRECTORY.resolve(envName).toString();

        try (BufferedReader br = Files.newBufferedReader(srcHttpdConf);
                BufferedWriter bw = Files.newBufferedWriter(httpdConf)) {

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
                    line += this.wes2BridgeCase.getHttpdPypiesPort();
                } else if (matcher2.matches()) {
                    line = matcher2.group(GROUP_INDEX_ONE);
                    line += serverRoot;
                    line += matcher2.group(GROUP_INDEX_TWO);
                } else if (line.stripLeading().startsWith("PidFile")) {
                    line = line.replace("/run", envPidLoc);
                } else if (line.stripLeading().startsWith("ErrorLog") || line.stripLeading().startsWith("CustomLog")) {
                    line = line.replace("/awips2", envHome);
                }

                bw.write(line + "\n");
            }

            bw.write("<IfModule !mpm_netware_module>\n");
            bw.write("PidFile \"run/httpd.pid\"\n");
            bw.write("</IfModule>\n");

        }
    }

    private void updatePypiesEnv(Path srcHttpdPypiesDirectory,
            Path httpdPypiesDirectory) throws IOException, FileNotFoundException {
        final Path pypiesEnvSuffix = Paths.get("bin", "httpd_pypies_env");
        final Path srcPypiesEnv = srcHttpdPypiesDirectory
                .resolve(pypiesEnvSuffix);
        final Path pypiesEnv = httpdPypiesDirectory
                .resolve(pypiesEnvSuffix);

        final String envName = this.wes2BridgeCase.getName();
        final String envPidLoc = "/run/" + envName; 
        final String envHome = WES2BRIDGE_DIRECTORY.resolve(envName).toString();

        try (BufferedReader br = Files.newBufferedReader(srcPypiesEnv);
                BufferedWriter bw = Files.newBufferedWriter(pypiesEnv)) {
            String line = StringUtils.EMPTY;
            while ((line = br.readLine()) != null) {
                if (line.contains("PYPIES_CFG")) {
                    line = line.replace("/awips2", envHome);
                }
                bw.write(line + "\n");
            }
        }
    }


    private void updateHttpdPypies(Path httpdPypiesDirectory,
            Path pypiesDirectory) throws IOException, FileNotFoundException {

        final String envName = this.wes2BridgeCase.getName();
        final String envPidLoc = "/run/" + envName; 
        final String envHome = WES2BRIDGE_DIRECTORY.resolve(envName).toString();
        
        
        Path srchttpd_pypies = AWIPSII_WES2BRIDGE_SCRIPTS
                .resolve("httpd-pypies.service");
        Path httpd_pypies = this.wes2BridgeScripts.resolve("httpd-pypies_" + envName.toLowerCase() + ".service");

        Path srchttpd_pypies_logging = AWIPSII_WES2BRIDGE_SCRIPTS
                .resolve("httpd-pypies-logging.service");
        Path httpd_pypies_logging = this.wes2BridgeScripts.resolve("httpd-pypies-logging_" + envName.toLowerCase() + ".service");

        Path srcpypies_watchdog = AWIPSII_WES2BRIDGE_SCRIPTS
                .resolve("pypies_watchdog.sh");
        Path pypies_watchdog = this.wes2BridgeScripts.resolve("pypies_" + envName.toLowerCase() + "_watchdog.sh");

        try (BufferedReader br = Files.newBufferedReader(srchttpd_pypies_logging);
                BufferedWriter bw = Files.newBufferedWriter(httpd_pypies_logging)) {
            String line = StringUtils.EMPTY;
            while ((line = br.readLine()) != null) {


                line = line.replace("/awips2", envHome);
                line = line.replace("/run", envPidLoc);

                if (line.contains("BindsTo")) {
                    line = line.replace("httpd-pypies.service", httpd_pypies.getFileName().toString());
                }
                bw.write(line + "\n");

            }
        }

        try (BufferedReader br = Files.newBufferedReader(srchttpd_pypies);
                BufferedWriter bw = Files.newBufferedWriter(httpd_pypies)) {

            String line = StringUtils.EMPTY;
            while ((line = br.readLine()) != null) {

                if (line.contains("ExecStartPre=+mkdir -p /run/httpd_pypies/")) {
                    String newLine = "ExecStartPre=+mkdir -p " + envPidLoc;
                    bw.write(newLine + "\n");
                }

                if (line.contains("Requires")) {
                    line = line.replace("httpd-pypies-logging.service", httpd_pypies_logging.getFileName().toString());
                }

                line = line.replace("/awips2", envHome);
                line = line.replace("/run", envPidLoc);

                bw.write(line + "\n");
            }
        }

        try (BufferedReader br = Files.newBufferedReader(srcpypies_watchdog);
                BufferedWriter bw = Files.newBufferedWriter(pypies_watchdog)) {

            String line = StringUtils.EMPTY;
            while ((line = br.readLine()) != null) {

                if (line.contains("service_action")) {
                    line = line.replace("httpd-pypies", "httpd-pypies_" + envName.toLowerCase());
                }

                bw.write(line + "\n");
            }
        }
    }
}
