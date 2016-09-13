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

package com.raytheon.edex.plugin.gfe.svcbackup;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Sets;
import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.request.AbstractGfePrivilegedRequest;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.RunProcess;
import com.raytheon.uf.edex.auth.AuthManager;
import com.raytheon.uf.edex.auth.AuthManagerFactory;
import com.raytheon.uf.edex.auth.authorization.IAuthorizer;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.site.SiteAwareRegistry;

/**
 * 
 * Utility class for Service Backup
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 09, 2009            bphillip    Initial creation
 * Sep 19, 2011 10955      rferrel     make sure process destroy is called.
 * Jun 12, 2012 00609      djohnson    Use EDEXUtil for EDEX_HOME.
 * Nov 15, 2012 15614      jdynina     Added check for national center
 * May 02, 2013 #1762      dgilling    Remove check for national center, add
 *                                     method to retrieve list of svcbu
 *                                     sites.
 * May 28, 2014 3211       njensen     Use IAuthorizer instead of IRoleStorage
 * Jul 10, 2014 2914       garmendariz Remove EnvProperties
 * Feb 17, 2015 4103       dgilling    Add getLockDir for specific site, code 
 *                                     cleanup.
 * Mar 27, 2015 4103       dgilling    Support new location for svcbu.properties.
 * Dec 15, 2015 5166       kbisanz     Update logging to use SLF4J
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class SvcBackupUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SvcBackupUtil.class);

    private static final String LOCALIZATION = "Localization";

    public static final String OPERATION_FAIL = "Failure";

    public static final String OPERATION_SUCCESS = "Success";

    private static final String SVCBU_PROPS_PATH = FileUtil.join("config",
            "gfe", "svcbu.properties");

    /** The logger instance */
    protected static transient Logger logger = LoggerFactory
            .getLogger(SvcBackupUtil.class);

    /**
     * A private constructor so that Java does not attempt to create one for us.
     * As this class should not be instantiated, do not attempt to ever call
     * this constructor; it will simply throw an AssertionError.
     * 
     */
    private SvcBackupUtil() {
        throw new AssertionError();
    }

    public static String execute(String... args) throws Exception {
        String[] newArgs = new String[args.length + 1];
        newArgs[0] = "sh";
        System.arraycopy(args, 0, newArgs, 1, newArgs.length - 1);
        return executeProcess(newArgs);
    }

    /**
     * Executes a process using the java.lang.ProcessBuilder.
     * <p>
     * The first argument is the command to execute. The proceeding arguments
     * are the arguments to pass to the command for execution
     * 
     * @param args
     *            First argument is the command. The proceeding arguments are
     *            the arguments to pass to the command for execution
     * @return The output of the process
     * @throws GfeException
     *             If errors occur while executing the process
     */
    private static String executeProcess(String... args) throws GfeException {
        RunProcess proc = RunProcess.getRunProcess();
        ProcessBuilder pBuilder = new ProcessBuilder();
        pBuilder.environment().put("LOCALIZATION_PATH",
                EDEXUtil.getEdexUtility());
        pBuilder.environment().put("AWIPS_HOME", "/awips2/");
        pBuilder.redirectErrorStream(true);
        pBuilder.command(args);
        try {
            proc.setProcess(pBuilder.start());
        } catch (IOException e) {
            throw new GfeException("Error executing process", e);
        }

        int exitValue = 0;
        String processOutput = "";

        exitValue = proc.waitFor();
        if (proc.isProcessInterrupted()) {
            throw new GfeException("Process interrupted");
        }
        processOutput = proc.getStdout();
        if (exitValue != 0) {
            logger.error(processOutput);
            throw new GfeException("Process terminated abnormally");
        }
        return processOutput;
    }

    /**
     * Examines the InputStream of a process and extracts any output into a
     * String
     * 
     * @param p
     *            The process to get the output from
     * @return The output
     * @throws GfeException
     *             If problems occur reading the process output
     */
    public static String getProcessOutput(Process p) throws GfeException {

        String retVal = null;
        InputStream in = p.getInputStream();
        StringBuilder out = new StringBuilder();
        int read = 0;
        final byte[] buffer = new byte[0x10000];
        try {
            do {
                read = in.read(buffer, 0, buffer.length);
                if (read > 0) {
                    out.append(new String(buffer), 0, read);
                }
            } while (read >= 0);
        } catch (IOException e) {
            throw new GfeException("Error reading process output", e);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    logger.error("Unable to close process input stream!", e);
                }
            }
            try {
                p.getOutputStream().close();
            } catch (IOException e1) {
                logger.error("Unable to close process output stream!", e1);
            }

            try {
                p.getErrorStream().close();
            } catch (IOException e1) {
                logger.error("Unable to close process error stream!", e1);
            }
        }

        retVal = out.toString();
        if (retVal.endsWith("\n")) {
            retVal = retVal.substring(0, retVal.length() - 1);
        }
        return retVal;
    }

    public static Properties getSvcBackupProperties() {
        Properties svcbuProperties = new Properties();

        IPathManager pathMgr = PathManagerFactory.getPathManager();

        LocalizationFile basePropsFile = pathMgr.getLocalizationFile(pathMgr
                .getContext(LocalizationType.EDEX_STATIC,
                        LocalizationLevel.BASE), SVCBU_PROPS_PATH);
        try (InputStream input = basePropsFile.openInputStream()) {
            svcbuProperties.load(input);
        } catch (IOException | LocalizationException e) {
            statusHandler.error(
                    "Unable to load BASE level svcbu.properties file.", e);
        }

        LocalizationFile sitePropsFile = pathMgr.getLocalizationFile(pathMgr
                .getContextForSite(LocalizationType.EDEX_STATIC,
                        EDEXUtil.getEdexSite()), SVCBU_PROPS_PATH);
        if (sitePropsFile.exists()) {
            try (InputStream input = sitePropsFile.openInputStream()) {
                svcbuProperties.load(input);
            } catch (IOException | LocalizationException e) {
                statusHandler.error(
                        "Unable to load SITE level svcbu.properties file.", e);
            }
        }

        return svcbuProperties;
    }

    /**
     * Returns the base lock directory for service backup. All site specific
     * lock directories will be children to this directory.
     * 
     * @return The {@code Path} that represents the base directory for service
     *         backup locks.
     */
    public static Path getLockDir() {
        String lockDir = SvcBackupUtil.getSvcBackupProperties().getProperty(
                "LOCK_DIR");
        return Paths.get(lockDir);
    }

    /**
     * Returns the site-specific lock directory for service backup.
     * 
     * @param siteID
     *            The 3-character site identifier.
     * @return he {@code Path} that represents the site-specific directory for
     *         service backup locks.
     */
    public static Path getLockDir(final String siteID) {
        return getLockDir().resolve(siteID.toUpperCase());
    }

    public static AuthorizationResponse authorizeWithLocalization(IUser user,
            AbstractGfePrivilegedRequest request) throws AuthorizationException {
        AuthManager manager = AuthManagerFactory.getInstance().getManager();
        IAuthorizer auth = manager.getAuthorizer();
        String roleId = request.getRoleId();
        if (auth.isAuthorized(roleId, user.uniqueId().toString(), LOCALIZATION)) {
            return new AuthorizationResponse(true);
        }

        return new AuthorizationResponse(false, "User, " + user
                + ", is not authorized to perform request needing role: "
                + roleId);
    }

    public static Set<String> getPrimarySites() {
        Properties svcbuProps = SvcBackupUtil.getSvcBackupProperties();
        String siteList = EDEXUtil.getEdexSite();
        if (svcbuProps != null) {
            String propVal = svcbuProps.getProperty("PRIMARY_SITES", "").trim();
            if (!propVal.isEmpty()) {
                siteList = propVal;
            }
        }

        String[] sites = siteList.split(",");
        Set<String> retVal = new HashSet<String>(sites.length, 1.0f);
        Set<String> validSites = Sets.newHashSet(SiteAwareRegistry
                .getInstance().getActiveSites());
        for (String site : sites) {
            String siteId = site.trim().toUpperCase();
            if (!siteId.isEmpty()) {
                if (validSites.contains(siteId)) {
                    retVal.add(siteId);
                } else {
                    final String msg = "Service backup primary site "
                            + site
                            + " is not a currently activated site. Service backup and export grids tasks cannot be run for this site. Check the PRIMARY_SITES setting in svcbu.properties.";
                    statusHandler.warn(msg);
                }
            }
        }

        return retVal;
    }
}
