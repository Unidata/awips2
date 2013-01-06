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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.request.AbstractGfePrivilegedRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.RunProcess;
import com.raytheon.uf.edex.auth.AuthManager;
import com.raytheon.uf.edex.auth.AuthManagerFactory;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;
import com.raytheon.uf.edex.auth.roles.IRoleStorage;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

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
 * Nov 15,2012  15614 	   jdynina     Added check for national center 
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

    /** The logger instance */
    protected static transient Log logger = LogFactory
            .getLog(SvcBackupUtil.class);

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
        pBuilder.environment().put(
                "LOCALIZATION_PATH",
                PropertiesFactory.getInstance().getEnvProperties()
                        .getEnvValue("UTILITYDIR"));
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
        FileInputStream fis = null;
        try {
            fis = new FileInputStream(
                    EDEXUtil.EDEX_HOME
                            + "/../GFESuite/ServiceBackup/configuration/svcbu.properties");
            svcbuProperties.load(fis);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error reading svcbu.properties file!", e);
            return null;
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error reading svcbu.properties file!", e);
                }
            }
        }
        return svcbuProperties;
    }

    public static boolean errorDetected() {
        File[] fileList = new File(getLockDir()).listFiles();

        for (File file : fileList) {
            if (file.getName().equals("svcbuerr")) {
                return true;
            }
        }
        return false;
    }

    public static void removeErrorFile() {
        File[] fileList = new File(getLockDir()).listFiles();

        for (File file : fileList) {
            if (file.getName().equals("svcbuerr")) {
                file.delete();
            }
        }
    }

    public static void removeLocks() {
        File[] fileList = new File(getLockDir()).listFiles();

        for (File file : fileList) {
            file.delete();
        }
    }

    public static String getLockDir() {
        String lockDir = SvcBackupUtil.getSvcBackupProperties().getProperty(
                "LOCK_DIR")
                + File.separator;
        lockDir = lockDir.replace("${GFESUITE_HOME}",
                EDEXUtil.EDEX_HOME + "/../GFESuite"
                        + File.separator);

        return lockDir;
    }

    public static AuthorizationResponse authorizeWithLocalization(
IUser user,
            AbstractGfePrivilegedRequest request)
            throws AuthorizationException {
        AuthManager manager = AuthManagerFactory.getInstance().getManager();
        IRoleStorage roles = manager.getRoleStorage();
        String roleId = request.getRoleId();
        if (roles
                .isAuthorized(roleId, user.uniqueId().toString(), LOCALIZATION)) {
            return new AuthorizationResponse(true);
        }

        return new AuthorizationResponse(false, "User, " + user
                + ", is not authorized to perform request needing role: "
                + roleId);
    }

    
    public static boolean ncCheck() {
    	String nationalCenter = SvcBackupUtil.getSvcBackupProperties()
    			.getProperty("NATIONAL_CENTER");
    	if (nationalCenter.equals("1")) {
    		return true;
    	}
    	return false;
    }
}
