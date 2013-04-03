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
package com.raytheon.edex.textdb.fax;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import com.raytheon.uf.common.util.RunProcess;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 28, 2010            bfarmer     Initial creation
 * Sep 19, 2011 10955      rferrel     Use RunProcess
 * Jan 10, 2012  4550	   mgamazaychikov	Fixed the sshCommand
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */

public class FaxSender {
    private static final String getDate = "date -u +%Y%m%d%H%M%S";

    // Utility class constructor.
    private FaxSender() {
    }

    public static String sendFax(String faxCompany, String faxNumber,
            String faxRecipient, String faxText, String faxTitle)
            throws IOException {
        String retval = "Success";
        // Get the date. (system exec)
        // DR#10995
        RunProcess p = RunProcess.getRunProcess().exec(getDate);
        String error = p.getStderr().trim();
        if (error.length() != 0) {
            // Send back an appropriate error string.
            retval = "Error reading date from system." + error;
            return retval;
        }
        String date = p.getStdout().trim();
        String fxadata = System.getenv("FXA_DATA");
        // Make fax data directory.
        String faxDir = fxadata + "/workFiles/fax/";
        // Make fax data file name.
        String faxDataFilename = fxadata + "/workFiles/fax/" + faxTitle + date
                + ".data";
        // Make fax script file name.
        String faxScriptFilename = fxadata + "/workFiles/fax/" + faxTitle
                + date + ".msg";
        // Make ldad fax data file name.
        String ldadExternalPublic = System.getenv("LDAD_EXTERNAL_PUBLIC");
        String ldadDataFilename = ldadExternalPublic + "/fax/" + faxTitle
                + date + ".data";
        // Make ldad fax script file name.
        String ldadScriptFilename = ldadExternalPublic + "/fax/" + faxTitle
                + date + ".msg";
        // Construct fax script file.
        StringBuilder faxScript = new StringBuilder();
        faxScript.append("sendfax ");
        if (faxCompany.length() > 0) {
            faxScript.append("-x \"");
            faxScript.append(faxCompany);
            faxScript.append("\" ");
        }
        if (faxRecipient.length() > 0) {
            faxScript.append("-d \"");
            faxScript.append(faxRecipient);
            faxScript.append("@");
            faxScript.append(faxNumber);
            faxScript.append("\" ");
        } else {
            faxScript.append("-d");
            faxScript.append(faxNumber);
            faxScript.append(" ");
        }
        faxScript.append(ldadDataFilename);
        faxScript.append("\n");
        // Write fax script file.
        File makeDirs = new File(faxDir);
        makeDirs.mkdirs();
        File scriptFile = new File(faxScriptFilename);
        scriptFile.createNewFile();
        BufferedWriter scriptWriter = new BufferedWriter(new FileWriter(
                scriptFile));
        scriptWriter.write(faxScript.toString());
        scriptWriter.close();
        // Write fax data file.
        File faxFile = new File(faxDataFilename);
        faxFile.createNewFile();
        BufferedWriter faxWriter = new BufferedWriter(new FileWriter(faxFile));
        faxWriter.write(faxText);
        faxWriter.write("\n");
        faxWriter.close();
        StringBuilder faxDataCommand = new StringBuilder();
        faxDataCommand.append("scp ");
        faxDataCommand.append(faxDataFilename);
        faxDataCommand.append(" ldad@ls1:");
        faxDataCommand.append(ldadDataFilename);
        // Copy fax data file to ldad fax data file (system exec)
        // DR#10955
        RunProcess copyData = RunProcess.getRunProcess().exec(
                faxDataCommand.toString());
        error = copyData.getStderr().trim();
        if (error.length() != 0) {
            // Send back an appropriate error string.
            retval = "Error sending fax data to ldad system. " + error;
            return retval;
        }
        StringBuilder ldadDataCommand = new StringBuilder();
        ldadDataCommand.append("scp ");
        ldadDataCommand.append(faxScriptFilename);
        ldadDataCommand.append(" ldad@ls1:");
        ldadDataCommand.append(ldadScriptFilename);
        // Copy fax script file to ldad fax script file (system exec)
        // DR#10955
        RunProcess copyScript = RunProcess.getRunProcess().exec(
                ldadDataCommand.toString());
        error = copyScript.getStderr().trim();
        if (error.length() != 0) {
            // Send back an appropriate error string.
            retval = "Error sending fax script to ldad system. " + error;
            return retval;
        }
        // Execute faxSender.csh on ldad fax script file (system exec)
        StringBuilder sshCommand = new StringBuilder();
        /*
         * DR4550 - the sshCommand should be:
         * ssh -n ls1 -l ldad $LDAD_EXTERNAL_HOME/bin/faxSender.csh filename
         */
        sshCommand.append("ssh -n ls1 -l ldad ");
        sshCommand.append(System.getenv("LDAD_EXTERNAL_HOME"));
        sshCommand.append("/bin/faxSender.csh ");
        sshCommand.append(ldadScriptFilename);
        // DR#10955
        RunProcess sshCommandExec = RunProcess.getRunProcess().exec(
        		sshCommand.toString());
        error = sshCommandExec.getStderr().trim();
        if (error.length() != 0) {
            // Send back an appropriate error string.
            retval = "Error sending fax command to ldad system. " + error;
            return retval;
        }
        // Return
        return retval;
    }

}
