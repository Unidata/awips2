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

import groovy.util.logging.*
import java.util.regex.Pattern
import java.util.regex.Matcher
import ProjectInformation

/**
 * Deploys the Raytheon-maintained Python Packages when requested.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 4, 2014  3836       bkowal      Initial Commit
 *
 * </pre>
 *
 * @author bkowal
 * @version 1.0
 */

@Log
class DeployPythonPackages {
    private static final String PYTHON_VERSION_PATTERN_STRING = "python([0-9].+)"
    private static final Pattern pythonVersionPattern =
    Pattern.compile(PYTHON_VERSION_PATTERN_STRING)

    private DeployPythonPackages() {
    }

    public static deploy(String pythonRootDirectory, ProjectInformation projectInformation,
            String[] pythonPackagesToDeploy) {
        if (projectInformation == null) {
            log.log(java.util.logging.Level.WARNING,
                    "Unable to find pythonPackages in the workspace; skipping python deployment")
            return
        }
        if (pythonPackagesToDeploy.length == 0) {
            log.info "No python packages have been specified for deployment; skipping python deployment."
            return
        }

        // determine what the python version directory is
        // loop through all directories in the python lib directory; attempt to find
        // the one that matches our pattern
        final String pythonLibDirectory = pythonRootDirectory + File.separator + "lib"
        String pythonVersion = null
        for (String libFile : new File(pythonLibDirectory).list())
        {
            Matcher matcher = pythonVersionPattern.matcher(libFile)
            if (matcher.matches())
            {
                pythonVersion = matcher.group(1)
            }
        }

        if (pythonVersion == null)
        {
            log.log(java.util.logging.Level.SEVERE,
                    "Unable to find the python version directory in " + pythonLibDirectory)
            System.exit(-1)
        }

        AntBuilder ant = new AntBuilder()
        ant.project.getBuildListeners().firstElement().setMessageOutputLevel(0)

        log.info "Deploying pythonPackages ..."
        final String pythonSitePackagesDirectory = pythonLibDirectory + File.separator +
                "python" + pythonVersion + File.separator + "site-packages"
        for (String pythonPackage : pythonPackagesToDeploy)
        {
            String pythonPackageDirectory = projectInformation.projectFullLocation + File.separator + pythonPackage
            if (pythonPackage == "pypies")
            {
                // special case for pypies
                pythonPackageDirectory += File.separator + "pypies"
            }
            if (new File(pythonPackageDirectory).exists() == false)
            {
                log.log(java.util.logging.Level.WARNING,
                        "Unable to find the " + pythonPackage + " python package in the workspace")
                continue
            }

            String pythonPackageDestination = pythonSitePackagesDirectory + File.separator +
                    pythonPackage
            log.info "Deploying pythonPackage ... " + pythonPackage

            // Remove the existing deployment
            new File(pythonPackageDestination).deleteDir()
            // Create an empty destination directory
            new File(pythonPackageDestination).mkdirs()
            ant.copy( todir : pythonPackageDestination )
            { fileset( dir : pythonPackageDirectory ) }
        }
    }
}
