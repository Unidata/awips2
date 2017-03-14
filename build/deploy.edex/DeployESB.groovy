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

/**
 * Deploys the EDEX esb scripts, libraries, and other files.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 4, 2014  3836       bkowal      Initial Commit
 * Dec 9, 2015  4216       dhladky     Fix multi WA deploys
 *
 * </pre>
 *
 * @author bkowal
 * @version 1.0
 */
@Log
class DeployESB
{
   private static final String SETUP_ENV = "setup.env"

   private DeployESB()
   {
   }

   public static void deploy(String edexRootDirectory, String esbDirectory, String overrideArchitecture)
   {
      if (new File(esbDirectory).exists() == false)
      {
         log.log(java.util.logging.Level.SEVERE,
            "The specified esb directory does not exist - " + esbDirectory)
         System.exit(-1)
      }

      new File(edexRootDirectory).mkdirs()

      // deploy the ESB directory structure.
      AntBuilder ant = new AntBuilder()
      ant.project.getBuildListeners().firstElement().setMessageOutputLevel(0)
      
      log.info "Deploying ESB ..."
      ant.copy( todir : edexRootDirectory,
                overwrite : true )
      {
         fileset( dir : esbDirectory )
      }

      // remove setup.env
      new File(edexRootDirectory + File.separator + "bin" + File.separator + SETUP_ENV).delete()
      // remove lib_illusion
      new File(edexRootDirectory + File.separator + "lib" + File.separator + "lib_illusion").deleteDir()

      // copy the correct lib_illusion based on architecture
      // determine architecture?

      // since this is only a temporary consideration since we will eventually be switching to a 64-bit
      // EDEX, we will just look at the OS architecture since the JDK does not provide a simple way
      // to retrieve the required information about the JVM process, itself. The -Darchitecture argument
      // can be used to override the dynamically determined architecture
      String architecture = overrideArchitecture
      if (architecture == "")
      {
         architecture = 
            (System.getProperty("os.arch") == "amd64") ? "x86_64" : "x86"
      }
      
   }

   public static void deployEdexConfiguration(String edexRootDirectory, String esbDirectory)
   {
      final String setupEnvSrc = esbDirectory + File.separator + "bin" + File.separator + SETUP_ENV
      final String destinationDirectory = edexRootDirectory  + File.separator + "bin"

      AntBuilder ant = new AntBuilder()
      ant.project.getBuildListeners().firstElement().setMessageOutputLevel(0)

      ant.copy( todir : destinationDirectory,
                overwrite : true )
      {
         fileset( file : setupEnvSrc )
      }
   }
}
