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
import ProjectInformation

/**
 * Deploys the sample EDEX localization files when requested.
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
class DeployEdexSiteLocalization
{
   private static final String EDEX_LOCALIZATION_DIRECTORY = "data" + File.separator +
      "utility"

   private DeployEdexSiteLocalization()
   {
   }

   public static void deploy(String edexRootDirectory, ProjectInformation projectInformation, String site)
   {

      String localizationDestination = edexRootDirectory + File.separator + EDEX_LOCALIZATION_DIRECTORY
      new File(localizationDestination).mkdirs()

      AntBuilder ant = new AntBuilder()
      ant.project.getBuildListeners().firstElement().setMessageOutputLevel(0)

      log.info "Deploying localization for site ... " + site
      ant.copy( todir : localizationDestination, overwrite : true )
      {
         fileset( dir : "../../localization/localization/utility" )
      }
   }
}
