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

/**
 * Deploys plugin-provided resource (properties) files.
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

class DeployEdexResources
extends AbstractExternalPluginFilesDeployer
{
   private static final String destinationDirectory = "resources"
   private static final String destinationDirectoryTree = "conf"

   public DeployEdexResources()
   {
      super(destinationDirectory, destinationDirectoryTree)
   }

   public void deploy(String edexRootDirectory, String plugin, String pluginFullPath)
   {
      super.deployExternalFilesystem(edexRootDirectory, pluginFullPath)
   }
}
