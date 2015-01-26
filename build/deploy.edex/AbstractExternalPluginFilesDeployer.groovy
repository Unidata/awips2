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
 * Abstraction of a customized plugin deployer that deploys
 * plugin-specific files and directories that exist outside
 * of the Java src directory. Files are deployed from
 * the specified externalDirectory in the plugin (if it exists) to the
 * specified destinationDirectoryTree located beneath the root edex directory. 
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

abstract class AbstractExternalPluginFilesDeployer
extends AbstractAntBasedPluginCustomDeployer {
    private String externalDirectory
    private String destinationDirectoryTree

    protected AbstractExternalPluginFilesDeployer(String externalDirectory, String destinationDirectoryTree) {
        super()
        this.externalDirectory = externalDirectory
        this.destinationDirectoryTree = destinationDirectoryTree
    }

    // Add plugin name if we want to add logging to this capability
    protected void deployExternalFilesystem(String edexRootDirectory, String pluginFullPath)
    {
        String fullPluginExternalPath = pluginFullPath + File.separator + this.externalDirectory
        // ensure that this plugin has an external directory
        if (new File(fullPluginExternalPath).exists() == false)
        {
            return
        }

        String fullDestinationPath = edexRootDirectory + File.separator +
                this.destinationDirectoryTree + File.separator + this.externalDirectory
        // ensure that the destination directory exists
        new File(fullDestinationPath).mkdirs()

        // copy the files
        ant.copy( todir : fullDestinationPath, overwrite : true )
        { fileset( dir : fullPluginExternalPath ) }
    }
}
