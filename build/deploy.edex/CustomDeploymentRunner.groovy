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
 * Searches for and runs custom plugin-specific groovy scripts named plugin-deploy.groovy 
 * located directly within the plugin directory. The requirement is that the custom
 * plugin deployments must only deploy files to a location relative to the EDEX root directory. 
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

class CustomDeploymentRunner
implements IPluginCustomDeployer {
    private static final String PLUGIN_GROOVY_SCRIPT = "plugin-deploy.groovy"
    private static final String BINDING_EDEX_ROOT = "__EDEX_ROOT__"
    private static final String BINDING_PLUGIN = "__PLUGIN__"
    private static final String BINDING_PLUGIN_PATH = "__PLUGIN_PATH__"

    public void deploy(String edexRootDirectory, String plugin, String pluginFullPath) {
        File groovyScript = new File(pluginFullPath + File.separator + PLUGIN_GROOVY_SCRIPT)
        if (groovyScript.exists() == false) {
            return
        }

        Binding binding = new Binding()
        binding.setVariable(BINDING_EDEX_ROOT, edexRootDirectory)
        binding.setVariable(BINDING_PLUGIN, plugin)
        binding.setVariable(BINDING_PLUGIN_PATH, pluginFullPath)

        def groovyShell = new GroovyShell(binding)
        groovyShell.run(groovyScript)
    }
}
