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

import IPluginCustomDeployer

/**
 * Deploys plugin-provided web applications as war files.
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
class DeployWeb
extends AbstractAntBasedPluginCustomDeployer
{
   private final String WEB_XML_PATH = "web" + File.separator + "WEB-INF" + File.separator + "web.xml"

   public DeployWeb()
   {
      super()
   }

   public void deploy(String edexRootDirectory, String plugin, String pluginFullPath)
   {
      final String edexWebappsDirectory = edexRootDirectory + File.separator + "webapps"

      // determine if the plugin encapsulates a web application
      final String expectedWebXMLPath = pluginFullPath + File.separator + WEB_XML_PATH
      if (new File(expectedWebXMLPath).exists() == false)
      {
         // no web application
         return
      }

      String webAppRootKey = this.getWebAppRootKey(expectedWebXMLPath)
      if (webAppRootKey == null)
      {
         log.log(java.util.logging.Level.WARNING,
            "webAppRootKey not specified in web.xml for plugin - " + plugin + "; skipping web app deployment")
         return
      }
      log.info "Deploying Web Application associated with plugin ... " + plugin

      final String warFile = edexWebappsDirectory + File.separator + webAppRootKey + ".war"
      final String unwarDirectory = edexWebappsDirectory + File.separator + webAppRootKey      
      this.cleanup(edexWebappsDirectory, warFile, unwarDirectory)

      // produce the war file
      this.ant.war( destfile : warFile,
                    webxml : expectedWebXMLPath )
      {
         fileset( dir : pluginFullPath + File.separator + "web",
                  excludes : "WEB-INF" + File.separator )
         classes( dir : pluginFullPath + File.separator + "web" + File.separator +
            "WEB-INF" + File.separator + "classes" )
         webinf ( file : pluginFullPath + File.separator + "web" + File.separator + 
            "WEB-INF" + File.separator + "dwr.xml" )
      }

      // finally, unwar the war file
      this.ant.unzip( src : warFile,
                      dest : unwarDirectory )
      new File(warFile).delete()
   }

   private String getWebAppRootKey(String webXMLFile)
   {
      String webAppRootKey = null
      def webXML = new XmlSlurper().parse(new File(webXMLFile))

      webXML.'context-param'.each 
      {
         String paramName = it.'param-name'.toString() 
         if (paramName == "webAppRootKey")
         {
            webAppRootKey = it.'param-value'.toString()
         }
      }

      return webAppRootKey
   }

   private void cleanup(String edexWebappsDirectory, String warFile, String webAppDirectory)
   {
      // cleanup any previous deployments
      new File(warFile).delete()
      new File(webAppDirectory).deleteDir()
   }
}
