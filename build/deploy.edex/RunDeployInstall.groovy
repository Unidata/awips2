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
import Feature
import FeatureParser
import java.util.Properties
import java.util.List
import java.util.ArrayList
import java.util.regex.Matcher
import java.util.regex.Pattern
import DeployESB
import DeployPythonPackages
import DeployEdexSiteLocalization
import IPluginCustomDeployer

/**
 * Initial version of the deploy-install driver. Temporarily
 * wrapped by and executed by an ant script until the Groovy
 * plugins become a standard part of the uframe Eclipse distribution.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 4, 2014  3836       bkowal      Initial Commit
 * Dec 9, 2015  4216       dhladky     Fix multi WA deploy
 *
 * </pre>
 *
 * @author bkowal
 * @version 1.0
 */

@Log
class DeployInstall
{
   private static final String edexFeatureRegex = '^.*\\.edex\\..*\\.feature$'
   
   private static final Pattern EDEX_FEATURE_PATTERN = 
       Pattern.compile(edexFeatureRegex)
   
   // temporarily have to declare and add this feature to the list of deployed features
   // manually. there are improvements that can be made in 15.1.    
   private static final String COMMON_BASE_FEATURE = "com.raytheon.uf.common.base.feature"
   
   // temporary. maintain backwards compatibility until baseline cleanup
   private final List<String> blacklistedEdexFeatures 
    
   private projectInformationMap = [:]
   private List<String> edexFeaturesToDeploy
   private String edexRootDirectory
   private String[] localizationSitesToDeploy = []
   private boolean deployPythonPackages
   private String pythonRootDirectory
   private String[] pythonPackagesToDeploy = []
   private String architecture
   private List<IPluginCustomDeployer> customPluginDeployers
   
   // a list of features that have been deployed to ensure that a feature is not deployed
   // more than once.
   private List featuresDeployed = []
   // a list of the plugins that we will be deploying.
   private List pluginsToDeploy = []
   // used to track which feature a plugin was discovered in; will be used to
   // verify that a plugin is not listed in more than one feature.
   private pluginToFeatureMap = [:]

   public DeployInstall(final String workspaceDirectory,
   final String localizationSites, final boolean deployPythonPackages, 
   final String edexRootDirectory, final String pythonRootDirectory, final String pythonPackages,
   final String architecture)
   {
      blacklistedEdexFeatures = new ArrayList<String>()
      // never deploy these features
      blacklistedEdexFeatures.add("com.raytheon.edex.wa.feature")
      blacklistedEdexFeatures.add("com.raytheon.edex.feature.uframe")
      //blacklistedEdexFeatures.add("com.raytheon.uf.edex.hydro.feature")
      //blacklistedEdexFeatures.add("com.raytheon.uf.edex.dat.feature")
       
      this.init(workspaceDirectory.trim())
      if (localizationSites.trim() != "")
      {
         this.localizationSitesToDeploy = localizationSites.trim().split(":")
      }
      this.deployPythonPackages = deployPythonPackages
      this.edexRootDirectory = edexRootDirectory.trim()
      this.pythonRootDirectory = pythonRootDirectory.trim()
      if (pythonPackages.trim() != "")
      {
         this.pythonPackagesToDeploy = pythonPackages.trim().split(":")
      }
      this.architecture = architecture.trim()
   }

   public void deploy()
   {
      // recursively build the list of plugins that will be deployed.
      for (String featureToDeploy : edexFeaturesToDeploy)
      {
          this.buildPluginList(featureToDeploy)
      }
      log.info "Found " + this.pluginsToDeploy.size() + " Plugins To Deploy."
      this.cleanup()
      this.deployPlugins()

      // complete the esb deployment

      // we need to determine the location of the build.edex project
      ProjectInformation projectInformation = this.projectInformationMap["deploy.edex.awips2"]
      if (projectInformation == null)
      {
         log.log(java.util.logging.Level.SEVERE,
            "Unable to find project - build.edex")
         System.exit(-1)
      }
      final String esbDirectory = projectInformation.projectFullLocation + File.separator + "esb"

      DeployESB.deploy(this.edexRootDirectory, esbDirectory, this.architecture)
      DeployESB.deployEdexConfiguration(this.edexRootDirectory, esbDirectory)
      if (this.deployPythonPackages)
      {
         DeployPythonPackages.deploy(this.pythonRootDirectory, 
            this.projectInformationMap["pythonPackages"], this.pythonPackagesToDeploy)
      }
      if (this.localizationSitesToDeploy.length > 0)
      {
         for (String localizationSite : this.localizationSitesToDeploy)
         {
            String localizationProject = "localization." + localizationSite
            DeployEdexSiteLocalization.deploy(this.edexRootDirectory, 
               this.projectInformationMap[localizationProject], localizationSite)
         }
      }
   }

   // remove the existing deployment
   private void cleanup()
   {
      // remove the contents of the edex lib directory (execluding native)
      log.info "Cleaning EDEX lib directory ..."
      final String EDEX_LIB_DIRECTORY = this.edexRootDirectory + File.separator + "lib"
      for (File file : new File(EDEX_LIB_DIRECTORY).listFiles())
      {
         if (file.getName() == "native")
         {
            continue
         }

         file.deleteDir()
      }

      // remove the shell scripts and the yajsw sub-directory from the edex bin directory
      log.info "Cleaning EDEX bin directory ..."
      final String EDEX_BIN_DIRECTORY = this.edexRootDirectory + File.separator + "bin"
      for (File file : new File(EDEX_BIN_DIRECTORY).listFiles())
      {
         if (file.getName() == "yajsw")
         {
            file.deleteDir()
         }

         // we really should be checking the file extension here instead of just doing a
         // basic String comparison
         if (file.getName().endsWith(".sh") || file.getName() == "setup.env")
         {
            file.delete()
         }
      }

      // remove the contents of the edex conf directory
      log.info "Cleaning EDEX conf directory ..."
      final String EDEX_CONF_DIRECTORY = this.edexRootDirectory + File.separator + "conf"
      for (File file : new File(EDEX_CONF_DIRECTORY).listFiles())
      {
         if (file.isDirectory())
         {
            file.deleteDir()
         }
         else
         {
            file.delete()
         }
      }
   }

   private void deployPlugins()
   {
      // we will need ant to complete this
      AntBuilder ant = new AntBuilder()
      ant.project.getBuildListeners().firstElement().setMessageOutputLevel(0)

      log.info "Deploying plugins ..."
      for (String plugin : this.pluginsToDeploy)
      {
         this.deployPlugin(plugin, ant)
      }
   }

   // FOSS plugins are plugins that include jar files.
   private boolean isFOSSPlugin(ProjectInformation projectInformation)
   {
      final String jarSuffix = ".jar"
      // loop through the files in the plugin directory; true if a single
      // jar file is found.
      for (String pluginFile : new File(projectInformation.projectFullLocation).listFiles())
      {
         if (pluginFile.endsWith(".jar"))
         {
            return true
         }
      }

      return false
   }

   private void deployPlugin(String plugin, AntBuilder ant)
   {
      // first, attempt to find the plugin in the plugin map
      ProjectInformation projectInformation = this.projectInformationMap[plugin]
      if (projectInformation == null)
      {
         log.log(java.util.logging.Level.SEVERE,
            "Unable to find plugin - " + plugin)
         System.exit(-1)
      }

      // next, attempt to access the build.properties file for the plugin
      final String PLUGIN_BUILD_PROPERTIES = projectInformation.projectFullLocation + File.separator +
         "build.properties"
      if (new File(PLUGIN_BUILD_PROPERTIES).exists() == false)
      {
         log.log(java.util.logging.Level.SEVERE,
            "Unable to find the build.properties file for plugin - " + plugin)
         System.exit(-1)         
      }

      log.info "Deploying plugin ... " + plugin

      // read the plugin build.properties file
      BufferedReader br =
         new BufferedReader(new FileReader(PLUGIN_BUILD_PROPERTIES))
      Properties properties = new Properties()
      properties.load(br)

      final String output = properties.getProperty("output..")
      final String binIncludes = properties.getProperty("bin.includes")

      if (output == null)
      {
         // we will not be producing a jar file and are most likely deploying a FOSS plugin
         this.deployFOSSPlugin(projectInformation, binIncludes, ant)
         return
      }

      // jar the plugin
      final String pluginJarName = plugin + ".jar"
      final String edexLibPlugins = this.edexRootDirectory + File.separator + "lib" + File.separator +
         "plugins"
      new File(edexLibPlugins).mkdirs()
      final String fullJarPath = edexLibPlugins + File.separator + pluginJarName

      ant.jar( destfile : fullJarPath,
               manifest : projectInformation.projectFullLocation + File.separator + "META-INF" +
                  File.separator + "MANIFEST.MF" )
             {
                fileset( dir : projectInformation.projectFullLocation + File.separator + output )
             }
      
      // is the plugin FOSS?
      if (this.isFOSSPlugin(projectInformation))
      {
         this.deployFOSSPlugin(projectInformation, binIncludes, ant)
         return
      }

      // finish the plugin based on build.properties
      for (String binInclude : binIncludes.split(","))
      {
         binInclude = binInclude.trim()
         if (binInclude == ".")
         {
            continue
         }

         // ensure that the artifact exists
         final String artifact = projectInformation.projectFullLocation + File.separator + binInclude
         if (new File(artifact).exists() == false)
         {
            log.log(java.util.logging.Level.SEVERE,
               artifact + " specified in build.properties for plugin " + projectInformation.project + " was not found")
            System.exit(-1)
         }

         // add the artifact to the jar
         if (new File(artifact).isDirectory())
         {
            ant.jar( destfile : fullJarPath,
                     update : true )
            {
               fileset( dir : projectInformation.projectFullLocation,
                        includes : binInclude )
            }
         }
         else
         {
            ant.jar( destfile : fullJarPath,
                     update : true )
            {
               fileset( file : artifact )
            }
         }      
      }

      // run "custom" deployment steps
      for (IPluginCustomDeployer pluginCustomDeployer : this.customPluginDeployers)
      {
         pluginCustomDeployer.deploy(this.edexRootDirectory, projectInformation.project,
            projectInformation.projectFullLocation)
      }
   }

   private void deployFOSSPlugin(ProjectInformation projectInformation, String binIncludes, AntBuilder ant)
   {
      final String edexLibDependencies = this.edexRootDirectory + File.separator + "lib" + File.separator +
         "dependencies" + File.separator + projectInformation.project
      // create the destination directory
      new File(edexLibDependencies).mkdirs()

      for (String binInclude : binIncludes.split(","))
      {
         binInclude = binInclude.trim()
         if (binInclude == ".")
         {
            continue
         }

         // ensure that the artifact exists
         final String artifact = projectInformation.projectFullLocation + File.separator + binInclude
         if (new File(artifact).exists() == false)
         {
            log.log(java.util.logging.Level.SEVERE,
               artifact + " specified in build.properties for plugin " + projectInformation.project + " was not found")
            System.exit(-1)
         }

         if (new File(artifact).isDirectory())
         {
            ant.copy( todir : edexLibDependencies )
            {
               fileset( dir : artifact )
            }
         }
         else
         {
            ant.copy( todir : edexLibDependencies, file : artifact )
         }
      }
   }

   private void buildPluginList(String featureName)
   {
      if (this.featuresDeployed.contains(featureName))
      {
         log.log(java.util.logging.Level.WARNING,
            "Feature " + featureName + " has been included more than once; skipping the duplicate.")
         return
      }
      log.info "Analyzing feature ... " + featureName

      // first, attempt to find the feature in the project map
      ProjectInformation projectInformation = this.projectInformationMap[featureName]
      // verify that the feature exists
      if (projectInformation == null)
      {
         log.log(java.util.logging.Level.SEVERE,
            "Unable to find the specified feature - " + featureName)
         System.exit(-1)
      }

      final String featureFullPath = projectInformation.projectFullLocation + File.separator + "feature.xml"
      // verify that the feature exists
      if (new File(featureFullPath).exists() == false)
      {
         log.log(java.util.logging.Level.SEVERE,
            "Unable to find the specified feature - " + featureName + "; '" + featureFullPath + "' does not exist")
         System.exit(-1)
      }

      Feature feature = FeatureParser.parseFeature(featureFullPath, featureName)
      // first, process any features that the feature includes
      for (String featureInclude : feature.getIncludes())
      {
         this.buildPluginList(featureInclude)
      }

      // should we also check dependencies?

      // complete an initial analysis of the plugins that we will be deploying
      for (String plugin : feature.getPlugins())
      {
         final String featureWithPlugin = this.pluginToFeatureMap[plugin]
         if (featureWithPlugin == null)
         {
            // we have not seen this plugin yet
            this.pluginToFeatureMap[plugin] = featureName
         }
         else
         {
            // we have seen this plugin before, verify that the plugin
            // is not in more than one feature
            if (featureWithPlugin != featureName)
            {
               log.log(java.util.logging.Level.SEVERE,
                  "Plugin is listed in more than one feature - " + featureWithPlugin + " AND " + featureName)
            }
         }

         if (this.pluginsToDeploy.contains(plugin) == false)
         {
            this.pluginsToDeploy.add(plugin)
         }
      }
   }

   private void init(final String workspaceDirectory)
   {
      this.edexFeaturesToDeploy = new ArrayList<String>()
      final String metadataProjectsDirectory = workspaceDirectory + File.separator +
         ".metadata" + File.separator + ".plugins" + File.separator + "org.eclipse.core.resources" +
         File.separator + ".projects"

      for (String project : new File(metadataProjectsDirectory).list())
      {
         // determine if the project is an EDEX feature.
         Matcher matcher = EDEX_FEATURE_PATTERN.matcher(project)
         if (matcher.matches() || project == COMMON_BASE_FEATURE)
         {
             // this is an EDEX feature.
             if (blacklistedEdexFeatures.contains(project) == false)
             {
                 this.edexFeaturesToDeploy.add(project)
                 log.log(java.util.logging.Level.INFO, 'Found EDEX Feature: ' + project)
             }
         } 
          
         final String expectedLocationFile = metadataProjectsDirectory + 
            File.separator + project + File.separator + ".location"
         if (new File(expectedLocationFile).exists())
         {
            this.catalogProject(expectedLocationFile)
         }
      }

      /*
       * previously this was dynamically loaded via spring. However, spring
       * was removed from groovy.
       */
      this.customPluginDeployers = new ArrayList<IPluginCustomDeployer>()
      this.customPluginDeployers.add(new DeployWeb())
      this.customPluginDeployers.add(new DeployEdexLocalization())
      this.customPluginDeployers.add(new DeployEdexResources())
      this.customPluginDeployers.add(new CustomDeploymentRunner())
      this.customPluginDeployers.add(new DeployModes())
   }

   private void catalogProject(String locationFile)
   {
      byte[] contents = new File(locationFile).getBytes()
      final String FILE_ = "file:"

      StringBuilder stringBuilder = new StringBuilder()
      boolean buildLocationString = false

      for (int i = 0; i < contents.length; i++)
      {
         if (contents[i] < 0 || contents[i] > 127)
         {
            continue
         }

         if (buildLocationString)
         {
            if (contents[i] == 0)
            {
               // the end of the file path (ideally).
               break
            }
            char c = (char) contents[i]
            stringBuilder.append(c)
         }
         else
         {
            // first: we want to find the letter 'f'
            char c = (char) contents[i]
            if ( c == 'f')
            {
               stringBuilder.append(c)
               // we have found 'f'; determine if we have found "file:"
               int counter = 0
               while (counter < 4)
               {
                  ++i
                  c = (char) contents[i] 
                  stringBuilder.append(c)
                  ++counter
               }

               if (FILE_ == stringBuilder.toString())
               {
                  buildLocationString = true
               }

               stringBuilder = new StringBuilder()
            }
         }
      }

      String projectLocationString = stringBuilder.toString()
      // get the .project file
      File projectMetadataFile = 
         new File(projectLocationString + File.separator + ".project")
      // ensure that the project metadata file actually exists
      if (projectMetadataFile.exists() == false)
      {
         return
      }

      // read the file
      def projectMetadataXML = new XmlSlurper().parse(projectMetadataFile)
      // extract the plugin name (as Eclipse sees it)
      final String projectName = projectMetadataXML.name

      ProjectInformation projectInformation = new ProjectInformation()
      projectInformation.project = projectName
      projectInformation.projectDirectory = new File(projectLocationString).getName()
      projectInformation.projectFullLocation = projectLocationString

      this.projectInformationMap[projectName] = projectInformation
   }
}

DeployInstall deployInstall = 
   new DeployInstall(args[0], args[1], Boolean.parseBoolean(args[2]), 
   args[3], args[4], args[5], args[6])
deployInstall.deploy()
