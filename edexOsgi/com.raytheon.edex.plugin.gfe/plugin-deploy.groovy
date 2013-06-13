import groovy.util.logging.*

@Log
class GFESuiteDeployer
{
   private static final String gfesuite_svcbackup_plugin="com.raytheon.uf.tools.gfesuite.servicebackup"
   private String gfesuitePluginPath

   public GFESuiteDeployer(String pluginPath)
   {
      this.init(pluginPath)
   }
   
   private void init(String pluginPath)
   {
      // Since both plugins are within the same repository sub-directory,
      // they will always be found at the same location in the directory 
      // tree.
      String containingDirectory = new File(pluginPath).getParent()
      this.gfesuitePluginPath = containingDirectory + File.separator +
         gfesuite_svcbackup_plugin
   }
   
   public void deployGFESuite(String edexRoot)
   {
      if (new File(this.gfesuitePluginPath).exists() == false)
      {
         log.log(java.util.logging.Level.WARNING, 
            "Unable to find GFESuite ServiceBackup plugin!")
         return
      }
      
      String containingDirectory = new File(edexRoot).getParent()
      final String destinationDirectory = containingDirectory +
         File.separator + "GFESuite"
      
      log.info("Deploying GFESuite ...")
      
      new File(destinationDirectory).mkdirs()
      AntBuilder ant = new AntBuilder()
      ant.project.getBuildListeners().firstElement().setMessageOutputLevel(0)
      
      final String svcBackupDirectory = this.gfesuitePluginPath + File.separator +
         "svcBackup"
      ant.copy( todir : destinationDirectory, overwrite : true )
      {
         fileset( dir : svcBackupDirectory )
      }
      
      final List sharedDirectories = [ "ISC", "ATBL" ]
      final String productsDirectory = destinationDirectory +
         File.separator + "products"
      for (String sharedDirectory : sharedDirectories)
      {
         new File(productsDirectory + File.separator + 
            sharedDirectory).mkdirs()
      }
      
      // add executable permissions to files in bin
      final String svcBackupBin = svcBackupDirectory + File.separator + "bin"
      final String gfesuiteBin = destinationDirectory + File.separator + "bin"
      for (File binFile : new File(svcBackupBin).listFiles())
      {
         File gfesuiteBinFile = new File(gfesuiteBin + File.separator + 
            binFile.getName())
         this.setPermissions777(gfesuiteBinFile)
      }
      
      // add executable permissions to files in ServiceBackup/scripts
      final String scriptsDir = svcBackupDirectory + File.separator +
         "ServiceBackup" + File.separator + "scripts"
      final String gfesuiteScripts = destinationDirectory + File.separator +
         "ServiceBackup" + File.separator + "scripts"
      for (File binFile : new File(scriptsDir).listFiles())
      {
         File gfesuiteScript = new File(gfesuiteScripts + File.separator +
            binFile.getName())
         this.setPermissions777(gfesuiteScript)
      }
   }
   
   private setPermissions777(File file)
   {
      file.setWritable(true, false)
      file.setReadable(true, false)
      file.setExecutable(true, false)      
   }
}

GFESuiteDeployer gfesuiteDeployer = new GFESuiteDeployer(__PLUGIN_PATH__)
gfesuiteDeployer.deployGFESuite(__EDEX_ROOT__)