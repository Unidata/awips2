The com.raytheon.uf.edex.archive.feeder plugin is not part of the standard EDEX deploy.
It is a tool for debugging the binary files generated for archiving.

The easiest way to generate this plugin jar is to add the following to
com.raytheon.edex.feature.uframe/feature.xml:
	
   <includes
         id="com.raytheon.uf.edex.archive.feeder.feature"
         version="0.0.0"/>

Then run the build.edex	deploy-install.xml. This will create the plugin jar file:
/awips2/edex/lib/plugins/com.raytheon.uf.edex.archive.feeder.jar

the needed configuration file:

/awips2/edex/conf/resources/com.raytheon.uf.edex.archive.feeder.properties

By placing these two files in the corresponding locations on the desired EDEX server and
restarting EDEX will start the clusteredArchiveBinaryFeederProc.  You can then drop the
binary files into the archive.feeder.directory, default /tmp/archive-feeder.