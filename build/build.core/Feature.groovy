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
 * POJO-based representation of the contents of an Eclipse feature file. 
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
class Feature
{
   private String feature
   private String featureDirectory
   /*
    * The first iteration of the build will assume that when includes
    * are present, there will not be any plugins or dependencies
    * because we will be working with the product feature
    */
   private List includesList
   private List dependenciesList
   private List pluginsList

   public Feature(String feature, String featureDirectory)
   {
      this.feature = feature
      this.featureDirectory = featureDirectory
      this.includesList = []
      this.dependenciesList = []
      this.pluginsList = []
   }

   public String getFeature()
   {
      return this.feature
   }

   public String getFeatureDirectory()
   {
      return this.featureDirectory
   }

   public void addInclude(String includedFeature)
   {
      this.includesList.add(includedFeature)
   }

   public List getIncludes()
   {
      return this.includesList
   }

   public boolean hasIncludes()
   {
      return this.includesList.size() > 0
   }

   public void addDependency(String dependency)
   {
      this.dependenciesList.add(dependency)
   }

   public List getDependencies()
   {
      return this.dependenciesList
   }

   public boolean hasDependencies()
   {
      return this.dependenciesList.size() > 0
   }

   public void addPlugin(String plugin)
   {
      this.pluginsList.add(plugin)
   }

   public List getPlugins()
   {
      return this.pluginsList
   }
}
