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
 * Parses an Eclipse feature file (feature.xml) and returns a POJO representation
 * of the contents of the feature file.
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

class FeatureParser
{  
   public static Feature parseFeature(String featureFile, String featureDirectory)
   {
      Feature feature = new Feature(featureFile, featureDirectory)

      def featureXML = new XmlSlurper().parse(new File(featureFile))
      featureXML.includes.each {feature.addInclude(it.attributes().id)}
      featureXML.requires.import.each {feature.addDependency(it.attributes().feature)}
      featureXML.plugin.each {feature.addPlugin(it.attributes().id)}

      return feature
   }
}
