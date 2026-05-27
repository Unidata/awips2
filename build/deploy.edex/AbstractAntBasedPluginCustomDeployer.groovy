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
 * Abstraction of a customized plugin deployer that provides access
 * to a pre-configured groovy Ant Builder. 
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

abstract class AbstractAntBasedPluginCustomDeployer
implements IPluginCustomDeployer
{
   protected AntBuilder ant

   protected AbstractAntBasedPluginCustomDeployer()
   {
      this.ant = new AntBuilder()
      // disable ant logging - based on: 
      // http://stackoverflow.com/questions/15143221/enable-verbose-output-from-groovys-antbuilder
      this.ant.project.getBuildListeners().firstElement().setMessageOutputLevel(0)
   }
}
