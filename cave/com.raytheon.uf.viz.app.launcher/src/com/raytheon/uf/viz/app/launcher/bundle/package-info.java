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
 * Contains definitions of Interfaces and classes implementing the launcher
 * bundle used by the Application Launcher facility. The bundle's XML structure
 * is
 * <P>
 * <PRE>
 *    &lt;launcher title="..."&gt;
 *       &lt;application runDir="..." path="..." name="..."&gt;
 *           &lt;arguments additional="..." optional="..."&gt;
 *              &lt;argument value="..."/&gt;
 *           &lt;/arguments&gt;
 *       &lt;/application&gt;
 *       &lt;settings&gt;
 *          &lt;appsdefaults&gt;
 *             &lt;default name="..." value="..." /&gt;
 *          &lt;/appsdefaults&gt;
 *          &lt;environment&gt;
 *             &lt;variable name="..." value="..." /&gt;
 *          &lt;/environment&gt;
 *          &lt;classpath&gt;
 *             &lt;entry value="..." /&gt;
 *          &lt;/classpath&gt;
 *       &lt;/settings&gt;
 *    &lt;/launcher&gt;
 * </PRE>
 * Notes:
 * <ul>
 * <li>the &lt;settings/&gt; tag set, and it's nested tags, is optional and may
 *     be omitted.
 * <li>the &lt;appsdefaluts/&gt; tag set is used only for Hydro Apps
 * <li>the &lt;classpath/&gt; tag set is used only for Java Apps
 * </ul>
 * The minimal Application Launcher bundle is
 * <PRE>
 *    &lt;launcher title="..."&gt;
 *       &lt;application name="..." /&gt;
 *    &lt;/launcher&gt;
 * </PRE>
 * This bundle attempts to launch the specified executable using the current 
 * system environment. Unless fully qualified, the executable must be in the
 * system path. No arguments are passed to the executable.
 */
package com.raytheon.uf.viz.app.launcher.bundle;