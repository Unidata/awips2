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
package com.raytheon.viz.awipstools.ui.action;

import com.raytheon.viz.awipstools.ui.layer.VRShearLayer;
import com.raytheon.viz.ui.actions.LoadBundleHandler;

/**
 * Loads a {@link VRShearLayer} from the bundles/tools/VRShear.xml localization
 * file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 03, 2013  2310     bsteffen    Rewritten to extend LoadBundleHandler.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 2.0
 * @deprecated Use {@link LoadBundleHandler} with
 *             bundleFile="bundles/tools/VRShear.xml".
 */
@Deprecated
public class VRShearAction extends LoadBundleHandler {

    public VRShearAction() {
        super("bundles/tools/VRShear.xml");
    }

}
