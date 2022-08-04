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

import java.util.HashMap;

import com.raytheon.viz.ui.actions.LoadBundleHandler;

/**
 * Show 'Az/Ran' Overlay.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Aug 30, 2013  2310     bsteffen    Rewritten to extend LoadBundleHandler.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 2.0
 * @deprecated Use {@link LoadBundleHandler} with
 *             bundleFile="bundles/tools/AzRan.xml".
 */
@Deprecated
public class AzimuthToolAction extends LoadBundleHandler {

    public AzimuthToolAction() {
        super("bundles/tools/AzRan.xml");
        HashMap<String, String> map = null;
    }

}
