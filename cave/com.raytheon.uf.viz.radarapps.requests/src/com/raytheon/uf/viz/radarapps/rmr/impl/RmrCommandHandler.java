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
package com.raytheon.uf.viz.radarapps.rmr.impl;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.uf.viz.radarapps.rmr.RmrWindow;

/**
 * Handler to open Radar Multiple Request window
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 21, 2016  5901     randerso  Add proper parent shell
 *
 * </pre>
 *
 * @author ????
 */
public class RmrCommandHandler extends AbstractHandler {

    static RmrWindow rmr;

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        if (rmr == null) {
            rmr = new RmrWindow(HandlerUtil.getActiveShell(event));
            rmr.open();
            rmr.getShell().addDisposeListener(new DisposeListener() {
                @Override
                public void widgetDisposed(DisposeEvent e) {
                    rmr = null;
                }
            });
        } else {
            rmr.open();
        }
        return null;
    }

}
