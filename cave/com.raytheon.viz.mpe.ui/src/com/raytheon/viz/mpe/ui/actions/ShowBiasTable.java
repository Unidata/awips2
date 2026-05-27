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
package com.raytheon.viz.mpe.ui.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.RadarBiasTableDialog;

/**
 * Retrieves the defined radar identifies and displays the
 * {@link RadarBiasTableDialog} on success.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 15, 2009  2616      snaples     Initial creation
 * Apr 06, 2016  5512      bkowal      Verify retrieval of the radar identifiers before even
 *                                     attempting to open the Radar Bias Table dialog.
 * May 01, 2018  7027      mduff       Change to launch only a single window.
 * </pre>
 * 
 * @author snaples
 */
public class ShowBiasTable extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        MPEDisplayManager displayManager = MPEDisplayManager.getCurrent();
        displayManager.launchBiasTable();

        return null;
    }

}
