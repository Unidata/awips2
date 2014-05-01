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
package com.raytheon.viz.mpe.ui.dialogs.postanalysis;

import org.eclipse.swt.layout.GridLayout;

import com.raytheon.viz.ui.panes.PaneManager;

/**
 * Extension of the PaneManager class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 15, 2011            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class PostAnalysisPaneManager extends PaneManager {

    /**
     * Override on the adjustPaneLayout method to get the maps to display
     * side-by-side instead of on top of each other.
     */
    @Override
    protected void adjustPaneLayout(int paneCount) {
        if (composite == null || composite.isDisposed()) {
            return;
        }

        int numColums = paneCount;
        int numRows = 1;

        GridLayout gl = new GridLayout(numColums, true);
        int width = composite.getBounds().width;
        int height = composite.getBounds().height;

        if (numColums > 0 && numRows > 0) {
            gl.horizontalSpacing = width % numColums == 0 ? 2 : 3;
            gl.verticalSpacing = height % numRows == 0 ? 2 : 3;
        }
        gl.marginHeight = 0;
        gl.marginWidth = 0;

        composite.setLayout(gl);
        composite.layout();
        composite.setFocus();
    }
}
