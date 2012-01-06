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
package com.raytheon.viz.ui.cmenu;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;

/**
 * This class will enable or disable frame looping in the smaller D2D panes when
 * the user clicks on the Looping menu option in the context menu.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2010            bkowal     Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class LoopingAction extends AbstractRightClickAction {
	public LoopingAction(IDisplayPaneContainer container) {
		this.setContainer(container);
		setChecked(container.getLoopProperties().isLooping());
	}

	@Override
	public void run() {
		boolean isLooping = false;
		IDisplayPaneContainer container = getContainer();

		if (container == null) {
			return;
		}

		isLooping = !container.getLoopProperties().isLooping();
		container.getLoopProperties().setLooping(isLooping);
		setChecked(isLooping);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.action.Action#getText()
	 */
	@Override
	public String getText() {
		return "Looping";
	}
}
