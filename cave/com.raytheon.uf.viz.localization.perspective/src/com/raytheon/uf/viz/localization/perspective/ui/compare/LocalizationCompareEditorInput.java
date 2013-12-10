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
package com.raytheon.uf.viz.localization.perspective.ui.compare;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.compare.CompareConfiguration;
import org.eclipse.compare.CompareEditorInput;
import org.eclipse.compare.ResourceNode;
import org.eclipse.compare.structuremergeviewer.DiffNode;
import org.eclipse.core.runtime.IProgressMonitor;

import com.raytheon.uf.viz.localization.perspective.editor.LocalizationEditorInput;

/**
 * Comparing editor input for localization files
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 24, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LocalizationCompareEditorInput extends CompareEditorInput {

    private DiffNode node;

    /**
     * @param configuration
     */
    public LocalizationCompareEditorInput(LocalizationEditorInput left,
            LocalizationEditorInput right) {
        super(new CompareConfiguration());

        CompareConfiguration config = getCompareConfiguration();
        config.setLeftEditable(false);
        config.setRightEditable(false);
        config.setLeftLabel(left.getName());
        config.setRightLabel(right.getName());

        node = new DiffNode(new ResourceNode(left.getFile()), new ResourceNode(
                right.getFile()));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.compare.CompareEditorInput#prepareInput(org.eclipse.core.
     * runtime.IProgressMonitor)
     */
    @Override
    protected Object prepareInput(IProgressMonitor monitor)
            throws InvocationTargetException, InterruptedException {
        return node;
    }
}
