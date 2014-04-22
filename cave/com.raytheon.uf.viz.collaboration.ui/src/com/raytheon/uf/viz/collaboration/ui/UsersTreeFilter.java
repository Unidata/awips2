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
package com.raytheon.uf.viz.collaboration.ui;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 13, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class UsersTreeFilter extends ViewerFilter {
    private String currentText;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ViewerFilter#select(org.eclipse.jface.viewers
     * .Viewer, java.lang.Object, java.lang.Object)
     */
    @Override
    public boolean select(Viewer viewer, Object parentElement, Object element) {
        boolean retVal = true;
        if (currentText != null) {
            String labelText = ((ILabelProvider) ((StructuredViewer) viewer)
                    .getLabelProvider()).getText(element);
            if (labelText.equals(currentText)) {
                viewer.setSelection(new StructuredSelection(element));
            }
            if (element instanceof UserId) {
                String[] words = getWords(currentText);
                for (String word : words) {
                    if (!labelText.toUpperCase().contains(word.toUpperCase())) {
                        retVal = false;
                        break;
                    }
                }
            } else {
                retVal = true;
            }
        }
        return retVal;
    }

    private String[] getWords(String text) {
        return text.trim().split("\\s+");
    }

    /**
     * @param currentText
     *            the currentText to set
     */
    public void setCurrentText(String currentText) {
        this.currentText = currentText;
    }
}