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
package com.raytheon.uf.viz.collaboration.ui.session;

import org.eclipse.swt.custom.StyledText;
import org.eclipse.ui.IViewPart;

/**
 * An <code>IViewPart</code> that has printable content. Printable content must
 * be in the form of a <code>StyledText</code> control.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 5, 2012            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public interface IPrintableView extends IViewPart {

    /**
     * The <code>StyledText</code> control that contains the content to be
     * printed. Developers should take care to ensure this never returns null.
     * 
     * @return The content to be printed.
     */
    public abstract StyledText getStyledText();

    /**
     * Defines the text that will be used for the header of the printed
     * document.
     * 
     * @return Header contents.
     */
    public abstract String getHeaderText();
}
