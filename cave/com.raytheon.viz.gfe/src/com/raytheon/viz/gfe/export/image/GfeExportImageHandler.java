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
package com.raytheon.viz.gfe.export.image;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.image.export.dialog.ImageExportDialog;
import com.raytheon.uf.viz.image.export.handler.ExportImageHandler;
import com.raytheon.uf.viz.image.export.options.ImageExportOptions;

/**
 * A custom {@link ExportImageHandler} for GFE which allows it to add options to
 * the dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 22, 2014  2312     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 * @see GfeImageExportDialog
 */
public class GfeExportImageHandler extends ExportImageHandler {

    @Override
    protected boolean openOptionsDialog(Shell shell, ImageExportOptions options) {
        ImageExportDialog dialog = new GfeImageExportDialog(shell, options);
        return dialog.open() != null;
    }

}
