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
package com.raytheon.uf.viz.localization.perspective.ui.custom;

import java.io.File;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.EditorPart;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.viz.ui.dialogs.colordialog.ColorData;
import com.raytheon.viz.ui.dialogs.colordialog.ColorEditComposite;
import com.raytheon.viz.ui.dialogs.colordialog.IColorEditCompCallback;

/**
 * ColorMap editor, has page for xml editing, and page for ui based colormap
 * editing
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 18, 2010            mschenke     Initial creation
 * Nov 11, 2013 2361       njensen      Use ColorMap.JAXB for XML processing
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ColorMapEditor extends EditorPart implements
        IColorEditCompCallback {

    private ColorMapParameters fakeParameters;

    private ColorEditComposite colorEditComp;

    private IColorEditCompCallback callback = this;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.colordialog.IColorBarAction#updateColor(com
     * .raytheon.viz.ui.dialogs.colordialog.ColorData, boolean)
     */
    @Override
    public void updateColor(ColorData colorData, boolean upperFlag) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.colordialog.IColorWheelAction#setColor(com
     * .raytheon.viz.ui.dialogs.colordialog.ColorData, java.lang.String)
     */
    @Override
    public void setColor(ColorData colorData, String colorWheelTitle) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.colordialog.IColorWheelAction#fillColor(com
     * .raytheon.viz.ui.dialogs.colordialog.ColorData)
     */
    @Override
    public void fillColor(ColorData colorData) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.colordialog.IColorEditCompCallback#updateColorMap
     * (com.raytheon.uf.common.colormap.ColorMap)
     */
    @Override
    public void updateColorMap(ColorMap newColorMap) {
        fakeParameters.setColorMap(newColorMap);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.colordialog.IColorEditCompCallback#
     * getColorMapParameters()
     */
    @Override
    public ColorMapParameters getColorMapParameters() {
        return fakeParameters;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.EditorPart#doSave(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    public void doSave(IProgressMonitor monitor) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.EditorPart#doSaveAs()
     */
    @Override
    public void doSaveAs() {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.EditorPart#init(org.eclipse.ui.IEditorSite,
     * org.eclipse.ui.IEditorInput)
     */
    @Override
    public void init(IEditorSite site, IEditorInput input)
            throws PartInitException {
        setSite(site);
        IFileEditorInput fei = (IFileEditorInput) input;
        File file = fei.getFile().getRawLocation().toFile();
        try {
            IColorMap cmap = ColorMap.JAXB.unmarshalFromXmlFile(file);
            fakeParameters = new ColorMapParameters();
            fakeParameters.setColorMap(cmap);
            fakeParameters.setColorMapMin(0);
            fakeParameters.setColorMapMax(cmap.getSize() - 1);
            fakeParameters.setDataMin(fakeParameters.getColorMapMin());
            fakeParameters.setDataMax(fakeParameters.getColorMapMax());
        } catch (SerializationException e) {
            throw new PartInitException(
                    "Error deserializing colormap from file: " + file, e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    public void createPartControl(Composite parent) {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        parent.setLayout(mainLayout);
        parent.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        colorEditComp = new ColorEditComposite(parent, SWT.FILL, callback);
        colorEditComp.setLayout(mainLayout);
        colorEditComp
                .setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        // TODO: Add any extras at bottom
    }

    @Override
    public void dispose() {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.EditorPart#isDirty()
     */
    @Override
    public boolean isDirty() {
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.EditorPart#isSaveAsAllowed()
     */
    @Override
    public boolean isSaveAsAllowed() {
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {
        colorEditComp.setFocus();
    }

    public void setCallback(IColorEditCompCallback callback) {
        this.callback = callback;
    }
}
