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

import javax.xml.bind.JAXBException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.wst.xml.ui.internal.tabletree.XMLMultiPageEditorPart;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.viz.ui.UiPlugin;
import com.raytheon.viz.ui.dialogs.colordialog.ColorData;
import com.raytheon.viz.ui.dialogs.colordialog.IColorEditCompCallback;

/**
 * Colormap editor combined with XML editor
 * 
 * TODO: update text when colormap changes
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 18, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

@SuppressWarnings("restriction")
public class ColorMapMultiPageEditorPart extends XMLMultiPageEditorPart
        implements IColorEditCompCallback {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(ColorMapMultiPageEditorPart.class);

    private ColorMapEditor editor;

    @Override
    protected void createPages() {
        super.createPages();
        createAndAddColorMapPage();
    }

    /**
     * 
     */
    private void createAndAddColorMapPage() {
        try {
            editor = new ColorMapEditor();
            editor.setCallback(this);
            int index = addPage(editor, getEditorInput());
            this.setPageText(index, "Interactive");
            this.setActivePage(index);
        } catch (PartInitException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error adding color edit page", e);
        }
    }

    /**
     * 
     */
    private void colormapChanged() {
        // TODO Update the text when the colormap changes and hope that the save
        // stuff auto gets picked up
        int size = this.getPageCount();
        ITextEditor editor = null;
        for (int i = 0; i < size; ++i) {
            IEditorPart part = getEditor(i);
            if (part instanceof ITextEditor) {
                editor = (ITextEditor) part;
            }
        }
        if (editor != null) {
            try {
                String newXml = SerializationUtil
                        .marshalToXml((ColorMap) getColorMapParameters()
                                .getColorMap());
                IDocument document = editor.getDocumentProvider().getDocument(
                        getEditorInput());
                document.set(newXml);
            } catch (JAXBException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error serializing updated colormap", e);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.colordialog.IColorBarAction#updateColor(com
     * .raytheon.viz.ui.dialogs.colordialog.ColorData, boolean)
     */
    @Override
    public void updateColor(ColorData colorData, boolean upperFlag) {
        editor.updateColor(colorData, upperFlag);
        colormapChanged();
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
        editor.setColor(colorData, colorWheelTitle);
        colormapChanged();
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
        editor.fillColor(colorData);
        colormapChanged();
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
        editor.updateColorMap(newColorMap);
        colormapChanged();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.colordialog.IColorEditCompCallback#
     * getColorMapParameters()
     */
    @Override
    public ColorMapParameters getColorMapParameters() {
        return editor.getColorMapParameters();
    }

    @Override
    public void doSave(IProgressMonitor monitor) {
        super.doSave(monitor);
        // TODO: Pick up saves and refresh color composite
        // TODO: Undo/Redo working? Add revert and interpolate buttons?
    }

}
