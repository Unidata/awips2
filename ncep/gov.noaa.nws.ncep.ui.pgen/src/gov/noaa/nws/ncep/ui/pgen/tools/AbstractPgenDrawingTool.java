/*
 * gov.noaa.nws.ncep.ui.pgen.tools.AbstractPgenDrawingTool
 * 
 * 21 May 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlgFactory;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.EditorUtil;

//import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;

/**
 * The abstract super class for all PGEN drawing tools.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/09					B. Yin   	Initial Creation.
 * 06/09					J. Wu   	Pop up "action" dialog if existing, e.g.
 * 										"Extrap", "Interp", etc.
 * 07/09					B. Yin 		Added several handler methods for Jet
 * 03/13         #972       G. Hull     call PgenUtil.isNatlCntrsEditor()
 * 03/13		#927		B. Yin		Added setHandler method.
 * 11/13        #1065       J. Wu       Added Kink Lines.
 * 
 * </pre>
 * 
 * @author B. Yin
 */

public abstract class AbstractPgenDrawingTool extends AbstractPgenTool {

    /**
     * Input handler for mouse events.
     */
    protected IInputHandler mouseHandler;

    /**
     * Attribute dialog of the current element.
     */
    protected AttrDlg attrDlg;

    /**
     * PGEN type of the current element, such as "High Pressure H", etc.
     */
    protected String pgenType = null;

    /**
     * PGEN class of the current element, such as Lines, Front, Symbol, Marker,
     * Text, etc.
     */
    protected String pgenCategory = null;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    protected void activateTool() {
        IEditorPart ep = EditorUtil.getActiveEditor();

        if (!PgenUtil.isNatlCntrsEditor(ep)) {
            // mapEditor = null;
            return;
        }

        if (!super.isDelObj()) {
            /*
             * Activate editor before tool is loaded, so that it is not loaded
             * twice.
             */
            PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getActivePage().activate(EditorUtil.getActiveEditor());
        }

        super.activateTool();

        /*
         * get pgenType and pgenCategory from ExecutionEvent parameters. They
         * are set when Object button is selection on the Palette.
         */
        String param;
        param = event.getParameter("name");
        if (param != null) {
            pgenType = param;
        }

        param = event.getParameter("className");
        if (param != null) {
            pgenCategory = param;
        }

        if (super.isDelObj()) {

            // rest delObj mode because clicking on any Object in Palette
            // de-activiates the delObj tool
            PgenSession.getInstance().getPgenPalette()
                    .setActiveIcon("Delete Obj");
            PgenUtil.setDelObjMode();

            /*
             * pop up the 'delete obj' confirm dialog
             */
            int numEls = drawingLayer.selectObj(pgenType);

            if (numEls > 0) {

                MessageDialog confirmDlg = new MessageDialog(
                        PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                                .getShell(),
                        // PgenSession.getInstance().getPgenPalette().getViewSite().getShell(),
                        "Confirm Delete", null,
                        "Are you sure you want to delete all " + numEls
                                + " selected element(s)?",
                        MessageDialog.QUESTION,
                        new String[] { "OK", "Cancel" }, 0);

                confirmDlg.open();

                if (confirmDlg.getReturnCode() == MessageDialog.OK) {

                    drawingLayer.deleteSelectedElements();

                } else {

                    drawingLayer.removeSelected();

                }

                if (ep instanceof IDisplayPaneContainer) {
                    ((IDisplayPaneContainer) ep).refresh();
                }

            }

            return;
        }

        /*
         * Bring up the action dialog if one exists
         */
        if (attrDlg == null) {
            attrDlg = AttrDlgFactory.createAttrDlg(pgenType, null, PlatformUI
                    .getWorkbench().getActiveWorkbenchWindow().getShell());
        }

        /*
         * If no action dialog, bring up the attribute dialog
         */
        if (attrDlg == null) {

            attrDlg = AttrDlgFactory.createAttrDlg(pgenCategory, pgenType,
                    PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                            .getShell());

        }

        if (attrDlg != null) {

            attrDlg.setType(event.getParameter("type"));
            attrDlg.setMouseHandlerName(this.getMouseHandler().getClass()
                    .getName());
            /*
             * Open the dialog and set the default attributes. Note: must call
             * "attrDlg.setBlockOnOpen(false)" first.
             */
            attrDlg.setBlockOnOpen(false);
            attrDlg.setPgenCategory(pgenCategory);
            attrDlg.setPgenType(pgenType);
            attrDlg.setDrawingLayer(drawingLayer);
            attrDlg.setMapEditor(mapEditor);

            if (attrDlg.getShell() == null) {
                attrDlg.open();
            }

            attrDlg.setBlockOnOpen(true);
            attrDlg.setDefaultAttr();
        }

        /*
         * register the mouse handler if ( this.mouseHandler != null &&
         * mapEditor!=null) {
         * 
         * mapEditor.unregisterMouseHandler( this.mouseHandler );
         * 
         * }
         * 
         * this.mouseHandler = getMouseHandler(); if ( this.mouseHandler != null
         * && mapEditor!=null) mapEditor.registerMouseHandler( this.mouseHandler
         * );
         */

        return;

    }

    // abstract public IInputHandler getMouseHandler();

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractModalTool#deactivateTool()
     */
    // @Override
    public void deactivateTool() {

        super.deactivateTool();

        if (editor != null)
            editor.unregisterMouseHandler(this.mouseHandler);
        if (attrDlg != null) {
            attrDlg.close();
            attrDlg = null;
        }
    }

    /**
     * Sets the mouse handler for the tool.
     */
    public void setHandler(IInputHandler handler) {

        if (mapEditor != null && handler != null) {
            mapEditor.unregisterMouseHandler(this.mouseHandler);
            mouseHandler = handler;
            mapEditor.registerMouseHandler(this.mouseHandler);
        }

        if (handler instanceof InputHandlerDefaultImpl) {
            ((InputHandlerDefaultImpl) handler).preprocess();
        }

    }

    public AttrDlg getAttrDlg() {
        return attrDlg;
    }

    public void setAttrDlg(AttrDlg attrDlg) {
        this.attrDlg = attrDlg;
    }

}
