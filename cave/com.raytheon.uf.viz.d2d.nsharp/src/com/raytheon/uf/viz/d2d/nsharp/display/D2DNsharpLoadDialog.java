/**
 * 
 * com.raytheon.uf.viz.d2d.nsharp.view.D2DNsharpLoadDialog
 * 
 * This java class performs the NSHARP D2DNsharpLoadDialog functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package com.raytheon.uf.viz.d2d.nsharp.display;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;

import com.raytheon.uf.viz.d2d.nsharp.display.map.D2DNsharpMapResource;
import com.raytheon.uf.viz.d2d.nsharp.display.D2DNsharpHandleArchiveFile;
import com.raytheon.uf.viz.d2d.nsharp.display.D2DNsharpObservedSoundingDialogContents;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;

public class D2DNsharpLoadDialog extends Dialog {

    private final static int DIALOG_WIDTH = 300;

    private final static int DIALOG_HEIGHT = 700;

    protected Composite top;

    private static Composite dialogParent;

    private static D2DNsharpLoadDialog INSTANCE = null;

    private static Shell shell;

    public static final String soundingTypeString = "Observed Soundings";

    public static final int OBSER_SND = 0;

    public static final int ARCHIVE = 3; // TBDGPD 5;

    private D2DNsharpObservedSoundingDialogContents obsDialog;

    private Group soundingTypeGp;

    private int activeLoadSoundingType;

    private NcSoundingProfile.ObsSndType activeObsSndType = NcSoundingProfile.ObsSndType.BUFRUA;

    private ArrayList<String> obsSelectedTimeList = new ArrayList<String>();

    private String activeGpdProdName = "";

    private ArrayList<String> gpdSelectedTimeList = new ArrayList<String>();

    private Text text1;

    private MessageBox mb;

    private Cursor waitCursor = null;

    private Font newFont;

    public Font getNewFont() {
        return newFont;
    }

    public D2DNsharpObservedSoundingDialogContents getObsDialog() {
        return obsDialog;
    }

    public void setAndOpenMb(String msg) {
        if (mb != null) {
            mb.setMessage(msg);
            try {
                mb.open();
            } catch (Exception e) {
                mb = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK);
                mb.setMessage(msg);
                mb.open();
            }
        }
    }

    private void cleanSelf() {

        if (text1 != null) {
            text1.dispose();
            text1 = null;
        }
    }

    private void cleanupDialog(int activeLoadType) {
        switch (activeLoadType) {
        case OBSER_SND:
            obsDialog.cleanup();
            break;
        default:
            break;
        }

    }

    public void setActiveLoadSoundingType(int activeLoadSoundingType) {
        this.activeLoadSoundingType = activeLoadSoundingType;
    }

    public int getActiveLoadSoundingType() {
        return activeLoadSoundingType;
    }

    public ArrayList<String> getObsSelectedTimeList() {
        return obsSelectedTimeList;
    }

    public void setObsSelectedTimeList(ArrayList<String> obsSelectedTimeList) {
        this.obsSelectedTimeList = obsSelectedTimeList;
    }

    public ArrayList<String> getGpdSelectedTimeList() {
        return gpdSelectedTimeList;
    }

    public void setGpdSelectedTimeList(ArrayList<String> gpdSelectedTimeList) {
        this.gpdSelectedTimeList = gpdSelectedTimeList;
    }

    public NcSoundingProfile.ObsSndType getActiveObsSndType() {
        return activeObsSndType;
    }

    public void setActiveObsSndType(
            NcSoundingProfile.ObsSndType activeObsSndType) {
        this.activeObsSndType = activeObsSndType;
    }

    public String getActiveGpdProdName() {
        return activeGpdProdName;
    }

    public void setActiveGpdProdName(String activeGpdProdName) {
        this.activeGpdProdName = activeGpdProdName;
    }

    static int count = 0;

    public static D2DNsharpLoadDialog getAccess() {
        return INSTANCE;
    }

    public D2DNsharpLoadDialog(Shell parentShell) throws VizException {
        super(parentShell);
        // set modeless so mouse button can be used by others
        this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE
                | SWT.SHELL_TRIM);
        activeLoadSoundingType = OBSER_SND;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        D2DNsharpLoadDialog.shell = shell;
        shell.setSize(getDialogWidth(), DIALOG_HEIGHT);
        shell.setText("D2DLoad");
        mb = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK);

        mb.setMessage("User Input Error!");
        Font font = shell.getFont();
        FontData[] fontData = font.getFontData();
        for (int i = 0; i < fontData.length; i++) {
            fontData[i].setHeight(7);
        }
        newFont = new Font(font.getDevice(), fontData);
        shell.setFont(newFont);
    }

    private void createLoadContents(Composite parent) {
        dialogParent = parent;
        obsDialog = new D2DNsharpObservedSoundingDialogContents(dialogParent);
        obsDialog.createObsvdDialogContents();
        activeLoadSoundingType = OBSER_SND;
    }

    /**
     * Creates the dialog area
     */
    @Override
    public Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        createLoadContents(top);
        // shell.setSize(DIALOG_WIDTH, DIALOG_HEIGHT);
        if (waitCursor == null)
            waitCursor = new Cursor(top.getDisplay(), SWT.CURSOR_WAIT);
        return top;
    }

    @Override
    public int open() {
        if (this.getShell() == null) {
            this.create();
        }
        this.getShell().setLocation(
                this.getShell().getParent().getLocation().x + 1100,
                this.getShell().getParent().getLocation().y + 200);
        D2DNsharpMapResource.bringMapEditorToTop();
        return super.open();
    }

    @Override
    public boolean close() {
//        D2DNsharpMapResource nsharpMapResource = D2DNsharpMapResource.getMapRsc();
//        if (nsharpMapResource != null)
//            nsharpMapResource.setPoints(null);
        cleanSelf();
        cleanupDialog(activeLoadSoundingType);
        // INSTANCE = null;
        if (waitCursor != null)
            waitCursor.dispose();
        waitCursor = null;
        newFont.dispose();
        return (super.close());
    }

    public boolean closeDiaOnly() {
        cleanSelf();
        return (super.close());
    }

    // Only use Cancel button but NOT ok button
    @Override
    public void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CLOSE_LABEL, false);
    }

    public static D2DNsharpLoadDialog getInstance(Shell parShell) {
        if (INSTANCE == null) {
            try {
                INSTANCE = new D2DNsharpLoadDialog(parShell);
            } catch (VizException e) {
                e.printStackTrace();
            }
        }
        return INSTANCE;
    }

    public void startWaitCursor() {
        if (waitCursor != null)
            top.setCursor(waitCursor);
    }

    public void stopWaitCursor() {
        top.setCursor(null);
    }

	public static int getDialogWidth() {
		return DIALOG_WIDTH;
	}
}
