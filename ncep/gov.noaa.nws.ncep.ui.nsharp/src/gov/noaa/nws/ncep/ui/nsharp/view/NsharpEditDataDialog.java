/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.palette.NsharpEditDataDialog
 * 
 * This java class performs the NSHARP NsharpLoadDialog functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 04/26/2011	229			Chin Chen	Initial coding
 * 07/16/2014   TTR828      Chin Chen   swapped wind direction and wind speed lines at edit dialog
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.view;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;

import java.util.List;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

public class NsharpEditDataDialog extends Dialog {
    // private NcepLogger logger =
    // NcepLoggerManager.getNcepCaveLogger(this.getClass());
    private List<NcSoundingLayer> curSoundingLayerList;

    private Text curTempText, newTempText, curDewText, newDewText, curWSpText,
            newWSpText, curWDirText, newWDirText, curPressText, newPressText;// curHeightText,
                                                                             // newHeightText;

    private static NsharpEditDataDialog thisDialog = null;

    private org.eclipse.swt.widgets.List pressureList;

    private String tempStr = "", dewStr = "", wspStr = "", wdirStr = "";

    private org.eclipse.swt.graphics.Color lightGrey = new org.eclipse.swt.graphics.Color(
            Display.getDefault(), 211, 211, 211);

    private enum EditType {
        SELECTED_LEVEL, NEW_LEVEL
    }

    private EditType currentEditType;

    private int selIndex = -1;

    protected Composite top;

    public NsharpEditDataDialog(Shell parentShell) {
        super(parentShell);
        // TODO Auto-generated constructor stub
    }

    public NsharpEditDataDialog(IShellProvider parentShell) {
        super(parentShell);
        // TODO Auto-generated constructor stub
    }

    public static NsharpEditDataDialog getInstance(Shell parShell) {

        if (thisDialog == null) {
            thisDialog = new NsharpEditDataDialog(parShell);
            // System.out.println("new parcel dialog INSTANCE created");

        } else {
            // System.out.println("current load dialog INSTANCE returned!");
        }

        return thisDialog;

    }

    private void createDialogContents(Composite parent) {
        top = parent;// (Composite) super.createDialogArea(parent);

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        top.setLayout(mainLayout);

        Group topGp = new Group(top, SWT.SHADOW_OUT);
        topGp.setLayout(new GridLayout(2, false));

        Group pressureListGp = new Group(topGp, SWT.SHADOW_ETCHED_IN);
        pressureListGp.setText("Pressure Level:");
        pressureList = new org.eclipse.swt.widgets.List(pressureListGp,
                SWT.BORDER | SWT.V_SCROLL);
        pressureList.setBounds(pressureListGp.getBounds().x,
                pressureListGp.getBounds().y + NsharpConstants.labelGap,
                NsharpConstants.listWidth + 40, NsharpConstants.listHeight);
        NsharpResourceHandler rsc = null;
        if (NsharpEditor.getActiveNsharpEditor() != null)
            rsc = NsharpEditor.getActiveNsharpEditor().getRscHandler();
        if (rsc != null && rsc.getSoundingLys() != null) {
            curSoundingLayerList = rsc.getSoundingLys();
            for (NcSoundingLayer layer : curSoundingLayerList) {
                pressureList.add(Float.toString(layer.getPressure()));
            }
        }
        // create a selection listener to handle user's selection on list
        pressureList.addListener(SWT.Selection, new Listener() {
            public void handleEvent(Event e) {
                selIndex = pressureList.getSelectionIndex();
            }
        });
        Group buttonGp = new Group(topGp, SWT.SHADOW_OUT);
        buttonGp.setLayout(new GridLayout(1, false));
        buttonGp.setText("Edit Option:");
        Button useSelectedBtn = new Button(buttonGp, SWT.PUSH);
        useSelectedBtn.setText(" Edit Selected Level ");
        useSelectedBtn.setEnabled(true);
        useSelectedBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                currentEditType = EditType.SELECTED_LEVEL;
                if (selIndex != -1) {
                    NcSoundingLayer selLevelSounding = curSoundingLayerList
                            .get(selIndex);
                    curTempText.setText("" + selLevelSounding.getTemperature());
                    curDewText.setText("" + selLevelSounding.getDewpoint());
                    curWSpText.setText("" + selLevelSounding.getWindSpeed());
                    curWDirText.setText(""
                            + selLevelSounding.getWindDirection());
                    curPressText.setText("" + selLevelSounding.getPressure());
                    newTempText.setText("" + selLevelSounding.getTemperature());
                    newDewText.setText("" + selLevelSounding.getDewpoint());
                    newWSpText.setText("" + selLevelSounding.getWindSpeed());
                    newWDirText.setText(""
                            + selLevelSounding.getWindDirection());
                    newPressText.setText("" + selLevelSounding.getPressure());
                }
            }
        });
        Button addNewLevelBtn = new Button(buttonGp, SWT.PUSH);
        addNewLevelBtn.setText("    Add New Level    ");
        addNewLevelBtn.setEnabled(true);
        addNewLevelBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                currentEditType = EditType.NEW_LEVEL;
                curTempText.setText(" N/A ");
                curDewText.setText(" N/A ");
                curWSpText.setText(" N/A ");
                curWDirText.setText(" N/A ");
                curPressText.setText(" N/A ");
                newTempText.setText("");
                newDewText.setText("");
                newWSpText.setText("");
                newWDirText.setText("");
                newPressText.setText("");
            }
        });

        Group top_form = new Group(top, SWT.SHADOW_ETCHED_IN);
        FormLayout formlayout = new FormLayout();
        formlayout.marginRight = 20;
        formlayout.marginLeft = 20;
        top_form.setLayout(formlayout);

        Listener positiveNumberListeener = new Listener() {
            public void handleEvent(Event e) {
                String string = e.text;
                char[] chars = new char[string.length()];
                string.getChars(0, chars.length, chars, 0);
                // to make sure user enter digits only
                for (int i = 0; i < chars.length; i++) {
                    if (!('0' <= chars[i] && chars[i] <= '9')
                            && chars[i] != '.') {
                        e.doit = false;
                        return;
                    }
                }
            }
        };
        Listener numberListeener = new Listener() {
            public void handleEvent(Event e) {
                String string = e.text;
                char[] chars = new char[string.length()];
                string.getChars(0, chars.length, chars, 0);
                // to make sure user enter digits only
                for (int i = 0; i < chars.length; i++) {
                    if (!('0' <= chars[i] && chars[i] <= '9')
                            && chars[i] != '.' && chars[i] != '-') {
                        e.doit = false;
                        return;
                    }
                }
            }
        };
        Label curlbl = new Label(top_form, SWT.NONE);
        curlbl.setText("Current Value");
        FormData fd = new FormData(100, 20);
        fd.top = new FormAttachment(0, 15);
        fd.left = new FormAttachment(0, 120);
        fd.right = new FormAttachment(100, -105);
        curlbl.setLayoutData(fd);

        Label newlbl = new Label(top_form, SWT.NONE);
        newlbl.setText("New Value");
        FormData fd2 = new FormData(100, 20);
        fd2.bottom = new FormAttachment(curlbl, 0, SWT.BOTTOM);
        fd2.left = new FormAttachment(curlbl, 10, SWT.RIGHT);
        newlbl.setLayoutData(fd2);

        curPressText = new Text(top_form, SWT.SINGLE | SWT.BORDER
                | SWT.READ_ONLY);
        curPressText.setBackground(lightGrey);
        fd = new FormData(100, 20);
        fd.top = new FormAttachment(curlbl, 5, SWT.BOTTOM);
        fd.left = new FormAttachment(curlbl, 0, SWT.LEFT);
        fd.right = new FormAttachment(curlbl, 0, SWT.RIGHT);
        curPressText.setLayoutData(fd);

        Label presslbl = new Label(top_form, SWT.NONE);
        presslbl.setText("Pressure (mb)");
        FormData fd1 = new FormData();
        fd1.bottom = new FormAttachment(curPressText, -4, SWT.BOTTOM);
        fd1.right = new FormAttachment(curPressText, -10, SWT.LEFT);
        presslbl.setLayoutData(fd1);

        newPressText = new Text(top_form, SWT.SINGLE | SWT.BORDER);
        newPressText.setText("");
        fd2 = new FormData(100, 20);
        fd2.bottom = new FormAttachment(curPressText, 0, SWT.BOTTOM);
        fd2.left = new FormAttachment(curPressText, 10, SWT.RIGHT);
        newPressText.setLayoutData(fd2);
        newPressText.addListener(SWT.Verify, positiveNumberListeener);

        curTempText = new Text(top_form, SWT.SINGLE | SWT.BORDER
                | SWT.READ_ONLY);
        curTempText.setText(tempStr);
        curTempText.setBackground(lightGrey);
        fd = new FormData(100, 20);
        fd.top = new FormAttachment(curPressText, 5, SWT.BOTTOM);
        fd.left = new FormAttachment(curPressText, 0, SWT.LEFT);
        fd.right = new FormAttachment(curPressText, 0, SWT.RIGHT);
        curTempText.setLayoutData(fd);

        Label templbl = new Label(top_form, SWT.NONE);
        templbl.setText("Temperature (C)");
        fd1 = new FormData();
        fd1.bottom = new FormAttachment(curTempText, -4, SWT.BOTTOM);
        fd1.right = new FormAttachment(curTempText, -10, SWT.LEFT);
        templbl.setLayoutData(fd1);

        newTempText = new Text(top_form, SWT.SINGLE | SWT.BORDER);
        newTempText.setText("");
        fd2 = new FormData(100, 20);
        fd2.bottom = new FormAttachment(curTempText, 0, SWT.BOTTOM);
        fd2.left = new FormAttachment(curTempText, 10, SWT.RIGHT);
        newTempText.setLayoutData(fd2);
        newTempText.addListener(SWT.Verify, numberListeener);

        curDewText = new Text(top_form, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        curDewText.setText(dewStr);
        curDewText.setBackground(lightGrey);
        fd = new FormData(100, 20);
        fd.top = new FormAttachment(curTempText, 5, SWT.BOTTOM);
        fd.left = new FormAttachment(curTempText, 0, SWT.LEFT);
        fd.right = new FormAttachment(curTempText, 0, SWT.RIGHT);
        curDewText.setLayoutData(fd);

        Label dewlbl = new Label(top_form, SWT.NONE);
        dewlbl.setText("DewPoint (C)");
        fd1 = new FormData();
        fd1.bottom = new FormAttachment(curDewText, -4, SWT.BOTTOM);
        fd1.right = new FormAttachment(curDewText, -10, SWT.LEFT);
        dewlbl.setLayoutData(fd1);

        newDewText = new Text(top_form, SWT.SINGLE | SWT.BORDER);
        newDewText.setText("");
        fd2 = new FormData(100, 20);
        fd2.bottom = new FormAttachment(curDewText, 0, SWT.BOTTOM);
        fd2.left = new FormAttachment(curDewText, 10, SWT.RIGHT);
        newDewText.setLayoutData(fd2);
        newDewText.addListener(SWT.Verify, numberListeener);

        curWDirText = new Text(top_form, SWT.SINGLE | SWT.BORDER
                | SWT.READ_ONLY);
        curWDirText.setText(wdirStr);
        curWDirText.setBackground(lightGrey);
        fd = new FormData(100, 20);
        fd.top = new FormAttachment(curDewText, 5, SWT.BOTTOM);
        fd.left = new FormAttachment(curDewText, 0, SWT.LEFT);
        fd.right = new FormAttachment(curDewText, 0, SWT.RIGHT);
        curWDirText.setLayoutData(fd);

        Label wdirlbl = new Label(top_form, SWT.NONE);
        wdirlbl.setText("Wind Direction");
        fd1 = new FormData();
        fd1.bottom = new FormAttachment(curWDirText, -4, SWT.BOTTOM);
        fd1.right = new FormAttachment(curWDirText, -10, SWT.LEFT);
        wdirlbl.setLayoutData(fd1);

        newWDirText = new Text(top_form, SWT.SINGLE | SWT.BORDER);
        newWDirText.setText("");
        fd2 = new FormData(100, 20);
        fd2.bottom = new FormAttachment(curWDirText, 0, SWT.BOTTOM);
        fd2.left = new FormAttachment(curWDirText, 10, SWT.RIGHT);
        newWDirText.setLayoutData(fd2);
        newWDirText.addListener(SWT.Verify, positiveNumberListeener);

        curWSpText = new Text(top_form, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
        curWSpText.setText(wspStr);
        curWSpText.setBackground(lightGrey);
        fd = new FormData(100, 20);
        fd.top = new FormAttachment(curWDirText, 5, SWT.BOTTOM);
        fd.left = new FormAttachment(curWDirText, 0, SWT.LEFT);
        fd.right = new FormAttachment(curWDirText, 0, SWT.RIGHT);
        curWSpText.setLayoutData(fd);

        Label wsplbl = new Label(top_form, SWT.NONE);
        wsplbl.setText("Wind Speed (Knot)");
        fd1 = new FormData();
        fd1.bottom = new FormAttachment(curWSpText, -4, SWT.BOTTOM);
        fd1.right = new FormAttachment(curWSpText, -10, SWT.LEFT);
        wsplbl.setLayoutData(fd1);

        newWSpText = new Text(top_form, SWT.SINGLE | SWT.BORDER);
        newWSpText.setText("");
        fd2 = new FormData(100, 20);
        fd2.bottom = new FormAttachment(curWSpText, 0, SWT.BOTTOM);
        fd2.left = new FormAttachment(curWSpText, 10, SWT.RIGHT);
        newWSpText.setLayoutData(fd2);
        newWSpText.addListener(SWT.Verify, positiveNumberListeener);

    }

    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText("Sounding Data Editor");

    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite top;
        top = (Composite) super.createDialogArea(parent);

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        top.setLayout(mainLayout);
        top.setLocation(0, 0);
        createDialogContents(top);
        return top;
    }

    private void handleEditing() {
        String textStr = newWDirText.getText();
        float winDir = 0, winSpd = 0, tp = 0, dp = 0, press = 0;
        try {
            if ((textStr != null) && !(textStr.isEmpty())) {
                winDir = Float.parseFloat(textStr);
            } else
                winDir = Float.parseFloat(curWDirText.getText());
            textStr = newWSpText.getText();
            if ((textStr != null) && !(textStr.isEmpty())) {
                winSpd = Float.parseFloat(textStr);
            } else
                winSpd = Float.parseFloat(curWSpText.getText());
            textStr = newDewText.getText();
            if ((textStr != null) && !(textStr.isEmpty())) {
                dp = Float.parseFloat(textStr);
            } else
                dp = Float.parseFloat(curDewText.getText());
            textStr = newTempText.getText();
            if ((textStr != null) && !(textStr.isEmpty())) {
                tp = Float.parseFloat(textStr);
            } else
                tp = Float.parseFloat(curTempText.getText());

            textStr = newPressText.getText();
            if ((textStr != null) && !(textStr.isEmpty())) {
                press = Float.parseFloat(textStr);
            } else
                press = Float.parseFloat(curPressText.getText());

            if ((dp > tp) || winDir >= 360.0) {
                Shell shell = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell();
                MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK);

                mb.setMessage("Invalid Data! (Dew Point > Temperature or Wind direction > 360)");
                mb.open();
            } else {
                NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
                if (editor != null) {
                    NsharpResourceHandler rscHandler = editor.getRscHandler();
                    switch (currentEditType) {
                    case NEW_LEVEL:
                        rscHandler.addNewLayer(tp, dp, winSpd, winDir, press);
                        break;
                    case SELECTED_LEVEL:
                        rscHandler.updateLayer(selIndex, tp, dp, winSpd,
                                winDir, press);
                        break;
                    default:
                        return;
                    }
                    editor.refresh();
                }
            }
        } catch (Exception e) {
            // System.out.println("bad input entered " );
            return;
        }
    }

    private boolean verifyNewlevelInput() {

        if ((newWDirText.getText().isEmpty())
                || curWDirText.getText().isEmpty()
                || curWSpText.getText().isEmpty()
                || curDewText.getText().isEmpty()
                || curTempText.getText().isEmpty()) {
            return false;
        }
        return true;
    }

    @Override
    public void createButtonsForButtonBar(Composite parent) {
        // create Apply and Close buttons by default
        Button okBtn = createButton(parent, IDialogConstants.CLIENT_ID,
                "Apply", true);
        okBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                // System.out.println("OK listener is called");
                if (currentEditType == EditType.NEW_LEVEL
                        && verifyNewlevelInput() == false) {
                    Shell shell = PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow().getShell();
                    MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING
                            | SWT.OK);

                    mb.setMessage("Missing input data! Should fill up all 5 entries!");
                    mb.open();
                    return;
                }
                handleEditing();
                // move close from okPressed() to here
                // close();
            }
        });

        Button canBtn = createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CLOSE_LABEL, false);
        canBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                // currentParcel = prevParcel ;
            }
        });
    }

    // @Override
    // This function name is miss leading....
    // This function is called when CR is preseed, but NOT "ok" button.
    // Override this and move close() from here to OK button Listener
    // So, we only close when "OK" is pressed, not "CR".
    // public void okPressed() {
    // System.out.println("CR is pressed");
    // setReturnCode(OK);
    // close();
    // }

    @Override
    public int open() {
        // System.out.println("parcel dialog opened");
        if (this.getShell() == null) {
            this.create();
        }
        return super.open();

    }

    @Override
    public boolean close() {

        thisDialog = null;
        return super.close();
    }

}
