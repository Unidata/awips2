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
/**
 * 
 */
package com.raytheon.viz.gfe.dialogs;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * @author wldougher
 * 
 */
public class ThreadedMessageDialog implements Runnable {

    /**
     * A convenience method to open an error dialog with the given title and
     * message.
     * 
     * @param title
     *            The title for the dialog
     * @param message
     *            The message to display to the user
     */
    public static void openError(String title, String message) {
        ThreadedMessageDialog dialog = new ThreadedMessageDialog();
        dialog.setTitle(title);
        dialog.setMessage(message);
        dialog.setStyle(MessageDialog.ERROR);
        List<String> okList = new ArrayList<String>(1);
        okList.add("Ok");
        dialog.setDialogButtonLabels(okList);
        dialog.open();
    }

    /**
     * A convenience method to open an information dialog with the given title
     * and message.
     * 
     * @param title
     *            The title for the dialog
     * @param message
     *            The message to display to the user
     */
    public static void openInformation(String title, String message) {
        ThreadedMessageDialog dialog = new ThreadedMessageDialog();
        dialog.setTitle(title);
        dialog.setMessage(message);
        dialog.setStyle(MessageDialog.INFORMATION);
        List<String> okList = new ArrayList<String>(1);
        okList.add("Ok");
        dialog.setDialogButtonLabels(okList);
        dialog.open();
    }

    /**
     * A convenience method to open a question dialog with the given title and
     * message.
     * 
     * @param title
     *            The title for the dialog
     * @param message
     *            The message to display to the user
     * @return true if the user clicked OK; false otherwise.
     */
    public static boolean openQuestion(String title, String message) {
        ThreadedMessageDialog dialog = new ThreadedMessageDialog();
        dialog.setTitle(title);
        dialog.setMessage(message);
        dialog.setStyle(MessageDialog.QUESTION);
        List<String> okCancelList = new ArrayList<String>(1);
        okCancelList.add("Ok");
        okCancelList.add("Cancel");
        dialog.setDialogButtonLabels(okCancelList);
        dialog.setDefaultIndex(1); // default to Cancel
        int val = dialog.open();
        return (val == 0);
    }

    /**
     * A convenience method to open a warning dialog with the given title and
     * message.
     * 
     * @param title
     *            The title for the dialog
     * @param message
     *            The message to display to the user
     */
    public static void openWarning(String title, String message) {
        ThreadedMessageDialog dialog = new ThreadedMessageDialog();
        dialog.setTitle(title);
        dialog.setMessage(message);
        dialog.setStyle(MessageDialog.WARNING);
        List<String> okList = new ArrayList<String>(1);
        okList.add("Ok");
        dialog.setDialogButtonLabels(okList);
        dialog.open();
    }

    protected int defaultIndex;

    List<String> dialogButtonLabels;

    protected String message;

    protected int style;

    protected String title;

    protected Image titleImage;

    protected volatile int result;

    /**
     * Constructor.
     */
    public ThreadedMessageDialog() {
        title = "";
        message = "";
        style = MessageDialog.NONE;
        dialogButtonLabels = new ArrayList<String>(0);
    }

    /**
     * Display the dialog on the UI thread and return its result code.
     * 
     * @return the result of the JFace MessageDialog's open() call
     */
    public int open() {
        result = SWT.CANCEL;
        Display.getDefault().syncExec(this);
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Runnable#run()
     */
    @Override
    public void run() {
        Shell shell = Display.getDefault().getActiveShell();
        String[] buttonLabels = dialogButtonLabels.toArray(new String[0]);
        MessageDialog dialog = new MessageDialog(shell, title, titleImage,
                message, style, buttonLabels, defaultIndex);
        result = dialog.open();
    }

    /**
     * @return the index of the button that will be chosen by default
     */
    public int getDefaultIndex() {
        return defaultIndex;
    }

    /**
     * Setter for defaultIndex. This is the index of the button that will be
     * chosen if the use does not click on a button.
     * 
     * @param defaultIndex
     *            the defaultIndex to set
     */
    public void setDefaultIndex(int defaultIndex) {
        this.defaultIndex = defaultIndex;
    }

    /**
     * Set the strings to be shown on the buttons.
     * 
     * @return the dialogButtonLabels
     */
    public List<String> getDialogButtonLabels() {
        return dialogButtonLabels;
    }

    /**
     * @param dialogButtonLabels
     *            the dialogButtonLabels to set
     */
    public void setDialogButtonLabels(List<String> dialogButtonLabels) {
        this.dialogButtonLabels = dialogButtonLabels;
    }

    /**
     * Getter for the message the user will be displayed.
     * 
     * @return the message
     */
    public String getMessage() {
        return message;
    }

    /**
     * Setter for the message to show the user in the dialog.
     * 
     * @param message
     *            the message to set
     */
    public void setMessage(String message) {
        this.message = message;
    }

    /**
     * Getter for the image style to display with the dialog. This should be one
     * of the constants defined by the JFace MessageDialog.
     * 
     * @return the style
     */
    public int getStyle() {
        return style;
    }

    /**
     * @param style
     *            the style to set
     */
    public void setStyle(int style) {
        this.style = style;
    }

    /**
     * Getter for the title displayed on the dialog.
     * 
     * @return the title of the dialog.
     */
    public String getTitle() {
        return title;
    }

    /**
     * Setter for the title to display on the dialog.
     * 
     * @param title
     *            the title to set
     */
    public void setTitle(String title) {
        this.title = title;
    }

    /**
     * Getter for the image displayed next to the title in the titlebar.
     * 
     * @return the titleImage
     */
    public Image getTitleImage() {
        return titleImage;
    }

    /**
     * Setter for the image displayed next to the title in the titlebar.
     * 
     * @param titleImage
     *            the titleImage to set
     */
    public void setTitleImage(Image titleImage) {
        this.titleImage = titleImage;
    }
}
