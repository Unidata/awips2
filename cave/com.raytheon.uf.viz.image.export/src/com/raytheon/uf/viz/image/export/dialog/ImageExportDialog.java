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
package com.raytheon.uf.viz.image.export.dialog;

import java.io.File;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.image.export.options.ImageExportOptions;
import com.raytheon.uf.viz.image.export.options.ImageExportOptions.DateTimeSelection;
import com.raytheon.uf.viz.image.export.options.ImageExportOptions.FrameSelection;
import com.raytheon.uf.viz.image.export.options.ImageExportOptions.ImageFormat;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog to configure {@link ImageExportOptions} for exporting an image.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 20, 2014  2312     bsteffen    Initial creation
 * Mar 10, 2014  2867     bsteffen    Better frame range validation.
 * Oct 28, 2014  3767     bsteffen    Automatically change filename if selected
 *                                    format does not support all options.
 * Dec 04, 2014  DR16713  jgerth      Support for date/time selection
 * Jul 07, 2015  4607     bsteffen    Prompt to save as GeoTIFF
 * Mar 15, 2016  5485     randerso    Fix GUI sizing issues
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class ImageExportDialog extends CaveSWTDialog {

    protected ImageExportOptions options;

    protected Text locationText;

    protected Button datetimeButton;

    protected Button selectedFramesButton;

    protected Button currentFramesButton;

    protected Button allFramesButton;

    protected Text framesFromText;

    protected Text framesToText;

    protected Button animatedButton;

    protected Text frameDelayText;

    protected Text firstFrameDwellText;

    protected Text lastFrameDwellText;

    public ImageExportDialog(Shell parentShell, ImageExportOptions options) {
        super(parentShell, SWT.RESIZE | SWT.DIALOG_TRIM);
        this.setText("Export Image");
        this.options = options;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        GridData gridData = new GridData(SWT.FILL, SWT.TOP, true, true);
        Group locationGroup = new Group(shell, SWT.NONE);
        locationGroup.setLayoutData(gridData);
        initializeLocationGroup(locationGroup);

        gridData = new GridData(SWT.FILL, SWT.TOP, true, true);
        Group framesGroup = new Group(shell, SWT.NONE);
        framesGroup.setLayoutData(gridData);
        initializeFramesGroup(framesGroup);

        gridData = new GridData(SWT.FILL, SWT.TOP, true, true);
        Group optionsGroup = new Group(shell, SWT.NONE);
        optionsGroup.setLayoutData(gridData);
        initializeAnimationGroup(optionsGroup);

        Composite buttonComposite = new Composite(shell, SWT.NONE);
        gridData = new GridData(SWT.CENTER, SWT.BOTTOM, true, false);
        buttonComposite.setLayoutData(gridData);
        initializeButtons(buttonComposite);

        shell.pack();
        shell.setMinimumSize(shell.getSize());
    }

    protected void initializeLocationGroup(Group group) {
        group.setLayout(new GridLayout(2, false));
        group.setText("Export Location");
        locationText = new Text(group, SWT.BORDER);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.widthHint = 250;
        locationText.setLayoutData(gridData);
        locationText.setText(options.getFileLocation().getAbsolutePath());
        Button button = new Button(group, SWT.PUSH);
        button.setText("Browse ...");
        button.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectDestinationFile();
            }
        });
        datetimeButton = new Button(group, SWT.CHECK);
        gridData = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        datetimeButton.setLayoutData(gridData);
        datetimeButton.setText("Include date and time in file name");
        datetimeButton
                .setSelection(options.getDateTimeSelection() == DateTimeSelection.DATETIME);
        datetimeButton
                .setToolTipText("Append the date and time to the file name when Animate is not selected.");
    }

    protected void initializeFramesGroup(Group group) {
        group.setLayout(new GridLayout(5, false));
        group.setText("Frame Selection");

        allFramesButton = new Button(group, SWT.RADIO);
        allFramesButton.setText("All Frames");
        GridData gridData = new GridData();
        gridData.horizontalSpan = 5;
        allFramesButton.setLayoutData(gridData);

        currentFramesButton = new Button(group, SWT.RADIO);
        currentFramesButton.setText("Current Frame");
        gridData = new GridData();
        gridData.horizontalSpan = 5;
        currentFramesButton.setLayoutData(gridData);

        selectedFramesButton = new Button(group, SWT.RADIO);
        selectedFramesButton.setText("Frames");
        gridData = new GridData();
        selectedFramesButton.setLayoutData(gridData);

        switch (options.getFrameSelection()) {
        case ALL:
            allFramesButton.setSelection(true);
            break;
        case CURRENT:
            currentFramesButton.setSelection(true);
            break;
        case USER:
            selectedFramesButton.setSelection(true);
        }

        new Label(group, SWT.NONE).setText("from:");
        framesFromText = new Text(group, SWT.BORDER);
        GC gc = new GC(framesFromText);
        int textWidth = gc.textExtent("99").x;
        gc.dispose();
        gridData = new GridData();
        gridData.widthHint = textWidth;
        framesFromText.setLayoutData(gridData);
        framesFromText.setEnabled(selectedFramesButton.getSelection());
        framesFromText
                .setText(String.valueOf(options.getFirstFrameIndex() + 1));
        new Label(group, SWT.NONE).setText("to:");
        framesToText = new Text(group, SWT.BORDER);
        gridData = new GridData();
        gridData.widthHint = textWidth;
        framesToText.setLayoutData(gridData);
        framesToText.setEnabled(selectedFramesButton.getSelection());
        framesToText.setText(String.valueOf(options.getLastFrameIndex() + 1));
        selectedFramesButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                framesToText.setEnabled(selectedFramesButton.getSelection());
                framesFromText.setEnabled(selectedFramesButton.getSelection());
            }

        });
    }

    protected void initializeAnimationGroup(Group group) {
        group.setLayout(new GridLayout(3, false));
        group.setText("Animation Options");
        animatedButton = new Button(group, SWT.CHECK);
        GridData gridData = new GridData();
        gridData.horizontalSpan = 3;
        animatedButton.setLayoutData(gridData);
        animatedButton.setText("Animate");
        animatedButton
                .setSelection(options.getImageFormat() == ImageFormat.ANIMATION);
        animatedButton
                .setToolTipText("Generate an animated gif, without this a new file is generated for each frame.");
        animatedButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                frameDelayText.setEnabled(animatedButton.getSelection());
                firstFrameDwellText.setEnabled(animatedButton.getSelection());
                lastFrameDwellText.setEnabled(animatedButton.getSelection());

                datetimeButton.setEnabled(!animatedButton.getSelection());
                datetimeButton.setSelection(false);
            }
        });

        new Label(group, SWT.NONE).setText("Frame Delay:");
        frameDelayText = new Text(group, SWT.BORDER);
        GC gc = new GC(frameDelayText);
        int textWidth = gc.textExtent("99.99").x;
        gc.dispose();
        gridData = new GridData(textWidth, SWT.DEFAULT);
        frameDelayText.setLayoutData(gridData);
        frameDelayText.setEnabled(animatedButton.getSelection());
        frameDelayText.setText(millisToText(options.getFrameDelay()));
        new Label(group, SWT.NONE).setText("seconds");

        new Label(group, SWT.NONE).setText("First Frame Dwell:");
        firstFrameDwellText = new Text(group, SWT.BORDER);
        gridData = new GridData(textWidth, SWT.DEFAULT);
        firstFrameDwellText.setLayoutData(gridData);
        firstFrameDwellText.setEnabled(animatedButton.getSelection());
        firstFrameDwellText.setText(millisToText(options.getFirstFrameDwell()));
        new Label(group, SWT.NONE).setText("seconds");

        new Label(group, SWT.NONE).setText("Last Frame Dwell:");
        lastFrameDwellText = new Text(group, SWT.BORDER);
        gridData = new GridData(textWidth, SWT.DEFAULT);
        lastFrameDwellText.setLayoutData(gridData);
        lastFrameDwellText.setEnabled(animatedButton.getSelection());
        lastFrameDwellText.setText(millisToText(options.getLastFrameDwell()));
        new Label(group, SWT.NONE).setText("seconds");
    }

    protected void initializeButtons(Composite comp) {
        comp.setLayout(new GridLayout(2, true));
        Button okButton = new Button(comp, SWT.PUSH);
        okButton.setText("OK");
        okButton.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        okButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                okPressed();
            }

        });

        Button cancelButton = new Button(comp, SWT.PUSH);
        cancelButton.setText("Cancel");
        cancelButton.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        cancelButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }

        });
    }

    protected String millisToText(int millis) {
        return Double.toString((double) millis / TimeUtil.MILLIS_PER_SECOND);
    }

    protected void selectDestinationFile() {
        FileDialog fileDialog = new FileDialog(this.shell, SWT.SAVE);
        File file = new File(locationText.getText());
        fileDialog.setFileName(file.getName());
        if ((file.getParentFile() != null)
                && file.getParentFile().isDirectory()) {
            fileDialog.setFilterPath(file.getParent());
        }
        int index = 0;
        String[] ext = new String[ImageFormat.values().length];
        String[] names = new String[ImageFormat.values().length];
        for (ImageFormat format : ImageFormat.values()) {
            String[] exts = format.getExtensions();
            StringBuilder filter = new StringBuilder(exts.length * 7);
            for (String suffix : exts) {
                if (!suffix.isEmpty()) {
                    if (filter.length() != 0) {
                        filter.append(';');
                    }
                    filter.append("*.").append(suffix);
                }
            }
            ext[index] = filter.toString();
            names[index] = format.getDescription();
            if (animatedButton.getSelection()
                    && (format == ImageFormat.ANIMATION)) {
                fileDialog.setFilterIndex(index);
            } else if (format != ImageFormat.ANIMATION) {
                fileDialog.setFilterIndex(index);
            }
            index += 1;
        }
        fileDialog.setFilterExtensions(ext);
        fileDialog.setFilterNames(names);
        String path = fileDialog.open();
        if (path != null) {
            this.locationText.setText(path);
        }
    }

    protected void okPressed() {
        options.setFileLocation(new File(locationText.getText()));

        if (allFramesButton.getSelection()) {
            options.setFrameSelection(FrameSelection.ALL);
        } else if (currentFramesButton.getSelection()) {
            options.setFrameSelection(FrameSelection.CURRENT);
        } else if (selectedFramesButton.getSelection()) {
            options.setFrameSelection(FrameSelection.USER);
            try {
                int from = Integer.parseInt(framesFromText.getText()) - 1;
                options.setFirstFrameIndex(from);
            } catch (NumberFormatException e) {
                invalidNumberMessage(framesFromText, "starting frame");
                return;
            }
            try {
                int to = Integer.parseInt(framesToText.getText());
                options.setLastFrameIndex(to);
            } catch (NumberFormatException e) {
                invalidNumberMessage(framesToText, "ending frame");
                return;
            }
        }

        if (animatedButton.getSelection()) {
            options.setImageFormat(ImageFormat.ANIMATION);
            try {
                double delayS = Double.parseDouble(frameDelayText.getText());
                int delay = (int) (delayS * TimeUtil.MILLIS_PER_SECOND);
                options.setFrameDelay(delay);
            } catch (NumberFormatException e) {
                invalidNumberMessage(frameDelayText, "frame delay");

                return;
            }
            try {
                double delayS = Double.parseDouble(firstFrameDwellText
                        .getText());
                int delay = (int) (delayS * TimeUtil.MILLIS_PER_SECOND);
                options.setFirstFrameDwell(delay);
            } catch (NumberFormatException e) {
                invalidNumberMessage(firstFrameDwellText, "first frame dwell");

                return;
            }
            try {
                double delayS = Double
                        .parseDouble(lastFrameDwellText.getText());
                int delay = (int) (delayS * TimeUtil.MILLIS_PER_SECOND);
                options.setLastFrameDwell(delay);
            } catch (NumberFormatException e) {
                invalidNumberMessage(lastFrameDwellText, "last frame dwell");

                return;
            }
        } else {
            options.setImageFormat(ImageFormat.SEQUENCE);
        }
        if (datetimeButton.getSelection()) {
            options.setDateTimeSelection(DateTimeSelection.DATETIME);
        }
        if (validate()) {
            setReturnValue(options);
            close();
        }
    }

    protected void invalidNumberMessage(Text text, String description) {
        MessageBox mb = new MessageBox(getShell(), SWT.ICON_ERROR | SWT.OK);
        mb.setText("Invalid Number");
        mb.setMessage(text.getText()
                + " is not a valid number, please enter a valid number for the "
                + description + ".");
        mb.open();
    }

    protected boolean validate() {
        if (options.getFrameSelection() == FrameSelection.USER) {
            String message = null;
            if ((options.getFirstFrameIndex() < options.getMinFrameIndex())) {
                message = "The first frame cannot be less than "
                        + (options.getMinFrameIndex() + 1);
            } else if ((options.getLastFrameIndex() > (options
                    .getMaxFrameIndex() + 1))) {
                message = "The last frame cannot be greater than than "
                        + (options.getMaxFrameIndex() + 1);
            } else if ((options.getFirstFrameIndex() > options
                    .getLastFrameIndex())) {
                message = "The last frame must be after the first frame";
            }
            if (message != null) {
                MessageBox mb = new MessageBox(getShell(), SWT.ICON_ERROR
                        | SWT.OK);
                mb.setText("Invalid Range");
                mb.setMessage("The frame range is invalid.\n" + message
                        + ".\nPlease enter a valid range");
                mb.open();
                return false;
            }
        }

        String path = options.getFileLocation().getAbsolutePath();
        String suffix = path.substring(path.lastIndexOf('.') + 1);

        boolean goodSuffix = false;
        for (String ext : options.getImageFormat().getExtensions()) {
            if (suffix.equals(ext)) {
                goodSuffix = true;
            }
        }
        if (!goodSuffix) {
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_WARNING
                    | SWT.YES | SWT.NO);
            mb.setText("Incompatible Format");
            ImageFormat format = options.getImageFormat();
            mb.setMessage("The format(" + suffix
                    + ") of the selected file is not valid for "
                    + format.getDescription()
                    + ". Would you like to continue and export the file in "
                    + format.getExtensions()[0] + " format?");
            int result = mb.open();
            if (result == SWT.YES) {
                suffix = format.getExtensions()[0];
                path = path.substring(0, path.lastIndexOf('.') + 1) + suffix;
                options.setFileLocation(new File(path));
            } else {
                return false;
            }
        }

        if (options.isGeoreferencable()
                && (suffix.equals("tif") || suffix.equals("tiff"))) {
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_QUESTION
                    | SWT.YES | SWT.NO | SWT.CANCEL);
            mb.setText("Convert to GeoTIFF?");
            String image = "image";
            if (options.getFrameSelection() != FrameSelection.CURRENT) {
                image = "images";
            }
            mb.setMessage("Your "
                    + image
                    + " can be converted to a GeoTIFF. This will allow "
                    + "it to be properly positioned in geospatial software. Saving as a "
                    + "GeoTIFF will reproject the image, which may introduce distortion "
                    + "and will increase file size.\n\n"
                    + "Reproject and include geospatial metadata?");
            int result = mb.open();
            if (result == SWT.CANCEL) {
                return false;
            }
            options.setGeoreference(result == SWT.YES);
        } else {
            options.setGeoreference(false);
        }

        File file = options.getFileLocation();
        if (!file.getParentFile().exists()) {
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_QUESTION
                    | SWT.YES | SWT.NO);
            mb.setText("Create Directory");
            mb.setMessage("The directory " + file.getParent()
                    + " does not exist, would you like to create it.");
            int result = mb.open();
            if (result == SWT.YES) {
                if (!file.getParentFile().mkdirs()) {
                    mb = new MessageBox(getShell(), SWT.ICON_ERROR | SWT.OK);
                    mb.setText("Error Creating Directory");
                    mb.setMessage("An unspecified error has occurred creating the directory, please select a new file location.");
                    mb.open();
                    return false;
                }
            } else {
                return false;
            }
        }

        if (file.exists()) {
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_WARNING
                    | SWT.YES | SWT.NO);
            mb.setText("Overwrite file");
            mb.setMessage("The specified file already exists. Would you like to overwrite it?");
            int result = mb.open();
            if (result == SWT.NO) {
                return false;
            }
        }
        return true;
    }
}
