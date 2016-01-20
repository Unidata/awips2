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

package com.raytheon.uf.viz.image.export.handler;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageTypeSpecifier;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.metadata.IIOInvalidTreeException;
import javax.imageio.metadata.IIOMetadata;
import javax.imageio.metadata.IIOMetadataNode;
import javax.imageio.stream.FileImageOutputStream;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.handlers.HandlerUtil;
import org.w3c.dom.Node;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.image.export.dialog.ImageExportDialog;
import com.raytheon.uf.viz.image.export.options.ImageExportOptions;
import com.raytheon.uf.viz.image.export.options.ImageExportOptions.DateTimeSelection;
import com.raytheon.uf.viz.image.export.options.ImageExportOptions.FrameSelection;
import com.raytheon.uf.viz.image.export.options.ImageExportOptions.ImageFormat;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Save the current screen to an image file
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 26, 2006           chammack    Initial Creation.
 * Jan 20, 2014  2312     bsteffen    Move to image export plugin, animation.
 * Dec 4, 2014   DR16713  jgerth      Support for date and time in file name
 * Jan 18, 2016  ----     mjames@ucar Append yyyy.MM.dd.HH.mm.ss to image filename
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ExportImageHandler extends AbstractImageCaptureHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ExportImageHandler.class);

    private static final String DATE_TIME_FORMAT = "yyyyMMdd_HHmmss";

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IEditorPart part = EditorUtil.getActiveEditor();
        AbstractEditor editor = null;
        if (part instanceof AbstractEditor) {
            editor = (AbstractEditor) part;
        }

        if (editor == null) {
            return null;
        }

        Shell shell = HandlerUtil.getActiveShell(event);

        ImageExportOptions options = new ImageExportOptions();
        options.populate(editor);
        String frameSelection = event.getParameter("frameSelection");
        if ("current".equalsIgnoreCase(frameSelection)) {
            options.setFrameSelection(FrameSelection.CURRENT);
        } else if ("all".equalsIgnoreCase(frameSelection)) {
            options.setFrameSelection(FrameSelection.ALL);
        }

        String dialogType = event.getParameter("dialogType");
        if ("file".equalsIgnoreCase(dialogType)) {
            FileDialog fileDialog = new FileDialog(shell, SWT.SAVE);
            File file = options.getFileLocation();
            fileDialog.setFileName(file.getName());
            if (file.getParentFile() != null
                    && file.getParentFile().isDirectory()) {
                fileDialog.setFilterPath(file.getParent());
            }
            ImageFormat format = options.getImageFormat();
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
            fileDialog.setFilterExtensions(new String[] { filter.toString() });
            fileDialog.setFilterNames(new String[] { format.getDescription() });
            String path = fileDialog.open();
            if (path == null) {
                return null;
            }

            options.setFileLocation(new File(path));

        } else {
            if (openOptionsDialog(shell, options) == false) {
                return null;
            }
        }

        LinkedHashMap<DataTime, BufferedImage> dtbiHash = new LinkedHashMap<DataTime, BufferedImage>();
        try {
            switch (options.getFrameSelection()) {
            case CURRENT:
                dtbiHash = captureCurrentFrames(editor);
                break;
            case ALL:
                dtbiHash = captureAllFrames(editor);
                break;
            case USER:
                dtbiHash = captureFrames(editor, options.getFirstFrameIndex(),
                        options.getLastFrameIndex());
                break;
            }
        } catch (VizException e) {
            String reason = "Error occurred while capturing images";
            statusHandler.handle(Priority.PROBLEM, reason, e);
            return null;
        }

        if (!dtbiHash.isEmpty()) {
            new SaveImageJob(options, dtbiHash);
        }
        return null;
    }

    /**
     * Open an image dialog
     * 
     * @return false if the user canceled, true otherwise.
     */
    protected boolean openOptionsDialog(Shell shell, ImageExportOptions options) {
        ImageExportDialog dialog = new ImageExportDialog(shell, options);
        return dialog.open() != null;
    }

    public void saveImages(IProgressMonitor monitor,
            ImageExportOptions options, Map<DataTime, BufferedImage> dtbiHash) {
        monitor.beginTask("Saving Images", dtbiHash.size());

        String path = options.getFileLocation().getAbsolutePath();
        String ppath = path;
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_TIME_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        NumberFormat twoDigit = new DecimalFormat("00");

        String suffix = path.substring(path.lastIndexOf('.') + 1);
        String basePath = path.substring(0, path.lastIndexOf('.'));
        Iterator<ImageWriter> iter = ImageIO.getImageWritersBySuffix(suffix);
        if (!iter.hasNext()) {
            String reason = "Unsupported filetype: \"" + suffix + "\"";
            statusHandler.handle(Priority.PROBLEM, reason);
            return;
        }
        ImageWriter writer = iter.next();

        FileImageOutputStream stream = null;
        try {
            if (options.getImageFormat() == ImageFormat.SEQUENCE) {
                int i = 0;
                for (Map.Entry<DataTime, BufferedImage> entry : dtbiHash.entrySet()) {
                    i++;
                    BufferedImage bi = entry.getValue();
                    String timeStamp = new SimpleDateFormat("yyyy.MM.dd.HH.mm.ss").format(new Date()); 
                    if (options.getDateTimeSelection() == DateTimeSelection.DATETIME) {
                        DataTime key = entry.getKey();
                        Date validTime = key.getValidTimeAsDate();
                        if (validTime != null && !isFakeTime(key)) {
                            path = basePath + "-" + sdf.format(validTime) + "-" + timeStamp + "." + suffix;
                            if (path.equals(ppath)) {
                                path = basePath + "-" + sdf.format(validTime) + "-" + twoDigit.format(i).toString() + "-" + timeStamp + "." + suffix;
                            }
                        } else {
                            path = basePath + "-" + twoDigit.format(i).toString() + "-" + timeStamp + "." + suffix;
                        }
                    } else if (dtbiHash.size() > 1) {
                        path = basePath + "-" + twoDigit.format(i).toString() + "-" + timeStamp + "." + suffix;
                    } else {
                        path = basePath + "-" + timeStamp + "." + suffix;
                    }
                    ppath = path;
                    stream = new FileImageOutputStream(new File(path));
                    writer.setOutput(stream);
                    writer.write(bi);
                    stream.close();
                    stream = null;
                    if (monitor.isCanceled()) {
                        dtbiHash.clear();
                        break;
                    }
                    monitor.worked(1);
                }
                dtbiHash.clear();
            } else if (options.getImageFormat() == ImageFormat.ANIMATION) {
                stream = new FileImageOutputStream(options.getFileLocation());
                writer.setOutput(stream);
                writer.prepareWriteSequence(null);
                List<BufferedImage> images = new ArrayList<BufferedImage>();
                images.addAll(dtbiHash.values());
                dtbiHash.clear();
                for (int i = 0; i < images.size(); i++) {
                    BufferedImage bi = images.get(i);
                    /* Allow GC to collect image after write. */
                    images.set(i, null);
                    ImageWriteParam iwp = writer.getDefaultWriteParam();
                    IIOMetadata metadata = writer.getDefaultImageMetadata(
                            new ImageTypeSpecifier(bi), iwp);
                    if (i == 0) {
                        configureAnimation(metadata,
                                options.getFirstFrameDwell(), true);
                    } else if (i == images.size() - 1) {
                        configureAnimation(metadata,
                                options.getLastFrameDwell(), false);
                    } else {
                        configureAnimation(metadata, options.getFrameDelay(),
                                false);
                    }
                    IIOImage ii = new IIOImage(bi, null, metadata);
                    writer.writeToSequence(ii, null);
                    monitor.worked(1);
                    if (monitor.isCanceled()) {
                        break;
                    }
                }

                writer.endWriteSequence();
                stream.close();
                stream = null;
            } else {
                String reason = "Unrecognized format "
                        + String.valueOf(options.getImageFormat());
                statusHandler.handle(Priority.PROBLEM, reason);
            }
        } catch (IOException e) {
            String reason = "Error occurred while writing image";
            statusHandler.handle(Priority.PROBLEM, reason, e);
        } finally {
            if (stream != null) {
                try {
                    stream.close();
                } catch (IOException e) {
                    /* Don't mask existing exception. */
                }
            }
        }
    }

    private void configureAnimation(IIOMetadata metadata, int delayMillis,
            boolean first) throws IIOInvalidTreeException {
        int delayCentis = delayMillis / 10;
        String metaFormat = metadata.getNativeMetadataFormatName();

        if (!"javax_imageio_gif_image_1.0".equals(metaFormat)) {
            throw new IllegalArgumentException(
                    "Unfamiliar gif metadata format: " + metaFormat);
        }

        Node root = metadata.getAsTree(metaFormat);

        /* find the GraphicControlExtension node */
        Node child = root.getFirstChild();
        while (child != null) {
            if ("GraphicControlExtension".equals(child.getNodeName())) {
                break;
            }
            child = child.getNextSibling();
        }

        IIOMetadataNode gce = (IIOMetadataNode) child;
        gce.setAttribute("userDelay", "FALSE");
        gce.setAttribute("delayTime", String.valueOf(delayCentis));

        /* only the first node needs the ApplicationExtensions node */
        if (first) {
            IIOMetadataNode aes = new IIOMetadataNode("ApplicationExtensions");
            IIOMetadataNode ae = new IIOMetadataNode("ApplicationExtension");
            ae.setAttribute("applicationID", "NETSCAPE");
            ae.setAttribute("authenticationCode", "2.0");
            /*
             * last two bytes is an unsigned short (little endian) that
             * indicates the the number of times to loop. 0 means loop forever.
             */
            byte[] uo = new byte[] { 0x1, 0x0, 0x0 };
            ae.setUserObject(uo);
            aes.appendChild(ae);
            root.appendChild(aes);
        }

        metadata.setFromTree(metaFormat, root);

    }

    /**
     * There may be cases in which a valid time is not associated with a frame.
     * In such cases, a valid time is set to a number of milliseconds from the
     * Epoch time based on the frame number.  Here we check if that is one of
     * those such cases.
     *
     * @param a DataTime
     * @return true if the DataTime is close to the Epoch time
     */
    public boolean isFakeTime(DataTime dt) {
        return dt.getValidTime().getTimeInMillis() < 1000;
    }

    protected class SaveImageJob extends Job {

        protected final ImageExportOptions options;

        protected final LinkedHashMap<DataTime, BufferedImage> dtbiHash;

        protected SaveImageJob(ImageExportOptions options,
                LinkedHashMap<DataTime, BufferedImage> dtbiHash) {
            super("Saving image");
            this.options = options;
            this.dtbiHash = dtbiHash;
            this.schedule();
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            saveImages(monitor, options, dtbiHash);
            return Status.OK_STATUS;
        }

    }
}
