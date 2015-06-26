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
package com.raytheon.uf.viz.alertview.action;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.ui.PlatformUI;
import org.osgi.framework.Bundle;
import org.osgi.framework.FrameworkUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.viz.alertview.Alert;
import com.raytheon.uf.viz.alertview.Alert.Priority;
import com.raytheon.uf.viz.alertview.ui.view.AlertView;

/**
 * 
 * Action for writing an alert to a file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 24, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class SaveToFileAction extends Action {

    private static final Charset UTF8 = Charset.forName("UTF-8");

    private static final SimpleDateFormat TIME_FORMAT = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss,SSS");

    /** Don't waste memory keeping excessive details. */
    private static final int MAX_DETAILS_SIZE = 16 * 1024;

    private static Logger logger = LoggerFactory.getLogger(AlertView.class);

    private final Alert alert;

    private final Path path;

    public SaveToFileAction(Alert alert) {
        super("Save to file...");
        this.alert = alert;
        this.path = null;
    }

    public SaveToFileAction(Alert alert, Path path) {
        super("Save to file...");
        this.alert = alert;
        this.path = path;
    }

    @Override
    public void run() {
        Path path = this.path;
        if (path == null) {
            FileDialog fd = new FileDialog(PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getShell(), SWT.SAVE);
            fd.setFileName("alert.txt");
            fd.setFilterExtensions(new String[] { "*.txt" });
            String fileName = fd.open();
            if (fileName != null) {
                path = Paths.get(fileName);
            } else {
                return;
            }
        }
        try {
            write(path, alert);
        } catch (IOException e) {
            logger.error("Unable to save alert", e);
        }
    }

    public static void write(Path path, Alert alert) throws IOException {
        Bundle bundle = FrameworkUtil.getBundle(SaveToFileAction.class);
        try (BufferedWriter writer = Files.newBufferedWriter(path, UTF8)) {
            writer.write("# Written by " + bundle.getSymbolicName() + " "
                    + bundle.getVersion() + "\n");
            writer.write(alert.getPriority().toString());
            writer.write(" ");
            writer.write(TIME_FORMAT.format(alert.getTime()));
            writer.write(" ");
            writer.write(alert.getMessage());
            writer.write("\n");
            writer.write(alert.getDetails());
        }
    }

    public static Alert read(Path path) throws IOException {
        byte[] bytes = Files.readAllBytes(path);
        String content = new String(bytes, UTF8);
        String first = "#";
        String details = content;
        while (first.startsWith("#")) {
            String[] split = details.split("\n", 2);
            if (split.length != 2) {
                throw new IOException(path.toString()
                        + " is not a valid alert file");
            }
            first = split[0];
            details = split[1];
        }
        String[] split = first.split(" ", 4);
        if (split.length != 4) {
            throw new IOException(path.toString()
                    + " is not a valid alert file");
        }
        Priority priority;
        try {
            priority = Priority.valueOf(split[0]);
        } catch (IllegalArgumentException e) {
            throw new IOException(split[0] + " is not a valid priority.");
        }
        Date time;
        try {
            time = TIME_FORMAT.parse(split[1] + " " + split[2]);
        } catch (ParseException e) {
            throw new IOException(path.toString()
                    + " is not a valid alert file", e);
        }
        String message = split[3];
        if (details.length() > MAX_DETAILS_SIZE) {
            details = details.substring(0, MAX_DETAILS_SIZE)
                    + "\n...file content truncated...";
        }
        return new ParsedAlert(priority, time, path.toString(), message,
                details);
    }

    private static class ParsedAlert implements Alert {

        private final Priority priority;

        private final Date time;

        private final String origin;

        private final String message;

        private final String details;

        public ParsedAlert(Priority priority, Date time, String origin,
                String message, String details) {
            this.priority = priority;
            this.time = time;
            this.origin = origin;
            this.message = message;
            this.details = details;
        }

        @Override
        public Date getTime() {
            return time;
        }

        @Override
        public Priority getPriority() {
            return priority;
        }

        @Override
        public String getOrigin() {
            return origin;
        }

        @Override
        public String getMessage() {
            return message;
        }

        @Override
        public String getDetails() {
            return details;
        }

    }
}
