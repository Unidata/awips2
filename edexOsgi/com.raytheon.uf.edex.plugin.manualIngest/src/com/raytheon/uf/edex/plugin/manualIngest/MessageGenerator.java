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
package com.raytheon.uf.edex.plugin.manualIngest;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;
import org.springframework.util.FileCopyUtils;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

/**
 * A bean based on FileToString that will take a message generated from a file
 * endpoint and attempt to search for a WMO header inside the first 100 bytes.
 * If found, that string is set to the message header and passed on. If it is
 * not found, it will use the filename.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 28, 2009            brockwoo     Initial creation
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */

public class MessageGenerator implements Processor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MessageGenerator.class);

    private static String DIR = PropertiesFactory.getInstance()
            .getEnvProperties().getEnvValue("ARCHIVEDIR")
            + File.separator + "manual";

    private static Pattern WMOPATTERN = Pattern
            .compile("([A-Z]{3}[A-Z0-9](\\d{0,2}|[A-Z]{0,2}) [A-Z0-9 ]{4} "
                    + "\\d{6}[^\\r\\n]*)[\\r\\n]+");

    private static MessageGenerator instance = new MessageGenerator();

    private String ingestRoute = null;

    public static MessageGenerator getInstance() {
        return instance;
    }

    public String getIngestRoute() {
        return ingestRoute;
    }

    public void setIngestRoute(String ingestRoute) {
        this.ingestRoute = ingestRoute;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.camel.Processor#process(org.apache.camel.Exchange)
     */
    @Override
    public void process(Exchange arg0) throws Exception {

        byte[] header = new byte[100];

        File file = (File) arg0.getIn().getBody();

        if (file != null) {
            String fileName = file.getName();

            InputStream is = null;
            try {
                is = new FileInputStream(file);
                is.read(header);
                String sHeader = new String(header, "ISO-8859-1");
                Matcher wmoSearch = WMOPATTERN.matcher(sHeader);
                String messageHeader = fileName;

                if (wmoSearch.find()) {
                    messageHeader = wmoSearch.group();
                }

                arg0.getIn().setBody(file.toString());
                arg0.getIn().setHeader("header", messageHeader);
                arg0.getIn().setHeader("enqueueTime",
                        System.currentTimeMillis());
            } finally {
                if (is != null) {
                    try {
                        is.close();
                    } catch (IOException e) {
                        // ignore
                    }
                }
            }
        } else {
            // No file received
            arg0.getOut().setFault(true);
        }
    }

    public File copyFileToArchive(File inFile) {
        String path = DIR + File.separator;

        // Determine the sub-directory
        String inputPath = inFile.getParent();
        
        // Split on the manual directory to get the sub-directory
        String[] parts = inputPath.split("manual");
        File dir = null;
        if (parts.length > 1) {
            dir = new File(path + parts[1]);
        } else {
            dir = new File(path);
        }
        
        if (!dir.exists()) {
            dir.mkdirs();
        }

        File newFile = new File(dir, inFile.getName());

        try {
            FileCopyUtils.copy(inFile, newFile);
            statusHandler.handle(Priority.INFO, "DataManual: " + inFile.getAbsolutePath());
        } catch (IOException e) {
            statusHandler.handle(Priority.ERROR, "Failed to copy file ["
                    + inFile.getAbsolutePath() + "] to archive dir", e);
            return null;
        }

        return newFile;
    }

    public File moveFileToArchive(File inFile) {
        File newFile = copyFileToArchive(inFile);
        if (newFile != null) {
            inFile.delete();
        }
        return newFile;
    }

    public boolean sendFileToIngest(String inFile) {
        return sendFileToIngest(inFile, ingestRoute);
    }

    public boolean sendFileToIngest(String inFile, String route) {
        boolean rval = true;

        try {
            File archiveFile = copyFileToArchive(new File(inFile));
            EDEXUtil.getMessageProducer().sendAsync(route,
                    archiveFile.getAbsolutePath());
        } catch (EdexException e) {
            rval = false;
            statusHandler.handle(Priority.ERROR, "Failed to insert file ["
                    + inFile + "] into ingest stream", e);
        }

        return rval;
    }
}
