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

package com.raytheon.edex.uengine.tasks.output;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.UUID;

import com.raytheon.edex.uengine.exception.MicroEngineException;
import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.props.EnvProperties;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

/**
 * FileOut task derived from original FileOut uEngine task. Writes the data out
 * of memory and into a file.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Mar 29, 2007                     njensen             Initial Creation
 * </PRE>
 * 
 */
public class FileOut extends ScriptTask {
    private String format;

    private URI uri;

    private byte[] data;

    private boolean ignoreDefaultDataDir = false;

    private String destFile = null;

    private String destDir = null;

    /**
     * Constructor
     * 
     * @param aFile
     *            the file in bytes to write out
     * @param aFormat
     *            the format of the file
     */
    public FileOut(Object aFile, String aFormat) {
        data = (byte[]) aFile;
        format = aFormat;
        init();
    }

    public FileOut(byte[] aFile, String aFormat, URI aProductUri) {
        data = aFile;
        format = aFormat;
        uri = aProductUri;
        init();
    }

    private void init() {
        EnvProperties envProperties = PropertiesFactory.getInstance()
                .getEnvProperties();

        String uengineOutDir = envProperties.getEnvValue("UENGINEOUTDIR");
        String defaultDataDir = envProperties.getEnvValue("DEFAULTDATADIR");

        /*
         * make sure there is an output directory defined
         */
        if (Util.isEmptyString(destDir)) {
            // default to uengineOutDir
            destDir = uengineOutDir;
            logger.debug("Defaulting destDir to: " + destDir);
        } else {
            // If the ignore default data dir flag is not true then
            // prepend the default data dir to the destination directory.
            if (!ignoreDefaultDataDir) {
                destDir = defaultDataDir + destDir;
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.js.tasks.ScriptTask#execute()
     */
    @Override
    public Object execute() {
        if (format.contains("contours-")) {
            format = format.substring(9);
        }

        // get the URI for the image out
        String suffix = "." + format;
        if (uri != null && !Util.isEmptyString(uri.toString())) {
            destFile = new File(uri).getName();
            if (!destFile.endsWith(suffix)) {
                destFile += suffix;
            }
        } else {
            destFile = UUID.randomUUID().toString() + suffix;
        }

        // create a new file
        File outFile = new File(destDir, destFile);
        uri = outFile.toURI();

        // get the canonical path
        try {
            outFile = outFile.getCanonicalFile();
        } catch (IOException e) {
            throw new MicroEngineException("Error getting file path.", e);
        }

        // write the data to the file
        try {
            FileUtil.bytes2File(data, outFile);
        } catch (IOException e) {
            throw new MicroEngineException("Error writing bytes to file.", e);
        }

        return outFile.toURI();
    }

    public byte[] getData() {
        return data;
    }

    public void setData(byte[] aData) {
        data = aData;
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String aFormat) {
        format = aFormat;
    }

    public boolean isIgnoreDefaultDataDir() {
        return ignoreDefaultDataDir;
    }

    public void setIgnoreDefaultDataDir(boolean aIgnoreDefaultDataDir) {
        ignoreDefaultDataDir = aIgnoreDefaultDataDir;
    }

    public URI getUri() {
        return uri;
    }

    public void setUri(URI aUri) {
        uri = aUri;
    }

}
