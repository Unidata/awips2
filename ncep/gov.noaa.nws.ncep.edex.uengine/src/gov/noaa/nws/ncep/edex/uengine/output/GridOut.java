package gov.noaa.nws.ncep.edex.uengine.output;

import gov.noaa.nws.ncep.edex.uengine.utility.GempakConvert;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.UUID;

import com.raytheon.edex.uengine.exception.MicroEngineException;
import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.edex.core.EDEXUtil;

public class GridOut extends ScriptTask {

    private String format;

    private URI uri;

    private float[] data;

    private boolean ignoreDefaultDataDir = false;

    private String destFile = null;

    private String destDir = null;

    private int nx;

    private int ny;

    /**
     * Constructor
     * 
     * @param aFile
     *            the file in bytes to write out
     * @param aFormat
     *            the format of the file
     */
    public GridOut(Object aFile, String aFormat, String aNx, String aNy) {
        // public GridOut(Object aFile, String aFormat) {
        data = (float[]) aFile;
        format = aFormat;
        nx = Integer.parseInt(aNx);
        ny = Integer.parseInt(aNy);
        init();
    }

    public GridOut(float[] aFile, String aFormat, URI aProductUri) {
        data = aFile;
        format = aFormat;
        uri = aProductUri;
        init();
    }

    private void init() {

        /*
         * make sure there is an output directory defined
         */
        if (StringUtil.isEmptyString(destDir)) {
            // default to uengineOutDir
            destDir = EDEXUtil.getEdexData() + File.separator + "uEngine";
            // logger.debug("Defaulting destDir to: " + destDir);
        } else {
            // If the ignore default data dir flag is not true then
            // prepend the default data dir to the destination directory.
            if (!ignoreDefaultDataDir) {
                destDir = EDEXUtil.getEdexData() + File.separator + destDir;
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
        if (uri != null && !StringUtil.isEmptyString(uri.toString())) {
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
            GempakConvert.data2File(data, nx, ny, outFile);
            // System.out.println("calling seialize2File");
            // GempakConvert.serialize2File(data, outFile);
        } catch (IOException e) {
            throw new MicroEngineException("Error writing bytes to file.", e);
        }

        return outFile.toURI();
    }

    public float[] getData() {
        return data;
    }

    public void setData(float[] aData) {
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
