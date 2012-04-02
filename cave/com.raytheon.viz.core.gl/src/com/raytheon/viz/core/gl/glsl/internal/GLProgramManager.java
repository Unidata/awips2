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
package com.raytheon.viz.core.gl.glsl.internal;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Loads a GL program from a file.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * May 11, 2007                     njensen             Initial Creation
 * </PRE>
 * 
 */
public class GLProgramManager {

    private static final String GLSL_FOLDER = "glsl";

    private static final String GLSL_INCLUDE_FOLDER = GLSL_FOLDER
            + IPathManager.SEPARATOR + "include";

    private static final String GLSL_EXTENSION = ".glsl";

    private static final String GLSL_HEADER_EXTENSION = ".glh";

    private static final Pattern INCLUDE_PATTERN = Pattern
            .compile("^#include\\s*<([a-zA-z0-9_]*)>.*");

    private static GLProgramManager instance;

    private final Map<String, String> includeCode = new HashMap<String, String>();

    private final Map<String, String> programsCode = new HashMap<String, String>();

    private final Map<String, File> headerFiles = new HashMap<String, File>();

    private final Map<String, File> includeFiles = new HashMap<String, File>();

    private final Map<String, File> programFiles = new HashMap<String, File>();

    private final IPathManager pm;

    private GLProgramManager() {
        pm = PathManagerFactory.getPathManager();

        // Lookup available main programs (will be in glsl directory)
        LocalizationFile[] files = pm.listStaticFiles(GLSL_FOLDER,
                new String[] { GLSL_EXTENSION }, false, true);
        for (LocalizationFile file : files) {
            File fileObj = file.getFile();
            String name = fileObj.getName();
            programFiles.put(name, fileObj);
            programsCode.put(name, readProgramContents(fileObj));
        }
    }

    public static GLProgramManager getInstance() {
        if (instance == null) {
            instance = new GLProgramManager();
        }

        return instance;
    }

    /**
     * Get program code for the program name specified, these files live in glsl
     * directory in localization.
     * 
     * @param aProgramName
     * @return
     * @throws VizException
     */
    public String getProgramCode(String aProgramName) throws VizException {
        String fileName = aProgramName + GLSL_EXTENSION;
        hasBeenModified(aProgramName);
        return programsCode.get(fileName);
    }

    /**
     * Checks if program has been modified
     * 
     * @param programName
     * @return
     */
    public boolean hasBeenModified(String programName) {
        // TODO: Check file modification time and reload contents. Should also
        // check modification times of files depends on and reload if any
        // changes
        return false;
    }

    /**
     * Reads program contends for file, will recursively load all #includes as
     * well
     * 
     * @param aFile
     * @return
     */
    private String readProgramContents(File aFile) {
        try {
            StringBuffer buffer = new StringBuffer();

            List<String> toInclude = new ArrayList<String>();
            getIncludes(aFile, toInclude);

            for (String include : toInclude) {
                // look for header file first
                File header = getIncludeFile(include, true);
                if (header != null) {
                    String name = header.getName();
                    String contents = includeCode.get(name);
                    if (contents == null) {
                        contents = readFileContents(header);
                        includeCode.put(name, contents);
                    }
                    buffer.append(contents);
                }
            }

            for (String include : toInclude) {
                // look for glsl files next
                File header = getIncludeFile(include, false);
                if (header != null) {
                    String name = header.getName();
                    String contents = includeCode.get(name);
                    if (contents == null) {
                        contents = readFileContents(header);
                        includeCode.put(name, contents);
                    }
                    buffer.append(contents);
                }
            }

            // Read program file contents
            buffer.append(readFileContents(aFile));
            return buffer.toString();
        } catch (IOException e) {
            UFStatus.getHandler().handle(Priority.PROBLEM,
                    "Error reading glsl program code from file system", e);
        }
        return null;
    }

    /**
     * Read file contents off file system
     * 
     * @param file
     * @param includeCode
     * @return
     * @throws IOException
     */
    private String readFileContents(File file) throws IOException {
        StringBuffer buffer = new StringBuffer();
        BufferedReader reader = new BufferedReader(new FileReader(file));

        String line = null;
        while ((line = reader.readLine()) != null) {
            Matcher match = INCLUDE_PATTERN.matcher(line);
            if (match.find() == false) {
                buffer.append(line).append("\n");
            }
        }

        return buffer.toString();
    }

    private void getIncludes(File file, List<String> toInclude)
            throws IOException {
        List<String> newIncludes = new ArrayList<String>();
        BufferedReader reader = new BufferedReader(new FileReader(file));
        String line = null;
        while ((line = reader.readLine()) != null) {
            Matcher match = INCLUDE_PATTERN.matcher(line);
            if (match.find()) {
                String include = match.group(1);
                if (toInclude.contains(include) == false
                        && newIncludes.contains(include) == false) {
                    newIncludes.add(include);
                    toInclude.add(include);
                }
            }
        }

        reader.close();

        // Recursively get all include files
        for (String newInclude : newIncludes) {
            getIncludes(getIncludeFile(newInclude, false), toInclude);
        }
    }

    private File getIncludeFile(String name, boolean header) {
        String fileName = name
                + (header ? GLSL_HEADER_EXTENSION : GLSL_EXTENSION);

        Map<String, File> toUse = null;
        if (header) {
            toUse = headerFiles;
        } else {
            toUse = includeFiles;
        }

        File file = toUse.get(name);
        if (file == null) {
            file = pm.getStaticFile(GLSL_INCLUDE_FOLDER
                    + IPathManager.SEPARATOR + fileName);
            toUse.put(name, file);
        }

        return file;
    }
}
