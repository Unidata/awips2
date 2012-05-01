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
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
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

    private static GLProgramManager instance;

    private final Map<String, String> programsCode = new HashMap<String, String>();

    private Map<String, File> fileMap = new HashMap<String, File>();

    private Map<File, Long> modificationMap = new HashMap<File, Long>();

    private GLProgramManager() {

    }

    public static GLProgramManager getInstance() {
        if (instance == null) {
            instance = new GLProgramManager();
        }

        return instance;
    }

    public String getProgramCode(String aProgramName) throws VizException {
        String program = programsCode.get(aProgramName);
        String searchPath = "glsl" + File.separator + aProgramName + ".glsl";
        if (hasBeenModified(searchPath, true) || program == null) {
            File file = fileMap.get(searchPath);
            modificationMap.put(file, file.lastModified());
            try {
                program = readFile(file);
                programsCode.put(aProgramName, program);
            } catch (IOException e) {
                throw new VizException("Error loading program code for: "
                        + aProgramName, e);
            }
        }

        return program;
    }

    public boolean hasBeenModified(String searchPath, boolean path) {
        if (searchPath == null) {
            return false;
        }

        if (!path) {
            searchPath = "glsl" + File.separator + searchPath + ".glsl";
        }

        File file = fileMap.get(searchPath);
        if (file == null) {
            IPathManager pm = PathManagerFactory.getPathManager();
            file = pm.getStaticFile(searchPath);
            fileMap.put(searchPath, file);
        }

        if (file.exists() == false) {
            return false;
        }

        Long lastMod = modificationMap.get(file);
        if (lastMod == null || file.lastModified() > lastMod) {
            return true;
        }
        return false;
    }

    private String readFile(File aFile) throws IOException {
        StringBuffer buffer = new StringBuffer();
        FileReader fr = new FileReader(aFile);
        BufferedReader reader = new BufferedReader(fr);
        String line = "";
        while (line != null) {
            buffer.append(line);
            buffer.append("\n");
            line = reader.readLine();
        }

        reader.close();
        fr.close();

        String text = buffer.toString();
        return text;
    }

}
