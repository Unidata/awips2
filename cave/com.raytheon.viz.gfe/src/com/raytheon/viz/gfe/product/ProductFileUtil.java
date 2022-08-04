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
package com.raytheon.viz.gfe.product;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.core.mode.CAVEMode;

/**
 * File utilities for text products.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11 Feb 2010  4132       ryu         Initial creation
 * Nov 12, 2015 4834       njensen     Changed LocalizationOpFailedException to LocalizationException
 * Feb 05, 2016 5242       dgilling    Remove calls to deprecated Localization APIs.
 * Apr 07, 2016 5559       dgilling    Fix directory creation bug in writeFile.
 * 
 * </pre>
 * 
 * @author ryu
 * @version 1.0
 * 
 */

public class ProductFileUtil {

    static public LocalizationFile getLocalizationFile(String path) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext siteContext = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        path.replace('/', File.separatorChar);
        return pathMgr.getLocalizationFile(siteContext, path);
    }

    static public LocalizationFile getProductFile(String pil, boolean practice) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext siteContext = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        String path = "gfe/products/";
        if (practice) {
            path += "practice/";
        }
        path.replace('/', File.separatorChar);
        String timePortion = new SimpleDateFormat("yyyyMMdd_HHmmss_")
                .format(SimulatedTime.getSystemTime().getTime());
        path += timePortion + pil;
        LocalizationFile file = pathMgr.getLocalizationFile(siteContext, path);
        return file;
    }

    static public LocalizationFile getLastProductFile(String pil,
            boolean practice) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext siteContext = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        String path = "gfe/products/";
        if (practice) {
            path += "practice/";
        }
        path.replace('/', File.separatorChar);
        LocalizationFile[] files = pathMgr.listFiles(siteContext, path, null,
                false, true);
        if (files.length == 0) {
            return null;
        }
        ArrayList<String> names = new ArrayList<String>();
        String regex = "2\\d{7}_\\d{6}_" + pil;
        for (LocalizationFile f : files) {
            String fname = LocalizationUtil.extractName(f.getPath());
            if (fname.matches(regex)) {
                names.add(fname);
            }
        }
        if (names.size() == 0) {
            return null;
        }
        Collections.sort(names);
        String fname = names.get(names.size() - 1);
        path += fname;
        LocalizationFile file = pathMgr.getLocalizationFile(siteContext, path);
        return file;
    }

    static public LocalizationFile getDraftFile(String productId) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext siteContext = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        String path = FileUtil.join("gfe", "drafts", productId);
        String mode = "Standard";
        if (CAVEMode.getMode().equals(CAVEMode.PRACTICE)) {
            mode = "PRACTICE";
        }
        path = path + "-" + mode;

        LocalizationFile file = pathMgr.getLocalizationFile(siteContext, path);
        return file;
    }

    static public void writeFile(String text, File file) throws IOException {
        Path filePath = file.toPath();
        Files.createDirectories(filePath.getParent());

        try (Writer out = Files.newBufferedWriter(filePath,
                StandardCharsets.UTF_8)) {
            writeFile(text, out);
        }
    }

    static public String readFile(File file) throws IOException {
        try (BufferedReader in = Files.newBufferedReader(file.toPath(),
                StandardCharsets.UTF_8)) {
            return readFile(in);
        }
    }

    static public void writeFile(String text, LocalizationFile localizationFile)
            throws IOException, LocalizationException {
        try (SaveableOutputStream outStream = localizationFile
                .openOutputStream();
                Writer out = new BufferedWriter(new OutputStreamWriter(
                        outStream))) {
            writeFile(text, out);
            out.close();
            outStream.save();
        }
    }

    static public String readFile(LocalizationFile localizationFile)
            throws IOException, LocalizationException {
        try (InputStream inStream = localizationFile.openInputStream();
                BufferedReader in = new BufferedReader(new InputStreamReader(
                        inStream))) {
            return readFile(in);
        }
    }

    private static String readFile(BufferedReader reader) throws IOException {
        StringBuilder contents = new StringBuilder();
        String line;
        while ((line = reader.readLine()) != null) {
            contents.append(line);
            contents.append(System.getProperty("line.separator"));
        }

        return contents.toString();
    }

    private static void writeFile(String text, Writer writer)
            throws IOException {
        writer.write(text);
    }
}
