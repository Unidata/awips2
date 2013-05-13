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
package com.raytheon.uf.common.archive.manager;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.archive.config.ArchiveConfig;
import com.raytheon.uf.common.archive.config.CategoryConfig;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.localization.LocalizationManager;

/**
 * Manager for access to archive data information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 1, 2013  1966       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class ArchiveDataManager {
    private final static ArchiveDataManager instance = new ArchiveDataManager();

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ArchiveDataManager.class);

    private final String ARCHIVE_DIR = "archive";

    private String site;

    private IPathManager pathMgr;

    private LocalizationContext siteCtx;

    private LocalizationFile archiveDir;

    private final Map<String, ArchiveConfig> archiveMap = new HashMap<String, ArchiveConfig>();

    public final static ArchiveDataManager getInstance() {
        return instance;
    }

    private ArchiveDataManager() {
        site = LocalizationManager.getInstance().getCurrentSite();
        pathMgr = PathManagerFactory.getPathManager();

        siteCtx = pathMgr.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);

        archiveDir = pathMgr.getLocalizationFile(siteCtx, ARCHIVE_DIR);
    }

    /**
     * Get list of site's archive data configuration files.
     * 
     * @return archiveDataList
     */
    private LocalizationFile[] getArchiveDataFiles() {
        String path = archiveDir.getName().trim();
        LocalizationFile[] files = pathMgr.listFiles(siteCtx, path,
                new String[] { "*.xml" }, false, true);
        return files;
    }

    public String[] getArchiveDataNamesList() throws IOException,
            LocalizationException {
        if (archiveMap.size() == 0) {
            LocalizationFile[] files = getArchiveDataFiles();
            for (LocalizationFile lFile : files) {
                ArchiveConfig archiveData = unmarshalArhiveDataFromXmlFile(lFile);
                archiveMap.put(archiveData.getName(), archiveData);
            }
        }
        String[] names = archiveMap.keySet().toArray(new String[0]);
        Arrays.sort(names, 0, names.length, String.CASE_INSENSITIVE_ORDER);

        return names;
    }

    public String[] getCategoryNames(ArchiveConfig archiveData) {
        List<CategoryConfig> categories = archiveData.getCategoryList();
        List<String> nameList = new ArrayList<String>(categories.size());
        for (CategoryConfig category : archiveData.getCategoryList()) {
            String name = category.getName();
            nameList.add(name);
        }
        String[] names = nameList.toArray(new String[0]);
        Arrays.sort(names, 0, names.length, String.CASE_INSENSITIVE_ORDER);
        return names;
    }

    public ArchiveConfig loadArchiveData(LocalizationFile lFile)
            throws IOException, LocalizationException {
        return unmarshalArhiveDataFromXmlFile(lFile);
    }

    public void saveArchiveData(LocalizationFile lFile, ArchiveConfig archiveData)
            throws IOException, LocalizationException {
        marshalArchiveDataToXmlFile(archiveData, lFile);
    }

    private void marshalArchiveDataToXmlFile(ArchiveConfig archiveData,
            LocalizationFile lFile) throws IOException, LocalizationException {
        OutputStream stream = null;
        try {
            stream = lFile.openOutputStream();
            JAXB.marshal(archiveData, stream);
        } finally {
            if (stream != null) {
                try {
                    stream.close();
                } catch (IOException ex) {
                    // ignore
                }
            }
        }
    }

    private ArchiveConfig unmarshalArhiveDataFromXmlFile(LocalizationFile lFile)
            throws IOException, LocalizationException {
        ArchiveConfig archiveData = null;
        InputStream stream = null;
        try {
            stream = lFile.openInputStream();
            archiveData = JAXB.unmarshal(stream, ArchiveConfig.class);
        } finally {
            if (stream != null) {
                try {
                    stream.close();
                } catch (IOException ex) {
                    // ignore
                }
            }
        }
        return archiveData;
    }

    // TODO for testing to be removed
    private void marshalArchiveDataToXmlFile(ArchiveConfig category, File file)
            throws IOException, LocalizationException {
        // TODO use lFile
        OutputStream stream = null;
        try {
            stream = new BufferedOutputStream(new FileOutputStream(file));
            JAXB.marshal(category, stream);
        } finally {
            if (stream != null) {
                try {
                    stream.close();
                } catch (IOException ex) {
                    // ignore
                }
            }
        }
    }

    // TODO for testing to be removed
    private ArchiveConfig unmarshalArhiveDataFromXmlFile(File file)
            throws IOException, LocalizationException {
        ArchiveConfig archiveData = null;
        InputStream stream = null;
        try {
            // stream = new BufferedInputStream(new FileInputStream(new File(
            // "/home/rferrel/Desktop/RAW_DATA.xml")));
            stream = new BufferedInputStream(new FileInputStream(file));
            archiveData = JAXB.unmarshal(stream, ArchiveConfig.class);
        } finally {
            if (stream != null) {
                try {
                    stream.close();
                } catch (IOException ex) {
                    // ignore
                }
            }
        }
        return archiveData;
    }

    // TODO remove main
    public static void main(String[] args) {
        ArchiveConfig rawArchiveData = new ArchiveConfig();
        rawArchiveData.setName("Raw");
        rawArchiveData.setRootDir("/data_store/");
        rawArchiveData.setRetentionHours(7 * 24);
        List<CategoryConfig> scList = new ArrayList<CategoryConfig>();
        CategoryConfig category = new CategoryConfig();
        category.setName("Model grib");
        category.setDirPattern("grib/[^/]*/[^/]*/(.*\\)");
        category.setDisplay("\\1");
        category.setSaveDir("grib/${YYYY}${MM}${DD}/${HH}/(.*)");
        category.setSaveFiles(".*");
        category.addSelectList("grib/${YYYY}${MM}${DD}/${HH}/NCEP_QPF/");
        scList.add(category);

        category = new CategoryConfig();
        category.setName("Model grib2");
        category.setDirPattern("grib2/[^/]*/[^/]*/(.*\\)");
        category.setDisplay("\\1");
        category.setSaveDir("grib2/${YYYY}${MM}${DD}/${HH}/.*");
        category.setSaveFiles(".*");
        scList.add(category);

        category = new CategoryConfig();
        category.setName("Model other");
        category.setDirPattern("(acars|airmet|binlighting|bufrmos|bufrua|bufsigwx|convsigmet|goessndg|manual|mdlsndg|poessndg|profiler|shef|text)/[^/]*/[^/]*/(.*)");
        category.setDisplay("\\1 - \\2");
        category.setSaveDir("(acars|airmet|binlighting|bufrmos|bufrua|bufsigwx|convsigmet|goessndg|manual|mdlsndg|poessndg|profiler|shef|text)/YYYYMMDD/HH/.*");
        category.setSaveFiles(".*");
        scList.add(category);

        category = new CategoryConfig();
        category.setName("Observations");
        category.setDirPattern("(aircraft|metar|sfcobs)/[^/]*/[^/]*/(.*)");
        category.setDisplay("\\1 - \\2");
        category.setSaveDir("(aircraft|metar|sfcobs)/${YYYY}${MM}${DD}/${HH}/");
        category.setSaveFiles(".*");
        category.addSelectList("metar/)${YYYY}${MM}${DD}/${HH}/");

        category = new CategoryConfig();
        category.setName("Satellilte");
        category.setRetentionHours(4 * 24 + 5);
        category.setDirPattern("sat/[^/]*/[^/]*/(.*)");
        category.setDisplay("\\1");
        category.setSaveDir("sat/${YYYY}${MM}${DD}/${HH}/.*");
        category.setSaveFiles(".*");
        scList.add(category);

        rawArchiveData.setCategoryList(scList);

        ArchiveConfig procArchiveData = new ArchiveConfig();
        procArchiveData.setName("Processed");
        procArchiveData.setRootDir("/awips2/edex/data/archive/");
        procArchiveData.setRetentionHours(7 * 24);
        scList = new ArrayList<CategoryConfig>();
        category = new CategoryConfig();
        category.setName("redbook");
        category.setDirPattern("hdf5/redbook/([^/]*)/");
        category.setDisplay("redbook - \\1");
        category.setSaveDir("hdf5/redbook/([^/]*)/");
        category.setSaveFiles("redbook-${YYYY}-${MM}-${DD}-${HH}\\..*");
        scList.add(category);

        category = new CategoryConfig();
        category.setName("obs");
        category.setDirPattern("hdf5/(obs)/");
        category.setSaveDir("hdf5/(obs)/");
        category.setDisplay("\\1");
        category.setSaveFiles("metar-${YYYY}-${MM}-${DD}-${HH}\\.*");
        scList.add(category);
        procArchiveData.setCategoryList(scList);

        try {
            System.out.println("marshal raw archiveData: "
                    + rawArchiveData.toString());
            File rawFile = new File("/home/rferrel/Desktop/RAW_DATA.xml");
            instance.marshalArchiveDataToXmlFile(rawArchiveData, rawFile);
            ArchiveConfig umCategory = instance
                    .unmarshalArhiveDataFromXmlFile(rawFile);
            System.out.println("unmarshal raw archiveData: "
                    + umCategory.toString());
            System.out.println("marshal processed archiveData: "
                    + rawArchiveData.toString());
            File procFile = new File("/home/rferrel/Desktop/PROCESSED_DATA.xml");
            instance.marshalArchiveDataToXmlFile(procArchiveData, procFile);
            umCategory = instance.unmarshalArhiveDataFromXmlFile(procFile);
            System.out.println("unmarshal processed archiveData: "
                    + umCategory.toString());
        } catch (IOException e) {
            instance.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        } catch (LocalizationException e) {
            instance.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }
}
