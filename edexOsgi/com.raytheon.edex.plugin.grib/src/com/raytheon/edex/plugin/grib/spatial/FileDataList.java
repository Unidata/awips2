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
package com.raytheon.edex.plugin.grib.spatial;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.LocalizationFile;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 31, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "FileListings")
public class FileDataList {
    private List<FileData> coverageFileList;

    private List<FileData> subGridFileList;

    private Map<String, FileData> coverageFileMap;

    private Map<String, FileData> subGridFileMap;

    @XmlElements({ @XmlElement(name = "CoverageFiles", type = FileData.class) })
    public List<FileData> getCoverageFileList() {
        if (coverageFileList == null && coverageFileMap != null) {
            coverageFileList = new ArrayList<FileData>(coverageFileMap.values());
        }

        return coverageFileList;
    }

    public void setCoverageFileList(List<FileData> coverageFileList) {
        this.coverageFileList = coverageFileList;
        coverageFileMap = null;
    }

    @XmlElements({ @XmlElement(name = "SubGridFiles", type = FileData.class) })
    public List<FileData> getSubGridFileList() {
        if (subGridFileList == null && subGridFileMap != null) {
            subGridFileList = new ArrayList<FileData>(subGridFileMap.values());
        }
        return subGridFileList;
    }

    public void setSubGridFileList(List<FileData> subGridFileList) {
        this.subGridFileList = subGridFileList;
        subGridFileMap = null;
    }

    public Map<String, FileData> getCoverageFileMap() {
        if (coverageFileMap == null && coverageFileList != null) {
            coverageFileMap = new HashMap<String, FileData>(
                    (int) (coverageFileList.size() * 1.25f) + 1);
            for (FileData cov : coverageFileList) {
                String name = cov.getFilePath();
                int index = name.lastIndexOf(File.separatorChar);
                if (index >= 0 && index < name.length() - 1) {
                    name = name.substring(index + 1);
                }

                coverageFileMap.put(name, cov);
            }

        }
        return coverageFileMap;
    }

    public void setCoverageFileMap(Map<String, FileData> coverageFileMap) {
        this.coverageFileMap = coverageFileMap;
        coverageFileList = null;
    }

    public Map<String, FileData> getSubGridFileMap() {
        if (subGridFileMap == null && subGridFileList != null) {
            subGridFileMap = new HashMap<String, FileData>(
                    (int) (subGridFileList.size() * 1.25f) + 1);
            for (FileData cov : subGridFileList) {
                String name = cov.getFilePath();
                int index = name.lastIndexOf(File.separatorChar);
                if (index >= 0 && index < name.length() - 1) {
                    name = name.substring(index + 1);
                }

                subGridFileMap.put(name, cov);
            }

        }
        return subGridFileMap;
    }

    public void setSubGridFileMap(Map<String, FileData> subGridFileMap) {
        this.subGridFileMap = subGridFileMap;
    }

    /**
     * Adds the specific coverage files to the coverage file list. If a file
     * already exists for the same name, the first one added will be in the
     * list, so order of add is important for localization.
     * 
     * @param coverageFiles
     */
    public void addCoverageFiles(LocalizationFile[] coverageFiles) {
        if (getCoverageFileMap() == null) {
            coverageFileMap = new HashMap<String, FileData>(
                    (int) (coverageFiles.length * 1.25) + 1);
        }

        for (LocalizationFile lf : coverageFiles) {
            File f = lf.getFile();
            FileData data = new FileData(f);
            String name = f.getName();

            // localization returns lowest level first
            if (!coverageFileMap.containsKey(name)) {
                coverageFileMap.put(name, data);
            }
        }
    }

    /**
     * Adds the specific subgrid files to the subgrid file list. If a file
     * already exists for the same name, the first one added will be in the
     * list, so order of add is important for localization.
     * 
     * @param subGridFiles
     */
    public void addSubGridFiles(LocalizationFile[] subGridFiles) {
        if (getSubGridFileMap() == null) {
            subGridFileMap = new HashMap<String, FileData>(
                    (int) (subGridFiles.length * 1.25) + 1);
        }

        for (LocalizationFile lf : subGridFiles) {
            File f = lf.getFile();

            if (!subGridFileMap.containsKey(f.getName())) {
            	FileData data = new FileData(f);
            	subGridFileMap.put(f.getName(), data);
            }
        }
    }

    @Override
    public int hashCode() {
        getCoverageFileMap();
        getSubGridFileMap();
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((coverageFileMap == null) ? 0 : coverageFileMap.hashCode());
        result = prime * result
                + ((subGridFileMap == null) ? 0 : subGridFileMap.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        FileDataList other = (FileDataList) obj;
        getCoverageFileMap();
        getSubGridFileMap();
        other.getCoverageFileMap();
        other.getSubGridFileMap();
        if (coverageFileMap == null) {
            if (other.coverageFileMap != null)
                return false;
        } else if (!coverageFileMap.equals(other.coverageFileMap))
            return false;
        if (subGridFileMap == null) {
            if (other.subGridFileMap != null)
                return false;
        } else if (!subGridFileMap.equals(other.subGridFileMap))
            return false;
        return true;
    }

}
