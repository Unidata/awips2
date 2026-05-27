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
package com.raytheon.uf.common.backupsvc;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlValue;

/**
 * Wrapper for files listed in backupSvc.xml that are 
 * intended for backup.
 * 
 * Format:
 * <file level="localization_level"> localization_type/subpath/to/file2</file>
 * 
 * Example: 
 * <file level="site">common_static/afos2awips/*</file>
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 14, 2021  84655      lsingh      Initial creation.
 *
 * </pre>
 *
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class BackupServiceFile {
    
    /**
     * Localization File path
     */
    @XmlValue
    private String filePath;
    
    /**
     * Localization level (SITE, USER, etc,.)
     */
    @XmlAttribute
    private String level;

    public String getFilePath() {
        return filePath;
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    public String getLevel() {
        return level;
    }

    public void setLevel(String level) {
        this.level = level;
    }
    
    @Override
    public String toString() {
        return level + ":" + filePath;
    }
}
