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
package com.raytheon.uf.edex.backupsvc.service;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.backupsvc.BackupServiceFile;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.filter.LocalizationFileFilter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * File filter for localization backup service. Files are listed in backupSvc.xml.
 * backupSvc.xml contains a list of files to include in the backup, and a list
 * of files to exclude. backupSvc.xml is scanned at regular intervals in a cron job
 * through {@link BackupService.java} for changes.  
 * 
 * Files
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 8, 2016  5937       tgurney     Initial creation
 * Apr 15 2021  84655      lsingh      Combined localizationBackupList.txt
 *                                     and backupSvc.xml into one file. 
 *
 * </pre>
 *
 * @author tgurney
 */

public class LocalizationBackupFileFilter {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(LocalizationBackupFileFilter.class);
    

    private volatile LocalizationFileFilter filter;

    /**
     * Constructor.
     */
    public LocalizationBackupFileFilter() {
        filter = new LocalizationFileFilter();
        reload();


    }

    /**
     * Reload all filter lists. If any lists fail to load, fall back to an empty
     * list
     */
    public synchronized void reload() {
        LocalizationFileFilter newFilter = new LocalizationFileFilter();
        BackupServiceConfigManager configManager = BackupServiceConfigManager.getInstance();
        
            List<BackupServiceFile> includeList = configManager.getIncludeList();
            if (includeList == null) {
                includeList = new ArrayList<BackupServiceFile>();
            }
            try (InputStream is = getInputStream(includeList)) {
                newFilter.addAcceptList(is);
            } catch (IOException | LocalizationException e) {
                statusHandler.error("Failed to add include file list to Backup Service file filter.", e);
            }
            
            List<BackupServiceFile> excludeList = configManager.getExcludeList();
            if (excludeList == null) {
                excludeList = new ArrayList<BackupServiceFile>();
            }
            try (InputStream is = getInputStream(excludeList)) {
                newFilter.addRejectList(is);
            } catch (IOException | LocalizationException e) {
                statusHandler.error("Failed to add exclude file list to Backup Service file filter.", e);
            } 

        filter = newFilter;
    }
    
    /**
     * Convert BackupServiceFiles to a single input stream
     * @param fileList list of localization files
     * @return ByteArrayInputStream of files
     */
    private InputStream getInputStream(List<BackupServiceFile> fileList) {
        StringBuilder fileListString = new StringBuilder();
        
        for(BackupServiceFile bsFile : fileList) {
            if(bsFile.getLevel() == null || bsFile.getFilePath() == null) {
                statusHandler.error("Could not parse backup service file list. Either Level or File path is null. ");
                return null;
            }
            
            //use the format denoted in ILocalizationFileFilter
            fileListString.append(bsFile.getLevel() + ":" + bsFile.getFilePath());
            
            if(fileList.indexOf(bsFile) != fileList.size()-1) {
                fileListString.append(System.lineSeparator());
            }
        } 

        return new ByteArrayInputStream(fileListString.toString().getBytes());
    }

    public boolean accept(ILocalizationFile file) {
        return filter.accept(file);
    }

}
