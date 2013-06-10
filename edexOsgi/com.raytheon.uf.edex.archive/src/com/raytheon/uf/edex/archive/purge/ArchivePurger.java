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
package com.raytheon.uf.edex.archive.purge;

import java.util.List;

import com.raytheon.uf.common.archive.ArchiveManagerFactory;
import com.raytheon.uf.common.archive.IArchive;
import com.raytheon.uf.common.archive.IArchiveElement;
import com.raytheon.uf.common.archive.IArchiveManager;
import com.raytheon.uf.common.archive.exception.ArchiveException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Purge task to purge archived data based on configured expiration.
 * 
 * TODO This is a purge that will run on a timer scheduled in spring properties
 * that is not related to the expiration date of data. Another possible solution
 * would be to programatically schedule a timer when an archive is created based
 * on the expiration date of the archive.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 6, 2013            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class ArchivePurger {
    private static final IUFStatusHandler logger = UFStatus
            .getHandler(ArchivePurger.class);

    /**
     * Purge expired elements from the archives.
     */
    public void purge() {
        IArchiveManager manager = ArchiveManagerFactory.getManager();
        List<IArchive> archives = manager.getArchives();
        for (IArchive archive : archives) {
            // TODO fix...
            for (IArchiveElement element : archive.getExpiredElements(null,
                    null)) {
                try {
                    if (!element.purge()) {
                        String elementName = element == null ? "null" : element
                                .getName();
                        logger.error("ArchivePurger unable to purge element "
                                + elementName);
                    }
                } catch (ArchiveException e) {
                    String elementName = element == null ? "null" : element
                            .getName();
                    logger.error("ArchivePurger unable to purge element "
                            + elementName, e);
                }
            }
        }
    }

}
