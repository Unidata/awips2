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

package com.raytheon.uf.edex.ndm.dataplugin.subscriber;

import com.raytheon.edex.plugin.poessounding.decoder.POESSoundingDataAdapter;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * Poes-sounding BUFR subscriber for National Dataset Subscriber sounding data.
 * Produces 'poesBufr.spi' in configured OAX folder:
 * (..common_static/configured/${siteID}/basemaps) when station-list text file
 * or 'poesBufr.spi' is copied to ndm drop folder: ('/awips2/edex/data/ndm').
 * 
 * For example, if site is OAX, 'poesBufr.spi' is produced in:
 * ..common_static/configured/OAX/basemaps
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- -------------------------------------------------------------------------------------------------
 * Mar 23, 2016 5495       jschmid     Initial creation: trigger broadcast of update to cluster on creation of localized (.spi) output.
 * 
 * </pre>
 * 
 * @author jschmid
 * @version 1.0
 */

public class PoesBufrSoundingSubscriber extends AbstractBufrSoundingSubscriber
        implements ILocalizationPathObserver {

    private final String SPI_FILE = "basemaps" + IPathManager.SEPARATOR
            + "poesBufr.spi";

    public PoesBufrSoundingSubscriber() {
        super();
        PathManager pathMgr = (PathManager) PathManagerFactory.getPathManager();
        pathMgr.addLocalizationPathObserver(SPI_FILE, this);
    }

    @Override
    public void fileChanged(ILocalizationFile file) {
        POESSoundingDataAdapter.updateSPIFile();
    }
}