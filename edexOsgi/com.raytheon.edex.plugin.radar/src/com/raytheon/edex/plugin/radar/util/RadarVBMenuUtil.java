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
package com.raytheon.edex.plugin.radar.util;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.xml.bind.JAXBException;

import com.raytheon.uf.common.dataplugin.radar.util.RadarsInUseUtil;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.menus.vb.VbSource;
import com.raytheon.uf.common.menus.vb.VbSourceList;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.menus.AbstractMenuUtil;

/**
 * Radar VB Sources Menu Creator
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 26, 2021  8576     randerso  Initial creation. Generate radar vbsources
 *                                  based on local radars as defined in
 *                                  radarsInUse.txt
 *
 * </pre>
 *
 * @author randerso
 */

public class RadarVBMenuUtil extends AbstractMenuUtil {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarVBMenuUtil.class);

    private static final String RADAR_VBSOURCES_FILE = LocalizationUtil
            .join(VbSourceList.VB_SOURCE_DIR, "radar.xml");

    private LocalizationContext caveConfigured = pm.getContext(
            LocalizationType.CAVE_STATIC, LocalizationLevel.CONFIGURED);

    @Override
    public void createMenus() {
        /*
         * retrieve the local radars from radarsInUse.txt
         */
        RadarsInUseUtil.setParsed(false);
        List<String> localRadars = RadarsInUseUtil.getSite(getSite(),
                RadarsInUseUtil.LOCAL_CONSTANT);

        List<VbSource> radarSources = new ArrayList<>();
        if (!localRadars.isEmpty()) {
            for (int i = localRadars.size() - 1; i >= 0; i--) {
                String icao = localRadars.get(i).toLowerCase();

                /* add VbSource */
                VbSource source = new VbSource();
                source.setKey("radar-" + icao);
                source.setName("Radar-" + icao.toUpperCase());
                source.setCategory("Volume/Radar");
                radarSources.add(source);
            }
        }

        /* Save or remove radar VbSources file */
        ILocalizationFile radarVbSourcesFile = pm
                .getLocalizationFile(caveConfigured, RADAR_VBSOURCES_FILE);
        if (radarSources.isEmpty()) {
            try {
                radarVbSourcesFile.delete();
            } catch (LocalizationException e) {
                statusHandler.error("Error removing " + radarVbSourcesFile, e);
            }
        } else {
            VbSourceList vbSourceList = new VbSourceList();
            vbSourceList.setEntries(radarSources);

            try (SaveableOutputStream out = radarVbSourcesFile
                    .openOutputStream()) {
                JAXBManager jaxb = new SingleTypeJAXBManager<>(
                        VbSourceList.class);
                jaxb.marshalToStream(vbSourceList, out);
                out.save();
            } catch (IOException | LocalizationException e) {
                statusHandler.error("Error writing " + radarVbSourcesFile, e);
            } catch (SerializationException | JAXBException e) {
                statusHandler.error("Error serializing radar VbSources to "
                        + radarVbSourcesFile, e);
            }
        }
    }

    @Override
    protected boolean checkCreated() {
        String type = "radar";
        String fileName = "radarsInUse.txt";

        LocalizationContext context = pm
                .getContextForSite(LocalizationType.COMMON_STATIC, getSite());
        ILocalizationFile lFile = pm.getLocalizationFile(context,
                LocalizationUtil.join(type, fileName));
        Date useTime = lFile.getTimeStamp();

        ILocalizationFile vbSources = pm.getLocalizationFile(caveConfigured,
                RADAR_VBSOURCES_FILE);
        boolean status = vbSources.exists()
                && !vbSources.getTimeStamp().before(useTime);

        return status;
    }

}
