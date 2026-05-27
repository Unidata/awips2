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
package com.raytheon.uf.edex.plugin.ffmp.common;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SourceType;
import com.raytheon.uf.common.monitor.xml.FFTISettingXML;
import com.raytheon.uf.common.monitor.xml.FFTISourceXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;

/**
 * Checks to see if FFTI is applicable and keeps the FFTI Source and FFTI
 * Attributes. Logic extracted from FFMPProcessor.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 05, 2018 6560       njensen     Initial creation
 * Aug 14, 2018 6720       njensen     Use simplified enums
 *
 * </pre>
 *
 * @author njensen
 */

public class FFTIChecker {

    private FFTISourceXML fftiSource = null;

    /** attributes are Accum, Ratio, and Diff */
    private List<String> fftiAttribute = new ArrayList<>();

    public FFTIChecker(List<FFTISettingXML> settingList, SourceXML source,
            String siteKey) {
        if (settingList != null && !settingList.isEmpty()) {
            for (FFTISettingXML setting : settingList) {
                for (String settingDispName : setting
                        .getSettingDisplayNames()) {
                    /*
                     * FIXME A setting can have multiple display names. But this
                     * object can only have one fftiSource and that's determined
                     * by the setting display name matching. So the
                     * FFTISourceXML that gets used will be the one
                     * corresponding to the last setting display name.
                     */

                    /*
                     * FFTI is still using display names so we will use it here
                     * too. It also then matches the names shown on the FFTI
                     * GUI.
                     */
                    String sourceDispName = source.getDisplayName();
                    SourceType sourceType = source.getSourceType();

                    /*
                     * settingDispName is something like koax-DHR, RFCFFG,
                     * QPFSCAN, or Bias HPE. sourceDispName is something like
                     * DHR, RFCFFG, QPFSCAN, or Bias HPE. Therefore this if
                     * block is most likely to apply to guidance or QPF. See the
                     * FFTI GUI for a better visualization.
                     */
                    if (settingDispName.equals(sourceDispName)) {
                        fftiAttribute
                                .add(setting.getAttribute().getAttributeName());
                        if (sourceType == SourceType.QPE) {
                            fftiSource = setting.getQpeSource();
                        } else if (sourceType == SourceType.QPF) {
                            fftiSource = setting.getQpfSource();
                        } else {
                            fftiSource = setting.getGuidSource();
                        }
                    }

                    /*
                     * settingDispName is something like koax-DHR, RFCFFG,
                     * QPFSCAN, or Bias HPE. sourceDispName is something like
                     * DHR, RFCFFG, QPFSCAN, or Bias HPE. Therefore this if
                     * block is most likely to apply to QPE. See the FFTI GUI
                     * for a better visualization.
                     */
                    String[] settingKey = settingDispName.split("-");
                    if (settingKey.length == 2) {
                        if (settingKey[0].equals(siteKey)
                                && settingKey[1].equals(sourceDispName)) {
                            if (sourceType == SourceType.QPE) {
                                fftiSource = setting.getQpeSource();
                                fftiAttribute.add(setting.getAttribute()
                                        .getAttributeName());
                            } else if (sourceType == SourceType.QPF) {
                                fftiSource = setting.getQpfSource();
                                fftiAttribute.add(setting.getAttribute()
                                        .getAttributeName());
                            }
                        }
                    }

                    /*
                     * TODO There's an example setting in FFTI GUI for
                     * mrms-MRMS-QPE related to source MRMS with displayName
                     * MRMS-QPE in FFMPSourceConfig.xml. That doesn't work since
                     * the split would have length 3. Is that a valid
                     * configuration/example?
                     */
                }
            }
        }

    }

    /**
     * is this source an FFTI source
     * 
     * @return
     */
    public boolean isFFTI() {
        return fftiSource != null;
    }

    /**
     * Gets that FFTI source
     * 
     * @return
     */
    public FFTISourceXML getFFTISource() {
        return fftiSource;
    }

    /**
     * Returns the FFTI attributes for this source
     * 
     * @return
     */
    public List<String> getAttributes() {
        return fftiAttribute;
    }

}
