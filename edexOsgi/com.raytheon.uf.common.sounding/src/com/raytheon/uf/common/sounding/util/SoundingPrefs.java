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
package com.raytheon.uf.common.sounding.util;

import java.io.File;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 15, 2011            bsteffen     Initial creation
 *
 * </pre>
 *
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class SoundingPrefs {
    private static final transient IUFStatusHandler statusHandler = UFStatus
    .getHandler(SoundingPrefs.class);
    
    private static final String SOUNDING_PREFS_FILE = "sounding/soundingPrefs.xml";
    
    @XmlElement
    private double temperatureOffset = 0.0;

    public double getTemperatureOffset() {
        return temperatureOffset;
    }

    public void setTemperatureOffset(double temperatureOffset) {
        this.temperatureOffset = temperatureOffset;
    }
    
    private static SoundingPrefs soundingPrefs;
    
    public static SoundingPrefs getSoundingPrefs() {
        if(soundingPrefs == null) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            File file = pathMgr.getFile(lc, SOUNDING_PREFS_FILE);
            if(file == null || !file.exists()) {
                 lc = pathMgr.getContext(
                        LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
                 file = pathMgr.getFile(lc, SOUNDING_PREFS_FILE);
            }
            try {
                soundingPrefs = (SoundingPrefs) SerializationUtil.jaxbUnmarshalFromXmlFile(file);
            } catch (SerializationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
                soundingPrefs = new SoundingPrefs();
            }
        }
        return soundingPrefs;
                
    }
    
}
