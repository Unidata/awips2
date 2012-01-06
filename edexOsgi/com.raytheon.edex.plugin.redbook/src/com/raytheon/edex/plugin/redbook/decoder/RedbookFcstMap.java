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
package com.raytheon.edex.plugin.redbook.decoder;

import java.io.File;
import java.util.HashMap;
import java.util.Properties;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Add fcsttime
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	-----------	-----------	--------------------------
 * 20101022            6424 kshrestha	Add fcsttime
 * 
 * </pre>
 * 
 * @author kshrestha
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class RedbookFcstMap implements ISerializableObject {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RedbookFcstMap.class);

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class MapFcstHr {
        @XmlElement(required = false)
        public String name;

        @XmlElement
        public String fcstHR;

        @XmlAttribute(name = "prd")
        public Integer binPeriod;

        @XmlAttribute(name = "ofs")
        public Integer binOffset;
    }

    public HashMap<String, MapFcstHr> mapping;

    public static File xmlFile;

    public static Properties filesXml = new Properties();

    public static RedbookFcstMap load() throws Exception {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pathMgr.getContext(
                LocalizationContext.LocalizationType.EDEX_STATIC,
                LocalizationContext.LocalizationLevel.BASE);
        try {
            xmlFile = pathMgr.getFile(ctx, "redbook/redbookFcstMap.xml");

            RedbookFcstMap map = (RedbookFcstMap) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(xmlFile.getAbsolutePath());
            return map;
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
            throw e;
        }
    }
}
