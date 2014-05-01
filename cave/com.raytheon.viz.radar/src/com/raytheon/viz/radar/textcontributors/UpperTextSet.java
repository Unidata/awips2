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
package com.raytheon.viz.radar.textcontributors;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * A set of all the upper texts read in from an xml file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 3, 2010            bsteffen    Initial creation
 * Oct 24, 2013  2491     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "upperTextSet")
public class UpperTextSet {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(UpperTextSet.class);

    @XmlElement(name = "upperText")
    protected List<UpperText> list = new ArrayList<UpperText>();

    private static Map<Integer, List<IRadarTextContributor>> map;

    public static List<IRadarTextContributor> getContributors(int productCode) {
        if (map == null) {
            map = new HashMap<Integer, List<IRadarTextContributor>>();
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            File file = pathMgr.getStaticFile("styleRules/RadarUpperText.xml");
            UpperTextSet set = null;
            try {
                set = JAXB.unmarshal(file, UpperTextSet.class);
            } catch (RuntimeException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error Occured Retrieving Text Contributions", e);
                return null;
            }
            for (UpperText contribution : set.list) {
                for (Integer code : contribution.codes) {
                    map.put(code, contribution.lines);
                }
            }
        }
        return map.get(productCode);
    }

}
