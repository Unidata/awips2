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
package com.raytheon.viz.texteditor.qc;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.adapters.XmlAdapter;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 6, 2011  10764      rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class BulletAdapter extends
        XmlAdapter<BulletElements[], Map<String, List<String>>> {

    @Override
    public Map<String, List<String>> unmarshal(BulletElements[] v)
            throws Exception {
        Map<String, List<String>> qcMap = new HashMap<String, List<String>>();
        for (BulletElements element : v) {
            qcMap.put(element.key, element.value);
        }
        return qcMap;
    }

    @Override
    public BulletElements[] marshal(Map<String, List<String>> v)
            throws Exception {
        BulletElements[] bulletElements = new BulletElements[v.size()];
        int index = 0;
        for (Map.Entry<String, List<String>> element : v.entrySet()) {
            bulletElements[index] = new BulletElements(element.getKey(),
                    element.getValue());
        }
        // TODO Auto-generated method stub
        return bulletElements;
    }

}
