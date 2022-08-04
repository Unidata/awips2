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
package com.raytheon.viz.pointdata.def;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import org.eclipse.swt.graphics.RGB;

/**
 * RgbAdapter object
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- -----------   --------------------------
 * 10/10/2019   71272      Mark Peters   Initial Creation
 *
 * </pre>
 *
 * @author mpeters
 */

public class RgbAdapter extends XmlAdapter<int[], RGB> {

    @Override
    public RGB unmarshal(int[] rgbArray) throws Exception {
        if (rgbArray == null) {
            return null;
        }
        return new RGB(rgbArray[0], rgbArray[1], rgbArray[2]);
    }

    @Override
    public int[] marshal(RGB rgb) throws Exception {
        if (rgb == null) {
            return null;
        }
        return new int[] { rgb.red, rgb.green, rgb.blue };
    }

}
