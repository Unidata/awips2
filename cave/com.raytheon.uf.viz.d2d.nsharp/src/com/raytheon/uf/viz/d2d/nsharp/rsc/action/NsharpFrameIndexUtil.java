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
package com.raytheon.uf.viz.d2d.nsharp.rsc.action;

import gov.noaa.nws.ncep.ui.nsharp.NsharpSoundingElementStateProperty;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;

import java.util.List;

import com.raytheon.uf.viz.core.drawables.IFrameCoordinator.FrameChangeMode;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator.FrameChangeOperation;

/**
 * Provide convenience methods for treating nsharp frames as an indexable
 * one-dimensional list.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 4, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class NsharpFrameIndexUtil {
/* Chin
    public static int getFrameCount(NsharpResourceHandler handler){
        int count = 0;
        for (List<NsharpSoundingElementStateProperty> list : handler.getStnTimeTable()) {
            count += list.size();
        }
        return count;
    }
    
    public static int getCurrentIndex(NsharpResourceHandler handler) {
        int index = 0;

        for (List<NsharpSoundingElementStateProperty> list : handler
                .getStnTimeTable()) {
            for (NsharpSoundingElementStateProperty element : list) {
                if (element.getElementDescription().equals(
                        handler.getCurSndProfileProp().getElementDescription())) {
                    return index;
                }
                index += 1;
            }
        }
        return 0;
    }

    public static void setCurrentIndex(NsharpResourceHandler handler, int index) {
        NsharpSoundingElementStateProperty selected = null;
        int i = 0;
        for (List<NsharpSoundingElementStateProperty> list : handler
                .getStnTimeTable()) {
            for (NsharpSoundingElementStateProperty element : list) {
                if (i == index) {
                    selected = element;
                    break;
                }
                i += 1;
            }
            if (selected != null) {
                break;
            }
        }
        if (selected == null) {
            return;
        }
        NsharpSoundingElementStateProperty current = handler
                .getCurSndProfileProp();
        while (!current.getStnDescription()
                .equals(selected.getStnDescription())) {
            handler.setSteppingStnIdList(FrameChangeOperation.NEXT);
            current = handler.getCurSndProfileProp();
        }
        while (!current.getElementDescription().equals(
                selected.getElementDescription())) {
            handler.setSteppingTimeLine(FrameChangeOperation.NEXT,
                    FrameChangeMode.TIME_ONLY);
            current = handler.getCurSndProfileProp();
        }
    }
*/
}
