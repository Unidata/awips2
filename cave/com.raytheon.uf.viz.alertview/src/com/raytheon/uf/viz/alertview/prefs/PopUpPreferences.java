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
package com.raytheon.uf.viz.alertview.prefs;

import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.viz.alertview.ui.popup.AlertPopup;

/**
 * Contains the preferences that control how {@link AlertPopup} appears to the
 * user.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 17, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlRootElement
public class PopUpPreferences {

    public static enum PopUpCorner {
        UPPER_LEFT, UPPER_RIGHT, LOWER_LEFT, LOWER_RIGHT;
    }

    private String filter = "warnPlus";

    private int duration = 3;

    private PopUpCorner corner = PopUpCorner.LOWER_RIGHT;

    private int width = 500;

    private int height = 50;

    public String getFilter() {
        return filter;
    }

    public void setFilter(String filter) {
        this.filter = filter;
    }

    public int getDuration() {
        return duration;
    }

    public void setDuration(int duration) {
        this.duration = duration;
    }

    public PopUpCorner getCorner() {
        return corner;
    }

    public void setCorner(PopUpCorner corner) {
        this.corner = corner;
    }

    public int getWidth() {
        return width;
    }

    public void setWidth(int width) {
        this.width = width;
    }

    public int getHeight() {
        return height;
    }

    public void setHeight(int height) {
        this.height = height;
    }

}
