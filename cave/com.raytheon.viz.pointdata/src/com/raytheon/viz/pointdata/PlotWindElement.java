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
package com.raytheon.viz.pointdata;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- -----------   --------------------------
 * 10/31/2019   71272      Mark Peters   Initial Creation
 *
 * </pre>
 *
 * @author mapeters
 */

/*
 * TODO may be good to have some PlotModelGuiElement subclass that has all the
 * setters, which is just used by the GUIs then...also maybe some interface, and
 * this is specifically the SVG implementation?
 */
public class PlotWindElement {
    public Node barbNode = null;

    public Element barbElement = null;

    public Node arrowNode = null;

    public Element arrowElement = null;

    public Node gustNode = null;

    public Element gustElement = null;

    public String gustX = null;

    public String gustY = null;
}