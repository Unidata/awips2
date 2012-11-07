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
package com.raytheon.viz.ui.presenter.components;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

/**
 * Base class for the widget configuration classes.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 26, 2012   223      mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */

public abstract class WidgetConf {
    public static final Runnable DO_NOTHING = new Runnable() {
        @Override
        public void run() {
        }
    };

    protected String toolTipText;

    public WidgetConf(String toolTipText) {
        this.toolTipText = toolTipText;
    }

    /**
     * @return the toolTipText
     */
    public String getToolTipText() {
        return toolTipText;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(toolTipText);

        return builder.toHashCode();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof WidgetConf) {
            WidgetConf other = (WidgetConf) obj;

            EqualsBuilder builder = new EqualsBuilder();
            builder.append(toolTipText, other.toolTipText);

            return builder.isEquals();
        }
        return super.equals(obj);
    }
}
