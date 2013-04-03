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
 * GUI implementation independent representation of a button.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2012    223     mpduff      Initial creation.
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */

public class ButtonConf extends LabeledWidgetConf {
    /**
     * Button enabled flag;
     */
    private final boolean enabled;

    /**
     * Event handler for the button.
     */
    private final Runnable onClickAction;

    /**
     * Constructor.
     *
     * @param enabled
     * @param displayText
     * @param toolTipText
     * @param onClickAction
     */
    public ButtonConf(boolean enabled, String displayText, String toolTipText,
            Runnable onClickAction) {
        super(displayText, toolTipText);

        this.enabled = enabled;
        this.onClickAction = onClickAction;
    }

    /**
     * @return the enabled
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * @return the onClickAction
     */
    public Runnable getOnClickAction() {
        return onClickAction;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(enabled);
        builder.append(onClickAction);
        builder.appendSuper(super.hashCode());

        return builder.toHashCode();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof ButtonConf) {
            ButtonConf other = (ButtonConf) obj;

            EqualsBuilder builder = new EqualsBuilder();
            builder.append(enabled, other.enabled);
            builder.append(onClickAction, other.onClickAction);
            builder.appendSuper(super.equals(obj));

            return builder.isEquals();
        }
        return super.equals(obj);
    }
}
