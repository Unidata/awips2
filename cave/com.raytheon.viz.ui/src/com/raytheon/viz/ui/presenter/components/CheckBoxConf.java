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
 * GUI implementation independent representation of a checkbox.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 15, 2012 0743       djohnson     Initial creation.
 * Aug 29, 2012 0223       mpduff       Extend WidgetConf.
 *
 * </pre>
 *
 * @author djohnson
 * @version 1.0
 */
public class CheckBoxConf extends LabeledWidgetConf {
    private final boolean initiallyChecked;

    private final Runnable onCheckedChangeAction;

    public CheckBoxConf(String displayText, boolean initiallyChecked,
            String toolTipText, Runnable onCheckedChangeAction) {
        super(displayText, toolTipText);

        this.initiallyChecked = initiallyChecked;
        this.onCheckedChangeAction = onCheckedChangeAction;
    }

    /**
     * @return the initiallyChecked
     */
    public boolean isInitiallyChecked() {
        return initiallyChecked;
    }

    /**
     * @return the onCheckedChangeAction
     */
    public Runnable getOnCheckedChangeAction() {
        return onCheckedChangeAction;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(initiallyChecked);
        builder.append(onCheckedChangeAction);
        builder.appendSuper(super.hashCode());

        return builder.toHashCode();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof CheckBoxConf) {
            CheckBoxConf other = (CheckBoxConf) obj;

            EqualsBuilder builder = new EqualsBuilder();
            builder.append(initiallyChecked, other.initiallyChecked);
            builder.append(onCheckedChangeAction, other.onCheckedChangeAction);
            builder.appendSuper(super.equals(obj));

            return builder.isEquals();
        }
        return super.equals(obj);
    }
}
