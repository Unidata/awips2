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
 * GUI implementation independent representation of a combo box.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 16, 2012 0743       djohnson     Initial creation
 * Aug 29, 2012 0223       mpduff       Extend WidgetConf.
 * Sep 07, 2012 1102       djohnson     Add missing getter for toolTipText.
 *
 * </pre>
 *
 * @author djohnson
 * @version 1.0
 */

public class ComboBoxConf extends WidgetConf {
    public static final String SELECT_ONE = "Select One";

    public static final String NONE_AVAILABLE = "None Available";

    private final boolean readOnly;

    private final Runnable onSelectionAction;

    /**
     * Constructor setting up configuration.
     *
     * @param readOnly
     *            whether or not the combo box should be read only
     * @param toolTipText
     *            the tool tip text
     * @param onSelectionAction
     *            on selection action
     */
    public ComboBoxConf(boolean readOnly, String toolTipText,
            Runnable onSelectionAction) {
        super(toolTipText);
        this.readOnly = readOnly;
        this.onSelectionAction = onSelectionAction;
    }

    /**
     * @return the readOnly
     */
    public boolean isReadOnly() {
        return readOnly;
    }

    /**
     * @return the onSelectionAction
     */
    public Runnable getOnSelectionAction() {
        return onSelectionAction;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(readOnly);
        builder.append(onSelectionAction);
        builder.appendSuper(super.hashCode());

        return builder.toHashCode();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof ComboBoxConf) {
            ComboBoxConf other = (ComboBoxConf) obj;

            EqualsBuilder builder = new EqualsBuilder();
            builder.append(readOnly, other.readOnly);
            builder.append(onSelectionAction, other.onSelectionAction);
            builder.appendSuper(super.equals(obj));

            return builder.isEquals();
        }
        return super.equals(obj);
    }
}
