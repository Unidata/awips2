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
 * Configuration object for org.eclipse.swt.widgets.List objects.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 12, 2012   223      mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */

public class ListConf extends WidgetConf {
    private String[] items;

    private boolean singleSelect = true;

    private final int width;

    private final int height;

    public ListConf(String toolTipText, String[] items, boolean singleSelect, int width, int height) {
        super(toolTipText);

        this.items = items;
        this.singleSelect = singleSelect;
        this.width = width;
        this.height = height;
    }

    /**
     * @param items the items to set
     */
    public void setItems(String[] items) {
        this.items = items;
    }

    /**
     * @return the items
     */
    public String[] getItems() {
        return items;
    }

    /**
     * @param singleSelect the singleSelect to set
     */
    public void setSingleSelect(boolean singleSelect) {
        this.singleSelect = singleSelect;
    }

    /**
     * @return the singleSelect
     */
    public boolean isSingleSelect() {
        return singleSelect;
    }

    /**
     * @return the width
     */
    public int getWidth() {
        return width;
    }

    /**
     * @return the height
     */
    public int getHeight() {
        return height;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(items);
        builder.append(singleSelect);
        builder.append(width);
        builder.append(height);
        builder.appendSuper(super.hashCode());

        return builder.toHashCode();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof CheckBoxConf) {
            ListConf other = (ListConf) obj;

            EqualsBuilder builder = new EqualsBuilder();
            builder.append(items, other.getItems());
            builder.append(singleSelect, other.isSingleSelect());
            builder.append(width, other.getWidth());
            builder.append(height, other.getHeight());
            builder.appendSuper(super.equals(obj));

            return builder.isEquals();
        }
        return super.equals(obj);
    }
}
