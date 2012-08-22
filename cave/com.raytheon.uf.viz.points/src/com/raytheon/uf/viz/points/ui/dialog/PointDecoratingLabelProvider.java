/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package com.raytheon.uf.viz.points.ui.dialog;

import org.eclipse.jface.viewers.DecoratingLabelProvider;
import org.eclipse.jface.viewers.ILabelDecorator;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Image;

/**
 * Class to properly display the columns of a maker node.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 31, 2012 #875       rferrel     Initial creation.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class PointDecoratingLabelProvider extends DecoratingLabelProvider
        implements ITableLabelProvider {

    ITableLabelProvider provider;

    ILabelDecorator decorator;

    public PointDecoratingLabelProvider(ILabelProvider provider,
            ILabelDecorator decorator) {
        super(provider, decorator);
        this.provider = (ITableLabelProvider) provider;
        this.decorator = decorator;
    }

    @Override
    public Image getColumnImage(Object element, int columnIndex) {
        Image image = provider.getColumnImage(element, columnIndex);
        if (decorator != null) {
            Image decorated = decorator.decorateImage(image, element);
            if (decorated != null) {
                return decorated;
            }
        }
        return image;
    }

    @Override
    public String getColumnText(Object element, int columnIndex) {
        String text = provider.getColumnText(element, columnIndex);
        if (decorator != null) {
            String decorated = decorator.decorateText(text, element);
            if (decorated != null) {
                return decorated;
            }
        }
        return text;
    }
}
