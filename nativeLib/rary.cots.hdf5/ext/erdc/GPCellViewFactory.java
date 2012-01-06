/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF Java Products. The full HDF Java copyright       *
 * notice, including terms governing use, modification, and redistribution,  *
 * is contained in the file, COPYING.  COPYING can be found at the root of   *
 * the source code distribution tree. You can also access it online  at      *
 * http://www.hdfgroup.org/products/licenses.html.  If you do not have       *
 * access to the file, you may request a copy from help@hdfgroup.org.        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

package ext.erdc;

import java.util.Map;
import org.jgraph.graph.DefaultCellViewFactory;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.VertexView;

/**
 * A default view factory for a JGraph. This simple factory associate a given
 * cell class to a cell view. This is a javabean, just parameter it correctly in
 * order it meets your requirements (else subclass it or subclass
 * DefaultCellViewFactory). You can also recover the gpConfiguration of that
 * javabean via an XML file via XMLEncoder/XMLDecoder.
 * 
 * @author rvalyi, license of this file: LGPL as stated by the Free Software
 *         Foundation
 */
public class GPCellViewFactory extends DefaultCellViewFactory {
	
	public static final String VIEW_CLASS_KEY = "viewClassKey";
	
	public static final void setViewClass(Map map, String viewClass) {
		map.put(VIEW_CLASS_KEY, viewClass);
	}

	protected VertexView createVertexView(Object v) {
		try {
			DefaultGraphCell cell = (DefaultGraphCell) v;
			String viewClass = (String) cell.getAttributes().get(VIEW_CLASS_KEY);

			VertexView view = (VertexView) Thread.currentThread()
					.getContextClassLoader().loadClass(viewClass).newInstance();
			view.setCell(v);
			return view;
		} catch (Exception ex) {
		}
		return super.createVertexView(v);
	}
}
