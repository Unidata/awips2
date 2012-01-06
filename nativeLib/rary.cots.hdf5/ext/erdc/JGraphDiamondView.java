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

import java.awt.BasicStroke;
import java.awt.Dimension;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Polygon;

import org.jgraph.graph.CellViewRenderer;
import org.jgraph.graph.VertexRenderer;
import org.jgraph.graph.VertexView;

public class JGraphDiamondView extends VertexView {
	public static transient ActivityRenderer renderer = new ActivityRenderer();

	public JGraphDiamondView() {
		super();
	}

	public JGraphDiamondView(Object cell) {
		super(cell);
	}

	@Override
	public CellViewRenderer getRenderer() {
		return renderer;
	}

	public static class ActivityRenderer extends VertexRenderer 
	{
		/**
		 * Return a slightly larger preferred size than for a rectangle.
		 */
		@Override
		public Dimension getPreferredSize() {
			Dimension d = super.getPreferredSize();
			d.width += d.height / 5;
			return d;
		}

		@Override
		public void paint(Graphics g)
		{
			// TODO this doesn't draw the border
			int b = borderWidth;
			Graphics2D g2 = (Graphics2D) g;
			Dimension d = getSize();
			boolean tmp = selected;
			// construct the diamond
			int width = d.width - b; // allow for border
			int height = d.height - b; // allow for border
			int halfWidth = (d.width - b) / 2;
			int halfHeight = (d.height - b) / 2;
			int[] xpoints =
			{ halfWidth, width, halfWidth, 0 };
			int[] ypoints =
			{ 0, halfHeight, height, halfHeight };
			Polygon diamond = new Polygon(xpoints, ypoints, 4);
			if (super.isOpaque())
			{
				g.setColor(super.getBackground());
				if (gradientColor != null && !preview)
				{
					setOpaque(false);
					g2.setPaint(new GradientPaint(0, 0, getBackground(),
							getWidth(), getHeight(), gradientColor, true));
				}
				g.fillPolygon(diamond);
			}
			try
			{
				setBorder(null);
				setOpaque(false);
				selected = false;
				super.paint(g);
			} finally
			{
				selected = tmp;
			}
			if (bordercolor != null)
			{
				g.setColor(bordercolor);
				g2.setStroke(new BasicStroke(b));
				g.drawPolygon(diamond);
			}
		}
	}
}
