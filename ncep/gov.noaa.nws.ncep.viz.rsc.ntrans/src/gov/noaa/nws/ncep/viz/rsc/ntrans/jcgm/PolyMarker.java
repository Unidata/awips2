/*
 * <copyright> Copyright 1997-2003 BBNT Solutions, LLC under sponsorship of the
 * Defense Advanced Research Projects Agency (DARPA).
 * Copyright 2009 Swiss AviationSoftware Ltd.
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the Cougaar Open Source License as published by DARPA on
 * the Cougaar Open Source Website (www.cougaar.org).
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm;

import java.awt.BasicStroke;
import java.awt.Graphics2D;
import java.awt.Stroke;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.io.DataInput;
import java.io.IOException;

import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.MarkerType.Type;


/**
 * Class=4, Element=3
 * @author xphc (Philippe Cadé)
 * @version $Id$
 * @since May 13, 2009
 */
public class PolyMarker extends Command {
	private Point2D.Double[] points;

	public PolyMarker(int ec, int eid, int l, DataInput in) throws IOException {
		super(ec, eid, l, in);
		
        int n = this.args.length / sizeOfPoint();
        this.points = new Point2D.Double[n];
        for (int i = 0; i < n; i++) {
        	this.points[i] = makePoint();
        }
        
        // make sure all the arguments were read
        assert (this.currentArg == this.args.length);
	}

	@Override
	public String toString() {
    	StringBuilder sb = new StringBuilder();
    	sb.append("PolyMarker [");
        for (int i = 0; i < this.points.length; i++) {
        	sb.append("(");
        	sb.append(this.points[i].x).append(",");
        	sb.append(this.points[i].y);
        	sb.append(")");
        }
        sb.append("]");
        return sb.toString();
    }

	private void drawAsterisk(Graphics2D g2d, Point2D.Double origin, double size) {
		double halfSize = size/2;
		double fourthSize = size/4;
		Line2D.Double line = new Line2D.Double(origin.x, origin.y - halfSize, origin.x, origin.y + halfSize);
		g2d.draw(line);
		
		line = new Line2D.Double(origin.x - halfSize, origin.y - fourthSize, origin.x + halfSize, origin.y + fourthSize);
		g2d.draw(line);

		line = new Line2D.Double(origin.x - halfSize, origin.y + fourthSize, origin.x + halfSize, origin.y - fourthSize);
		g2d.draw(line);
	}

	private void drawCircle(Graphics2D g2d, Point2D.Double origin, double size, boolean fill) {
		double halfSize = size/2;
		Ellipse2D.Double circle = new Ellipse2D.Double(origin.x - halfSize, origin.y - halfSize, size, size);
		
		if (fill) {
			g2d.fill(circle);
		}
		else {
			g2d.draw(circle);
		}
	}

	private void drawCross(Graphics2D g2d, Point2D.Double origin, double size) {
		double halfSize = size/2;
		Line2D.Double line = new Line2D.Double(origin.x - halfSize, origin.y - halfSize, origin.x + halfSize, origin.y + halfSize);
		g2d.draw(line);
		
		line = new Line2D.Double(origin.x - halfSize, origin.y + halfSize, origin.x + halfSize, origin.y - halfSize);
		g2d.draw(line);
	}

	private void drawPlus(Graphics2D g2d, Point2D.Double origin, double size) {
		double halfSize = size/2;
		Line2D.Double line = new Line2D.Double(origin.x, origin.y - halfSize, origin.x, origin.y + halfSize);
		g2d.draw(line);
		
		line = new Line2D.Double(origin.x - halfSize, origin.y, origin.x + halfSize, origin.y);
		g2d.draw(line);
	}

	@Override
	public void paint(CGMDisplay d) {
        Graphics2D g2d = d.getGraphics2D();
		g2d.setColor(d.getMarkerColor());
		
		Type markerType = d.getMarkerType();
		double size = d.getMarkerSize();
		
		// save the current stroke and use a default one for markers
		Stroke savedStroke = g2d.getStroke();
		// TODO: what is the best stroke to use for markers?
		g2d.setStroke(new BasicStroke());
		
		if (MarkerType.Type.ASTERISK.equals(markerType)) {
			for (Point2D.Double point : this.points) {
				drawAsterisk(g2d, point, size);
			}
		}
		else if (MarkerType.Type.CIRCLE.equals(markerType)) {
			for (Point2D.Double point : this.points) {
				drawCircle(g2d, point, size, false);
			}
		}
		else if (MarkerType.Type.CROSS.equals(markerType)) {
			for (Point2D.Double point : this.points) {
				drawCross(g2d, point, size);
			}
		}
		else if (MarkerType.Type.DOT.equals(markerType)) {
			for (Point2D.Double point : this.points) {
				drawCircle(g2d, point, size, true);
			}
		}
		else if (MarkerType.Type.PLUS.equals(markerType)) {
			for (Point2D.Double point : this.points) {
				drawPlus(g2d, point, size);
			}
		}
		
		g2d.setStroke(savedStroke);
    }
}

/*
 * vim:encoding=utf8
 */
