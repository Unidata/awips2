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

import java.awt.Graphics2D;
import java.awt.geom.Arc2D;
import java.awt.geom.Point2D;
import java.io.*;



/**
 * Class=4, Element=13
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class CircularArc3Point extends Command {
    private Point2D.Double center;
	protected Arc2D.Double shape;
	private double radius;
	private double startDeltaX;
	private double startDeltaY;
	private double intermediateDeltaX;
	private double intermediateDeltaY;
	private double endDeltaX;
	private double endDeltaY;
	private Point2D.Double p1;
	private Point2D.Double p2;
	private Point2D.Double p3;

	public CircularArc3Point(int ec, int eid, int l, DataInput in)
            throws IOException {
        super(ec, eid, l, in);
        
        this.p1 = makePoint();
        this.p2 = makePoint();
        this.p3 = makePoint();
        
        this.center = findCenter(this.p1.x, this.p1.y, this.p2.x, this.p2.y, this.p3.x, this.p3.y);
        this.radius = distance(this.p1, this.center);
        
        this.startDeltaX = this.p1.x - this.center.x;
        this.startDeltaY = this.p1.y - this.center.y;
        
        this.intermediateDeltaX = this.p2.x - this.center.x;
        this.intermediateDeltaY = this.p2.y - this.center.y;
        
        this.endDeltaX = this.p3.x - this.center.x;
        this.endDeltaY = this.p3.y - this.center.y;
    }

    private double distance(Point2D.Double p1, Point2D.Double p2) {
    	return Math.sqrt((p2.x-p1.x)*(p2.x-p1.x)+(p2.y-p1.y)*(p2.y-p1.y));
	}

	@Override
	public void paint(CGMDisplay d) {
		if (this.shape == null)
			initShape(d);
		
    	Graphics2D g2d = d.getGraphics2D();
    	g2d.setColor(d.getLineColor());
		g2d.setStroke(d.getLineStroke());
    	g2d.draw(this.shape);
	}

	protected void initShape(CGMDisplay d) {
		double startAngle = d.angle(this.startDeltaX, this.startDeltaY);
		double intermediateAngle = d.angle(this.intermediateDeltaX, this.intermediateDeltaY);
		double endAngle = d.angle(this.endDeltaX, this.endDeltaY);
		
		this.shape = new Arc2D.Double(getClosureType());
		this.shape.setFrame(this.center.x - this.radius, this.center.y - this.radius, 2*this.radius, 2*this.radius);
		
		if (endAngle > startAngle) {
			if (intermediateAngle < startAngle ||  endAngle < intermediateAngle) {
				// intermediate angle outside of start/end
				this.shape.setAngles(this.p1, this.p3);
			}
			else {
				this.shape.setAngles(this.p3, this.p1);
			}
		}
		else {
			if (intermediateAngle < endAngle || startAngle < intermediateAngle) {
				// intermediate angle outside of start/end
				this.shape.setAngles(this.p3, this.p1);
			}
			else {
				this.shape.setAngles(this.p1, this.p3);
			}
		}
	}
	
    /**
     * Finds the center of the circle going through P1(x1, y1), P2(x2, y2) and P3(x3, y3).
     * @param x1
     * @param y1
     * @param x2
     * @param y2
     * @param x3
     * @param y3
     * @return The coordinates of the circle's center
     */
    private Point2D.Double findCenter(double x1, double y1, double x2, double y2, double x3, double y3) {
    	Point2D.Double ret = new Point2D.Double();
    	ret.x = ((((x1*x1)*(y3 - y2)) + (y2*((x3*x3) + (y1*(y2 - y1)) + (y3*(y3 - y2)))) + (y1*((y3*(y1 - y3)) + (x2*x2) - (x3*x3))) - (y3*(x2*x2)))/(2.0*(((x1 - x3)*(y3 - y2)) + ((x2 - x3)*(y1 - y3)))));
    	ret.y = (((x3*((y1*y1) - (x2*x2) - (y2*y2))) + (x2*((x3*x3) + (y3*y3) - (y1*y1))) + ((x1*x1)*(x3 - x2)) + (x1*((x2*x2) - (x3*x3) + (y2*y2) - (y3*y3))))/(2.0*((x2*(y3 - y1)) + (x1*(y2 - y3)) + (x3*(y1 - y2)))));
    	return ret;
	}

	protected int getClosureType() {
		return Arc2D.OPEN;
	}
	
    @Override
	public String toString() {
    	StringBuilder sb = new StringBuilder();
    	sb.append("CircularArc3Point ");
    	sb.append("p1=").append(this.p1);
    	sb.append("p2=").append(this.p2);
    	sb.append("p3=").append(this.p3);
    	
        return sb.toString();
    }
}

/*
 * vim:encoding=utf8
 */
