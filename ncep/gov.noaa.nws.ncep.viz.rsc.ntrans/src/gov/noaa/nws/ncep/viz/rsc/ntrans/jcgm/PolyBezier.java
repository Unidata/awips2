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
import java.awt.geom.CubicCurve2D;
import java.awt.geom.Point2D;
import java.io.*;


/**
 * Class=4, Element=26
 * Support for cubic Bézier curves
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class PolyBezier extends Command {
    /**
     * 1: discontinuous, 2: continuous, >2: reserved for registered values
     */
	private int continuityIndicator;

	/**
	 * The scales curves to draw
	 */
	private CubicCurve2D.Double[] curves;

	private Point2D.Double[] p1;
	private Point2D.Double[] p2;
	private Point2D.Double[] p3;
	private Point2D.Double[] p4;

    public PolyBezier(int ec, int eid, int l, DataInput in)
            throws IOException {

        super(ec, eid, l, in);
        
        this.continuityIndicator = makeIndex();
        
        if (this.continuityIndicator == 1) {
        	assert ((this.args.length - this.currentArg) / sizeOfPoint()) % 4 == 0;
        	
        	int n = ((this.args.length - this.currentArg) / sizeOfPoint()) / 4;
        	
        	this.p1 = new Point2D.Double[n];
        	this.p2 = new Point2D.Double[n];
        	this.p3 = new Point2D.Double[n];
        	this.p4 = new Point2D.Double[n];

        	int point = 0;
        	while (point < n) {
        		this.p1[point] = makePoint();
        		this.p2[point] = makePoint();
        		this.p3[point] = makePoint();
        		this.p4[point] = makePoint();
        		point++;
        	}
        }
        else if (this.continuityIndicator == 2) {
        	assert ((this.args.length - this.currentArg - 1) / sizeOfPoint()) % 3 == 0;
        	int n = ((this.args.length - this.currentArg - 1) / sizeOfPoint()) / 3;
        	
        	this.p1 = new Point2D.Double[n];
        	this.p2 = new Point2D.Double[n];
        	this.p3 = new Point2D.Double[n];
        	this.p4 = new Point2D.Double[n];
        	
        	int point = 0;
        	while (point < n) {
        		if (point == 0) {
        			this.p1[point] = makePoint();
        		}
        		else {
        			this.p1[point] = this.p4[point-1];
        		}
        		this.p2[point] = makePoint();
        		this.p3[point] = makePoint();
        		this.p4[point] = makePoint();
        		point++;
        	}
        }
        else
        	unsupported("unsupported continuity indicator "+this.continuityIndicator);
    }

    @Override
	public String toString() {
        String s = "PolyBezier";
        if (this.continuityIndicator == 1)
        	s = s + " discontinuous";
        else if (this.continuityIndicator == 2)
        	s = s + " continuous";
        else if (this.continuityIndicator > 2)
        	s = s + "reserved for registered values";
        	
        s = s + "[";
        for (int i = 0; i < this.p1.length; i++) {
            s = s + "(" + this.p1[i].x + "," + this.p1[i].y + ") ";
            s = s + "(" + this.p2[i].x + "," + this.p2[i].y + ") ";
            s = s + "(" + this.p3[i].x + "," + this.p3[i].y + ") ";
            s = s + "(" + this.p4[i].x + "," + this.p4[i].y + ") ";
        }
        s = s + "]";
        return s;
    }

	public void initCurves() {
    	this.curves = new CubicCurve2D.Double[this.p1.length];
    	
    	for (int i = 0; i < this.p1.length; i++) {
    		CubicCurve2D.Double c = new CubicCurve2D.Double();
    		c.setCurve(this.p1[i].x, this.p1[i].y,
    			this.p2[i].x, this.p2[i].y, this.p3[i].x, this.p3[i].y, this.p4[i].x, this.p4[i].y);
    		this.curves[i] = c;
    	}
    }

    @Override
	public void paint(CGMDisplay d) {
    	if (this.curves == null)
    		initCurves();
    	
    	Graphics2D g2d = d.getGraphics2D();
		g2d.setStroke(d.getLineStroke());
    	g2d.setColor(d.getLineColor());
    	
    	for (int i = 0; i < this.curves.length; i++) {
    		g2d.draw(this.curves[i]);
    	}
    }
}

/*
 * vim:encoding=utf8
 */
