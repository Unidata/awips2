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
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.io.*;


/**
 * Class=4, Element=11
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class RectangleElement extends Command {
	private Rectangle2D.Double shape;
	private Point2D.Double firstCorner;
	private Point2D.Double secondCorner;

    public RectangleElement(int ec, int eid, int l, DataInput in)
            throws IOException {
        super(ec, eid, l, in);
        
        this.firstCorner = makePoint();
        this.secondCorner = makePoint();
        
        double x1 = this.firstCorner.x;
        double y1 = this.firstCorner.y;
        double x2 = this.secondCorner.x;
        double y2 = this.secondCorner.y;
        
        if (x1 > x2) {
        	double temp = x1;
        	x1 = x2;
        	x2 = temp;
        }
        
        if (y1 > y2) {
        	double temp = y1;
        	y1 = y2;
        	y2 = temp;
        }
        
        double w = x2 - x1;
        double h = y2 - y1;
        
        this.shape = new Rectangle2D.Double(x1, y1, w, h);
        
        // make sure all the arguments were read
        assert (this.currentArg == this.args.length);
    }

    @Override
	public String toString() {
        return "Rectangle [" + this.firstCorner.x + "," + this.firstCorner.y + "] [" + this.secondCorner.x + "," + this.secondCorner.y + "]";
    }

    @Override
	public void paint(CGMDisplay d) {
    	d.fill(this.shape);
    	
    	Graphics2D g2d = d.getGraphics2D();

    	if (d.drawEdge()) {
    		g2d.setColor(d.getEdgeColor());
    		g2d.setStroke(d.getEdgeStroke());
    		g2d.draw(this.shape);
    	}
    }
}

/*
 * vim:encoding=utf8
 */
