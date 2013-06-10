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
import java.awt.geom.Path2D;
import java.awt.geom.Point2D;
import java.io.DataInput;
import java.io.IOException;


/**
 * Class=4, Element=8
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class PolygonSet extends Command {
	final private static int INVISIBLE = 0;
	final private static int VISIBLE = 1;
	final private static int CLOSE_INVISIBLE = 2;
	final private static int CLOSE_VISIBLE = 3;
	
	/** Polygon used to draw the edges */
    private Path2D.Double edgePolygon;
    /** Polygon used to draw the filling */
    private Path2D.Double fillPolygon;

    public PolygonSet(int ec, int eid, int l, DataInput in)
            throws IOException {

        super(ec, eid, l, in);
        
        assert (this.args.length % (sizeOfPoint() + sizeOfEnum()) == 0);
        int n = this.args.length / (sizeOfPoint() + sizeOfEnum());

        Point2D.Double p = makePoint();
        this.edgePolygon = new Path2D.Double();
        this.fillPolygon = new Path2D.Double(Path2D.WIND_EVEN_ODD);
        
        Point2D.Double currentClosurePoint = p;
        this.edgePolygon.moveTo(p.x, p.y);
        this.fillPolygon.moveTo(p.x, p.y);
        int edgeOutFlag = makeEnum();
        
        for (int i = 1; i < n; i++) {
        	p = makePoint();
        	
        	if (edgeOutFlag == INVISIBLE) {
        		this.edgePolygon.moveTo(p.x, p.y);
        		this.fillPolygon.lineTo(p.x, p.y);
        	}
        	else if (edgeOutFlag == VISIBLE) {
        		this.edgePolygon.lineTo(p.x, p.y);
        		this.fillPolygon.lineTo(p.x, p.y);
            }
        	else if (edgeOutFlag == CLOSE_INVISIBLE) {
        		this.fillPolygon.lineTo(currentClosurePoint.x, currentClosurePoint.y);
        		currentClosurePoint = p;
        		this.edgePolygon.moveTo(p.x, p.y);
        		
        		this.fillPolygon.moveTo(p.x, p.y);
            }
        	else if (edgeOutFlag == CLOSE_VISIBLE) {
        		this.fillPolygon.lineTo(currentClosurePoint.x, currentClosurePoint.y);
        		this.edgePolygon.lineTo(currentClosurePoint.x, currentClosurePoint.y);
        		currentClosurePoint = p;
        		this.edgePolygon.moveTo(p.x, p.y);
        		
        		this.fillPolygon.moveTo(p.x, p.y);
            }
        	
        	edgeOutFlag = makeEnum();
        }
        
        if (edgeOutFlag == VISIBLE || edgeOutFlag == CLOSE_VISIBLE) {
        	this.edgePolygon.lineTo(currentClosurePoint.x, currentClosurePoint.y);
        	this.fillPolygon.closePath();
        }
        
        // make sure all the arguments were read
        assert (this.currentArg == this.args.length);
    }

    @Override
	public String toString() {
        return "PolygonSet";
    }

    @Override
	public void paint(CGMDisplay d) {
    	Graphics2D g2d = d.getGraphics2D();
    	
   		d.fill(this.fillPolygon);

    	if (d.drawEdge()) {
    		g2d.setColor(d.getEdgeColor());
    		g2d.setStroke(d.getEdgeStroke());
    		g2d.draw(this.edgePolygon);
    	}
    }
}

/*
 * vim:encoding=utf8
 */
